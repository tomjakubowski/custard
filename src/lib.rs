#![feature(phase)]

extern crate arena;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;

#[phase(plugin, link)] extern crate log;

use rustc_driver::driver;
use rustc::session::{mod, config};
use rustc::middle::ty;

use syntax::{ast, ast_map, codemap, diagnostic, visit};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use arena::TypedArena;

pub use rustc::session::config::Input;

use clean::{Arg, Clean, FnDecl, Type};

mod cdecl;
mod clean;

pub type Externs = HashMap<String, Vec<String>>;

pub fn run_core(libs: Vec<Path>, cfgs: Vec<String>, externs: Externs,
                input: Input, triple: Option<String>) {
    // FIXME: cut the rustdoc stuff we don't need

    // Parse, resolve, and typecheck the given crate.

    let name = driver::anon_src();

    let sessopts = config::Options {
        maybe_sysroot: None,
        addl_lib_search_paths: RefCell::new(libs),
        crate_types: vec![config::CrateTypeRlib],
        externs: externs,
        target_triple: triple.unwrap_or(config::host_triple().to_string()),
        cfg: config::parse_cfgspecs(cfgs),
        ..config::basic_options().clone()
    };


    let codemap = codemap::CodeMap::new();
    let diagnostic_handler = diagnostic::default_handler(diagnostic::Auto, None);
    let span_diagnostic_handler =
        diagnostic::mk_span_handler(diagnostic_handler, codemap);

    let sess = session::build_session_(sessopts,
                                       None, // FIXME
                                       span_diagnostic_handler);

    let cfg = config::build_configuration(&sess);

    let krate = driver::phase_1_parse_input(&sess, cfg, &input);

    let krate = driver::phase_2_configure_and_expand(&sess, krate, name.as_slice(), None)
                    .expect("phase_2_configure_and_expand aborted in rustdoc!");

    let mut forest = ast_map::Forest::new(krate);
    let ast_map = driver::assign_node_ids_and_map(&sess, &mut forest);

    let type_arena = TypedArena::new();
    let ty::CrateAnalysis {
        ref ty_cx, ..
    } = driver::phase_3_run_analysis_passes(sess, ast_map, &type_arena, name);

    let mut visitor = CFnDeclVisitor {
        funcs: Vec::new(),
        defs: HashSet::new(),
        tcx: ty_cx
    };

    visit::walk_crate(&mut visitor, ty_cx.map.krate());

    // TODO: Do this in two passes:
    // 1) Walk over fns, collecting + cleaning pub extern ones, and the def or node IDs
    //    of types referenced in the inputs/output of those fns. Check for no_mangle and
    //    export_name.  Args should be Type::ResolvedPath.
    // 2) Walk over struct/enum/... items, collecting + cleaning those which correspond
    //    to the def or node IDs. Check is_ffi_safe and whatever else.  Cleaned output
    //    be an enum Item {
    //        Struct { name: String, fields: Vec<Item>, .. },
    //        Field { name: String, ty: Type, .. }, },
    //        Enum { .. /* ??? */ },
    //    }
    // Still to be figured out: type defs, pointers (should just be another Type
    // variant I reckon)

    // write needed includes (FIXME: this should actually be computed)
    println!("#include <stdint.h>");
    println!("");

    // write forward decls of types... (FIXME: lol)
    for t in visitor.defs.iter() {
        use cdecl::CtypeSpec;
        println!("{};", t.ctype_spec());
    }

    // write actual decls of types
    for t in visitor.defs.iter() {
        use cdecl::Cdecl;
        println!("{};", t.cdecl());
    }

    // write fn prototypes
    for f in visitor.funcs.iter() {
        use cdecl::Cdecl;
        println!("{}", f.cdecl());
    }
}

struct CFnDeclVisitor<'tcx> {
    funcs: Vec<FnDecl>,
    defs: HashSet<Type>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'a> visit::Visitor<'a> for CFnDeclVisitor<'a> {
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::abi::Abi;
        use syntax::ast::Item_::ItemFn;

        if item.vis != ast::Visibility::Public { return }
        if let ItemFn(_, _, Abi::C, _, _) = item.node {
            let func = item.clean(self.tcx);
            for &Arg { ref ty, ty_node, .. } in func.inputs.iter() {
                if let &Type::ResolvedPath { did, .. } = ty {
                    debug!("found a resolved path in the args: {}", did);
                    let &tty = match self.tcx.ast_ty_to_ty_cache.borrow()[ty_node] {
                        ty::atttce_resolved(ref ty) => ty,
                        _ => panic!()
                    };
                    assert!(ty::is_ffi_safe(self.tcx, tty));
                    self.defs.insert(ty.clone());
                }
            }
            self.funcs.push(func);
        }
    }
}
