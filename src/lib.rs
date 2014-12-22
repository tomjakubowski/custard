#![feature(phase)]

extern crate arena;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;

#[phase(plugin, link)] extern crate log;

use rustc_driver::driver;
use rustc::session::{mod, config};
use rustc::middle::ty;

use syntax::{ast, ast_map, ast_util, codemap, diagnostic, visit};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use arena::TypedArena;

pub use rustc::session::config::Input;

use clean::{Arg, Clean, FnDecl, Return, Struct};

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
        // FIXME: use the improper_ctypes lint once #19834 is fixed
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

    let mut fn_visitor = CFnDeclVisitor::new(ty_cx);
    visit::walk_crate(&mut fn_visitor, ty_cx.map.krate());

    debug!("type node ids: {}", fn_visitor.types);

    debug!("ast_ty_to... {}",
           ty_cx.ast_ty_to_ty_cache.borrow().keys().collect::<Vec<_>>());

    let mut ty_visitor = CTypeVisitor::new(ty_cx, fn_visitor.types);
    visit::walk_crate(&mut ty_visitor, ty_cx.map.krate());

    debug!("ty_visitor structs: {}", ty_visitor.structs);

    // necessary includes (FIXME: compute this)
    println!("#include <stdint.h>");
    println!("");

    // write forward decls of types (FIXME: compute this [should be straightforward])
    for t in ty_visitor.structs.iter() {
        use cdecl::CtypeSpec;
        println!("{};", t.ctype_spec());
    }

    // write actual decls of types
    for t in ty_visitor.structs.iter() {
        use cdecl::Cdecl;
        println!("{}", t.cdecl());
    }

    // write fn prototypes
    for f in fn_visitor.funcs.iter() {
        use cdecl::Cdecl;
        println!("{}", f.cdecl());
    }
}

struct CFnDeclVisitor<'tcx> {
    funcs: Vec<FnDecl>,
    types: HashSet<ast::NodeId>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'tcx> CFnDeclVisitor<'tcx> {
    fn new<'a>(tcx: &'a ty::ctxt<'a>) -> CFnDeclVisitor<'a> {
        CFnDeclVisitor {
            funcs: vec![],
            types: HashSet::new(),
            tcx: tcx
        }
    }
}

struct CTypeVisitor<'tcx> {
    types: HashSet<ast::NodeId>,
    structs: Vec<Struct>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'tcx> CTypeVisitor<'tcx> {
    fn new<'a>(tcx: &'a ty::ctxt<'a>, types: HashSet<ast::NodeId>) -> CTypeVisitor<'a> {
        CTypeVisitor {
            types: types,
            structs: vec![],
            tcx: tcx
        }
    }
}

impl<'a> visit::Visitor<'a> for CFnDeclVisitor<'a> {
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::abi::Abi;
        use syntax::ast::Item_::ItemFn;
        use clean::Type::ResolvedPath;

        // FIXME: once rust-lang/rust#19834, can replace this with just
        // denying the improper_ctypes lint
        fn check_ty(tcx: &ty::ctxt, node: ast::NodeId) -> bool {
            let tty = match tcx.ast_ty_to_ty_cache.borrow()[node] {
                ty::atttce_resolved(tty) => tty,
                _ => panic!("ty missing from attt cache")
            };
            ty::is_ffi_safe(tcx, tty)
        }

        if item.vis != ast::Visibility::Public { return }

        if let ItemFn(ref fn_decl, _, Abi::C, _, _) = item.node {
            debug!("ItemFn {}", item.ident.as_str());

            for &ast::Arg { ref ty, .. } in fn_decl.inputs.iter() {
                if !check_ty(self.tcx, ty.id) {
                    self.tcx.sess.span_fatal(ty.span, "Type is not FFI safe.");
                }
            }
            if let ast::FunctionRetTy::Return(ref ty) = fn_decl.output {
                if !check_ty(self.tcx, ty.id) {
                    self.tcx.sess.span_fatal(ty.span, "Type is not FFI safe.");
                }
            }

            let func: FnDecl = item.clean(self.tcx);
            for &Arg { ref ty, .. } in func.inputs.iter() {
                if let &ResolvedPath { did, .. } = ty {
                    if ast_util::is_local(did) {
                        self.types.insert(did.node);
                    }
                }
            }
            if let Return {
                ty: ResolvedPath { did, .. }, ..
            } = func.output {
                if ast_util::is_local(did) {
                    self.types.insert(did.node);
                }
            }

            self.funcs.push(func);
        }
    }
}

impl<'a> visit::Visitor<'a> for CTypeVisitor<'a> {
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::ast::Item_::ItemStruct;
        if !self.types.contains(&item.id)  {
            return
        }

        match item.node {
            ItemStruct(_, _) => {
                let s = item.clean(self.tcx);
                self.structs.push(s)
            },
            _ => {}
        }
    }
}
