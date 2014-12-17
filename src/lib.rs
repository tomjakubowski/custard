extern crate arena;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;

use rustc_driver::driver;
use rustc::session::{mod, config};
use rustc::middle::ty;

use syntax::{ast, ast_map, codemap, diagnostic, visit};

use std::cell::RefCell;
use std::collections::HashMap;
use arena::TypedArena;

pub use rustc::session::config::Input;

use clean::{Clean, FnDecl};

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
        // tys: HashSet::new(),
        tcx: ty_cx
    };

    visit::walk_crate(&mut visitor, ty_cx.map.krate());

    // write needed includes (FIXME: this should actually be computed)
    println!("#include <stdint.h>");
    println!("");

    // write fn prototypes
    for f in visitor.funcs.iter() {
        use cdecl::Cdecl;
        println!("{}", f.cdecl());
    }
}

/// Collects all public C ABI fn definitions from a given crate.
pub struct CFnDeclVisitor<'tcx> {
    funcs: Vec<FnDecl>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'a> visit::Visitor<'a> for CFnDeclVisitor<'a> {
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::abi::Abi;
        use syntax::ast::Item_::ItemFn;

        if item.vis != ast::Visibility::Public { return }
        if let ItemFn(_, _, Abi::C, _, _) = item.node {
            self.funcs.push(item.clean(self.tcx));
        }
    }
}
