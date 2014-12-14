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
        ty_cx, ..
    } = driver::phase_3_run_analysis_passes(sess, ast_map, &type_arena, name);

    let ctxt = CDeclContext {
        krate: ty_cx.map.krate(),
        _ty_cx: ty_cx
    };

    let mut visitor = CDeclVisitor::new();

    visit::walk_crate(&mut visitor, ctxt.krate);

    println!("ok");
}

pub struct CDeclContext<'tcx> {
    krate: &'tcx ast::Crate,
    _ty_cx: ty::ctxt<'tcx>
}

#[deriving(Default)]
/// Collects all extern "C" fn definitions and the types referenced in
/// their signatures from a given crate.
pub struct CDeclVisitor {
    funcs: Vec<ast::NodeId>,
    _tys: Vec<ast::NodeId>
}

impl CDeclVisitor {
    pub fn new() -> CDeclVisitor {
        CDeclVisitor {
            ..std::default::Default::default()
        }
    }
}

impl<'a> visit::Visitor<'a> for CDeclVisitor {
    fn visit_fn(&mut self, fn_kind: visit::FnKind, fn_decl: &'a ast::FnDecl,
                _: &'a ast::Block, _: codemap::Span, node: ast::NodeId) {
        use syntax::abi::Abi;
        use syntax::visit::FnKind::FkItemFn;

        if let FkItemFn(id, _, _, Abi::C) = fn_kind {
            println!("Adding fn {} ({})", id.as_str(), fn_decl);
            self.funcs.push(node)
        }
    }
}
