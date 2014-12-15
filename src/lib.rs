extern crate arena;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;

use rustc_driver::driver;
use rustc::session::{mod, config};
use rustc::middle::def;
use rustc::middle::ty;

use syntax::{ast, ast_map, codemap, diagnostic, visit};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
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
        ref ty_cx, ..
    } = driver::phase_3_run_analysis_passes(sess, ast_map, &type_arena, name);

    // let ctxt = CDeclContext {
    //     krate: ty_cx.map.krate(),
    //     ty_cx: ty_cx
    // };

    let mut visitor = CDeclVisitor {
        funcs: HashSet::new(),
        tys: HashSet::new(),
        tcx: ty_cx
    };

    visit::walk_crate(&mut visitor, ty_cx.map.krate());

    println!("ok:\n{}\n{}", visitor.funcs, visitor.tys);
}

// pub struct CDeclContext<'tcx> {
//     krate: &'tcx ast::Crate,
//     tcx: ty::ctxt<'tcx>
// }

// rust types

#[deriving(Clone, Copy, Show, Hash, PartialEq, Eq)]
pub enum Primitive {
    I8,
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
    ResolvedPath {
        path: ast::Path,
        did: ast::DefId
    }
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct FnDecl {
    name: String,
    inputs: Vec<Type> // FIXME needs names
}

/// Collects all C ABI fn definitions and the types referenced in their
/// signatures from a given crate.
pub struct CDeclVisitor<'tcx> {
    funcs: HashSet<FnDecl>,
    tys: HashSet<Type>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'a> visit::Visitor<'a> for CDeclVisitor<'a> {
    fn visit_fn(&mut self, fn_kind: visit::FnKind, fn_decl: &'a ast::FnDecl,
                _: &'a ast::Block, _: codemap::Span, _: ast::NodeId) {
        use syntax::abi::Abi;
        use syntax::ast::Arg;
        use syntax::visit::FnKind::FkItemFn;

        if let FkItemFn(id, _, _, Abi::C) = fn_kind {
            let inputs = fn_decl.inputs.iter().map(|&Arg { ref ty, .. }| {
                let ty = resolve_type(self.tcx, &**ty);
                self.tys.insert(ty.clone());
                ty
            }).collect::<Vec<_>>();

            let fun = FnDecl {
                name: id.as_str().into_string(),
                inputs: inputs
            };

            println!("Adding fn {}", id.as_str());
            self.funcs.insert(fun);
        }
    }
}

fn resolve_type<'cx>(tcx: &'cx ty::ctxt<'cx>, ty: &ast::Ty) -> Type {
    let (path, def) = match ty.node {
        ast::TyPath(ref path, id) => {
            match tcx.def_map.borrow().get(&id) {
                Some(&def) => (path.clone(), def),
                None => panic!("node id {} missing in def_map", id)
            }
        }
        _ => unimplemented!()
    };

    match def {
        def::DefPrimTy(p) => match p {
            ast::TyInt(ast::TyI8) => Type::Primitive(Primitive::I8),
            _ => unimplemented!()
        },
        def::DefTy(def_id, false) => Type::ResolvedPath {
            path: path,
            did: def_id
        },
        _ => panic!("not yet implemented: {}", def)
    }
}
