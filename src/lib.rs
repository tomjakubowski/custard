extern crate arena;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;

use rustc_driver::driver;
use rustc::session::{mod, config};
use rustc::middle::def;
use rustc::middle::ty;

use syntax::{ast, ast_map, codemap, diagnostic, visit};
use syntax::ast::NodeId;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use arena::TypedArena;

pub use rustc::session::config::Input;

mod cdecl;

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
        funcs: HashSet::new(),
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

trait Clean<T> {
    fn clean(&self, &ty::ctxt) -> T;
}

// "cleaned" rust types

#[deriving(Clone, Copy, Show, Hash, PartialEq, Eq)]
pub enum Primitive {
    Unit,
    I8,
    I16,
    I32,
    I64
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub enum TypeKind {
    Primitive(Primitive),
    ResolvedPath {
        path: ast::Path,
        did: ast::DefId
    }
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct Type {
    kind: TypeKind,
    node: NodeId
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct Arg {
    name: String,
    ty: Type,
    node: NodeId
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct FnDecl {
    name: String,
    inputs: Vec<Arg>,
    output: Type,
    span: codemap::Span
}

impl Clean<FnDecl> for ast::Item {
    fn clean(&self, tcx: &ty::ctxt) -> FnDecl {
        use syntax::ast::ItemFn;

        // FIXME: mangled names... export_name...
        match self.node {
            ItemFn(ref decl, _, _, _, _) => FnDecl {
                name: self.ident.as_str().into_string(),
                inputs: decl.inputs.clean(tcx),
                output: decl.output.clean(tcx),
                span: self.span
            },
            _ => unreachable!()
        }
    }
}

impl<T: Clean<C>, C> Clean<Vec<C>> for Vec<T> {
    fn clean(&self, tcx: &ty::ctxt) -> Vec<C> {
        self.iter().map(|x| x.clean(tcx)).collect()
    }
}

impl Clean<String> for ast::Pat {
    fn clean(&self, _: &ty::ctxt) -> String {
        match self.node {
            ast::PatIdent(_, ref ident, _) => {
                ident.node.as_str().into_string()
            }
            _ => panic!("unimplemented: {}", self)
        }
    }
}

impl Clean<Arg> for ast::Arg {
    fn clean(&self, tcx: &ty::ctxt) -> Arg {
        Arg {
            name: self.pat.clean(tcx),
            ty: self.ty.clean(tcx),
            node: self.id
        }
    }
}

impl Clean<Type> for ast::Ty {
    fn clean(&self, tcx: &ty::ctxt) -> Type {
        // FIXME: is_ffi_safe
        let (path, def) = match self.node {
            ast::TyPath(ref path, id) => {
                match tcx.def_map.borrow().get(&id) {
                    Some(&def) => (path.clone(), def),
                    None => panic!("node id {} missing in def_map", id)
                }
            }
            _ => unimplemented!()
        };

        let kind = match def {
            def::DefPrimTy(p) => match p {
                ast::TyInt(ast::TyI8)  => TypeKind::Primitive(Primitive::I8),
                ast::TyInt(ast::TyI16) => TypeKind::Primitive(Primitive::I16),
                ast::TyInt(ast::TyI32) => TypeKind::Primitive(Primitive::I32),
                ast::TyInt(ast::TyI64) => TypeKind::Primitive(Primitive::I64),
                _ => unimplemented!()
            },
            def::DefTy(def_id, false) => TypeKind::ResolvedPath {
                path: path,
                did: def_id
            },
            _ => panic!("not yet implemented: {}", def)
        };

        Type {
            kind: kind,
            node: self.id
        }
    }
}

impl<'a> Clean<Type> for ast::FunctionRetTy {
    fn clean(&self, tcx: &ty::ctxt) -> Type {
        use syntax::ast::FunctionRetTy::{NoReturn, Return};
        match *self {
            Return(ref ty) => ty.clean(tcx),
            NoReturn(_) => Type {
                kind: TypeKind::Primitive(Primitive::Unit),
                node: ast::DUMMY_NODE_ID
            }
        }
    }
}

/// Collects all public C ABI fn definitions from a given crate.
pub struct CFnDeclVisitor<'tcx> {
    funcs: HashSet<FnDecl>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'a> visit::Visitor<'a> for CFnDeclVisitor<'a> {
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::abi::Abi;
        use syntax::ast::Item_::ItemFn;

        if item.vis != ast::Visibility::Public { return }
        if let ItemFn(_, _, Abi::C, _, _) = item.node {
            self.funcs.insert(item.clean(self.tcx));
        }
    }
}
