use rustc::middle::{def, ty};

use syntax::{ast, codemap};
use syntax::ast::NodeId;

pub trait Clean<T> {
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
pub enum Type {
    Primitive(Primitive),
    ResolvedPath {
        path: ast::Path,
        did: ast::DefId
    }
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct Arg {
    pub name: String,
    pub ty: Type,
    pub node: NodeId
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct FnDecl {
    pub name: String,
    pub inputs: Vec<Arg>,
    pub output: Type,
    pub span: codemap::Span
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

        match def {
            def::DefPrimTy(p) => match p {
                ast::TyInt(ast::TyI8)  => Type::Primitive(Primitive::I8),
                ast::TyInt(ast::TyI16) => Type::Primitive(Primitive::I16),
                ast::TyInt(ast::TyI32) => Type::Primitive(Primitive::I32),
                ast::TyInt(ast::TyI64) => Type::Primitive(Primitive::I64),
                _ => unimplemented!()
            },
            def::DefTy(def_id, false) => Type::ResolvedPath {
                path: path,
                did: def_id
            },
            _ => panic!("not yet implemented: {}", def)
        }
    }
}

impl<'a> Clean<Type> for ast::FunctionRetTy {
    fn clean(&self, tcx: &ty::ctxt) -> Type {
        use syntax::ast::FunctionRetTy::{NoReturn, Return};
        match *self {
            Return(ref ty) => ty.clean(tcx),
            NoReturn(_) => Type::Primitive(Primitive::Unit)
        }
    }
}
