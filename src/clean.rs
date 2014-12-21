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
pub enum PointerKind {
    Mutable,
    Const
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
    Pointer(PointerKind, Box<Type>),
    ResolvedPath {
        path: ast::Path,
        did: ast::DefId,
    }
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct Arg {
    pub name: String,
    pub ty: Type,
    pub ty_node: NodeId
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct Return {
    pub ty: Type,
    pub ty_node: Option<NodeId>
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct FnDecl {
    pub name: String,
    pub inputs: Vec<Arg>,
    pub output: Return,
    pub span: codemap::Span
}

impl<T: Clean<C>, C> Clean<Vec<C>> for Vec<T> {
    fn clean(&self, tcx: &ty::ctxt) -> Vec<C> {
        self.iter().map(|x| x.clean(tcx)).collect()
    }
}

impl Clean<String> for ast::Ident {
    fn clean(&self, _: &ty::ctxt) -> String {
        self.name.as_str().into_string()
    }
}

impl Clean<String> for ast::Pat {
    fn clean(&self, tcx: &ty::ctxt) -> String {
        match self.node {
            ast::PatIdent(_, ref ident, _) => {
                ident.node.clean(tcx)
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
            ty_node: self.ty.id
        }
    }
}

impl Clean<PointerKind> for ast::Mutability {
    fn clean(&self, _: &ty::ctxt) -> PointerKind {
        use syntax::ast::Mutability;

        match *self {
            Mutability::MutMutable => PointerKind::Mutable,
            Mutability::MutImmutable => PointerKind::Const
        }
    }
}

impl Clean<Type> for ast::Ty {
    fn clean(&self, tcx: &ty::ctxt) -> Type {
        let (path, def) = match self.node {
            ast::TyPath(ref path, id) => (path.clone(), tcx.def_map.borrow()[id]),
            ast::TyTup(ref tup) => {
                assert!(tup.is_empty());
                return Type::Primitive(Primitive::Unit)
            },
            ast::TyPtr(ref mty) => {
                return Type::Pointer(mty.mutbl.clean(tcx), box mty.ty.clean(tcx))
            },
            _ => panic!("unimplemented: {}", self.node)
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
                did: def_id,
            },
            _ => panic!("not yet implemented: {}", def)
        }
    }
}

impl Clean<Return> for ast::FunctionRetTy {
    fn clean(&self, tcx: &ty::ctxt) -> Return {
        use syntax::ast::FunctionRetTy::{NoReturn, Return};
        let (ty, id) = match *self {
            Return(ref ty) => (ty.clean(tcx), Some(ty.id)),
            NoReturn(_) => (Type::Primitive(Primitive::Unit), None)
        };
        Return {
            ty: ty,
            ty_node: id
        }
    }
}

impl Clean<FnDecl> for ast::Item {
    fn clean(&self, tcx: &ty::ctxt) -> FnDecl {
        use syntax::ast::ItemFn;
        // FIXME: mangled names... export_name...
        match self.node {
            ItemFn(ref decl, _, _, _, _) => FnDecl {
                name: self.ident.clean(tcx),
                inputs: decl.inputs.clean(tcx),
                output: decl.output.clean(tcx),
                span: self.span
            },
            _ => unreachable!()
        }
    }
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>
}

impl Clean<Struct> for ast::Item {
    fn clean(&self, tcx: &ty::ctxt) -> Struct {
        use syntax::ast::ItemStruct;
        // FIXME: mangled names... export_name...
        match self.node {
            ItemStruct(ref decl, _,) => Struct {
                name: self.ident.clean(tcx),
                fields: decl.fields.clean(tcx)
            },
            _ => unreachable!()
        }
    }
}

#[deriving(Clone, Show, Hash, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: Type
}

impl Clean<StructField> for ast::StructField {
    fn clean(&self, tcx: &ty::ctxt) -> StructField {
        let name = match self.node.kind {
            ast::NamedField(ref id, _) => id.clean(tcx),
            ast::UnnamedField(_) => unimplemented!()
        };
        StructField {
            name: name,
            ty: self.node.ty.clean(tcx)
        }
    }
}

