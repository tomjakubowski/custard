// FIXME: these names are nonsense
use {Arg, FnDecl, Primitive, Type, TypeKind};

pub trait Cdecl {
    fn cdecl(&self) -> String;
}

impl Cdecl for FnDecl {
    fn cdecl(&self) -> String {
        format!("{} {}({});",
                self.output.ctype_spec(),
                self.name,
                self.inputs.cdecl())
    }
}

// Parameter list
impl Cdecl for Vec<Arg> {
    fn cdecl(&self) -> String {
        let mut w = vec![];
        let mut first = true;
        for arg in self.iter() {
            if first {
                first = false;
            } else {
                (write!(&mut w, ", ")).unwrap();
            }
            (write!(&mut w, "{} {}", arg.ty.ctype_spec(), arg.name)).unwrap();
        }
        String::from_utf8(w).unwrap()
    }
}

pub trait CtypeSpec {
    fn ctype_spec(&self) -> String;
}

impl CtypeSpec for Type {
    fn ctype_spec(&self) -> String {
        match self.kind {
            TypeKind::Primitive(Primitive::I8) => "int8_t".into_string(),
            TypeKind::Primitive(Primitive::I16) => "int16_t".into_string(),
            TypeKind::Primitive(Primitive::I32) => "int32_t".into_string(),
            TypeKind::Primitive(Primitive::I64) => "int64_t".into_string(),
            _ => unimplemented!()

        }
    }
}
