use clean::{Arg, FnDecl, Primitive, Struct, Type};

// FIXME: these names are nonsense
pub trait Cdecl {
    fn cdecl(&self) -> String;
}

impl Cdecl for FnDecl {
    fn cdecl(&self) -> String {
        format!("{} {}({});",
                self.output.ty.ctype_spec(),
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

impl Cdecl for Struct {
    fn cdecl(&self) -> String {
        let mut out = Vec::<u8>::with_capacity(32);
        write!(&mut out, "struct {} {{", self.name).unwrap();
        for f in self.fields.iter() {
            write!(&mut out, "\n    {} {};", f.ty.ctype_spec(), f.name).unwrap();
        }
        writeln!(&mut out, "\n}};").unwrap();
        String::from_utf8(out).unwrap()
    }
}

pub trait CtypeSpec {
    fn ctype_spec(&self) -> String;
}

impl CtypeSpec for Struct {
    fn ctype_spec(&self) -> String {
        format!("struct {}", self.name)
    }
}
impl CtypeSpec for Type {
    fn ctype_spec(&self) -> String {
        match *self {
            Type::Primitive(Primitive::Unit) => "void".into_string(),
            Type::Primitive(Primitive::I8) => "int8_t".into_string(),
            Type::Primitive(Primitive::I16) => "int16_t".into_string(),
            Type::Primitive(Primitive::I32) => "int32_t".into_string(),
            Type::Primitive(Primitive::I64) => "int64_t".into_string(),
            Type::ResolvedPath { ref path, .. } => {
                let ref last = path.segments[path.segments.len() - 1];
                // FIXME: wrong for not-structs
                format!("struct {}", last.identifier.as_str())
            }
        }
    }
}
