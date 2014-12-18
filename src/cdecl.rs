use clean::{Arg, FnDecl, Primitive, Type};

// FIXME: these names are nonsense
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

impl Cdecl for Type {
    fn cdecl(&self) -> String {
        match *self {
            Type::ResolvedPath { ref path, .. } => {
                let ref last = path.segments[path.segments.len() - 1];
                format!("struct {}", last.identifier.as_str()) // wrrrooonnngggg
            }
            Type::Primitive(_) => panic!("cannot redeclare a primitive!"),
        }
    }
}

pub trait CtypeSpec {
    fn ctype_spec(&self) -> String;
}

impl CtypeSpec for Type {
    fn ctype_spec(&self) -> String {
        match *self {
            Type::Primitive(Primitive::I8) => "int8_t".into_string(),
            Type::Primitive(Primitive::I16) => "int16_t".into_string(),
            Type::Primitive(Primitive::I32) => "int32_t".into_string(),
            Type::Primitive(Primitive::I64) => "int64_t".into_string(),
            Type::ResolvedPath { ref path, .. } => {
                let ref last = path.segments[path.segments.len() - 1];
                format!("struct {}", last.identifier.as_str())
            }
            _ => unimplemented!()

        }
    }
}
