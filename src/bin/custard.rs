extern crate ironic;

pub fn main() {
    use std::collections::HashMap;
    use ironic::Input;

    let args = std::os::args();
    let crate_path = match &*args {
        [_, ref p] => p,
        _ => panic!("usage")
    };

    ironic::run_core(vec![Path::new("/usr/lib")], vec![], HashMap::new(),
        Input::File(Path::new(crate_path)), None);
}
