extern crate custard;

pub fn main() {
    let args = std::env::args();
    custard::run_core(args);
}
