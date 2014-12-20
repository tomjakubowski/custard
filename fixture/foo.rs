#![feature(globs, lang_items)]
#![no_std]
#![crate_type="staticlib"]

extern crate core;
extern crate libc;

use core::prelude::*;

#[lang="panic_fmt"] fn panic_fmt() -> ! { loop { }}
#[lang="stack_exhausted"] fn stack_exhausted() -> ! { loop { }}
#[lang="eh_personality"] fn eh_personality() -> ! { loop { }}

#[repr(C)]
pub struct TinyPoint {
    pub x: i8,
    pub y: i8,
    pub z: i8
}
impl Copy for TinyPoint {}

#[repr(C)]
pub struct BigPoint {
    pub x: i64,
    pub y: i64,
    pub z: i64
}
impl Copy for BigPoint {}

pub extern "C" fn id_8(x: i8) -> i8 { x }
pub extern "C" fn id_16(x: i16) -> i16 { x }
pub extern "C" fn id_32(x: i32) -> i32 { x }
pub extern "C" fn id_tinypt(x: TinyPoint) -> TinyPoint { x }
pub extern "C" fn id_bigpt(x: BigPoint) -> BigPoint { x }

#[no_mangle]
pub extern "C" fn print_bigpt(p: BigPoint) {
    let fmt = b"BigPoint <%d, %d, %d>\n\0";
    unsafe { printf(fmt.as_ptr() as *const _, p.x, p.y, p.z) }
}

extern {
    fn printf(fmt: *const libc::c_char, ...);
}
