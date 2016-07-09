#![feature(intrinsics, lang_items, start, no_core, fundamental)]
#![no_core]
#![allow(dead_code)]
#![allow(unused_variables)]

#[lang = "sized"]
#[fundamental]
pub trait Sized { }

#[lang = "copy"]
pub trait Copy : Clone { }

pub trait Clone : Sized { }

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;
    fn add(self, rhs: RHS) -> Self::Output;
}

impl Add for isize {
    type Output = isize;
    fn add(self, rhs: isize) -> Self::Output { self + rhs }
}


#[link(name = "c")]
extern { }

extern { fn puts(s: *const u8); }
extern "rust-intrinsic" { fn transmute<T, U>(t: T) -> U; }

#[lang = "eh_personality"] extern fn eh_personality() {}
#[lang = "eh_unwind_resume"] extern fn eh_unwind_resume() {}
#[lang = "panic_fmt"] fn panic_fmt() -> ! { loop {} }
#[no_mangle] pub extern fn rust_eh_register_frames () {}
#[no_mangle] pub extern fn rust_eh_unregister_frames () {}

fn real_main() -> isize {
    let i = 1;
    let j = i + 2;
    j
}

#[start]
fn main(i: isize, _: *const *const u8) -> isize {
    /*unsafe {
        let (ptr, _): (*const u8, usize) = transmute("Hello!\0");
        puts(ptr);
}*/

    let result = real_main() + 3;
    result
}
