#![feature(intrinsics, lang_items, main, no_core, fundamental)]
#![no_core]

#[lang = "sized"]
#[fundamental]
pub trait Sized { }

#[lang = "copy"]
pub trait Copy : Clone { }

pub trait Clone : Sized { }

#[lang = "mul"]
pub trait Mul<RHS=Self> {
    type Output;
    fn mul(self, rhs: RHS) -> Self::Output;
}

impl Mul for i32 {
    type Output = i32;
    fn mul(self, rhs: i32) -> Self::Output { self * rhs }
}

// access to the wasm "spectest" module test printing functions
mod wasm {
    pub fn print_i32(i: i32) {
        unsafe { _print_i32(i); }
    }

    extern {
        fn _print_i32(i: i32);
    }
}

struct Rectangle {
    w: i32,
    h: i32,
}

impl Rectangle {
    fn area(&self) -> i32 {
        self.w * self.h
    }
}

#[main]
fn main() {
    let mut r = Rectangle {w: 2, h: 5};
    wasm::print_i32(r.area()); // (i32.const 10)

    r.w = 3;
    wasm::print_i32(r.area()); // (i32.const 15)
}
