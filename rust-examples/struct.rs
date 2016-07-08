#![feature(intrinsics, lang_items, main, no_core, fundamental)]
#![no_core]

#[derive(Clone, Copy)]
struct Rectangle {
    w: i32,
    h: i32,
}

// impl Rectangle {
//     // fn area(&self) -> i32 {
//     //     self.w * self.h
//     // }

//     fn dump(&self) {
//         wasm::print_i32(self.w);
//     }
// }

use clone::Clone;

#[main]
fn main() {
    let mut r = Rectangle {w: 2, h: 5};
    // // wasm::print_i32(r.area()); // (i32.const 10)

    // // r.w = 3;
    // // wasm::print_i32(r.area()); // (i32.const 15)

    // r.dump();

    let mut r = r.clone();
    // r.dump();

    wasm::print_i32(r.w);
    // wasm::print_i32(r.h);
    // r.w = 4;
    // wasm::print_i32(r.w);
    // wasm::print_i32(r.h);
    // // wasm::print_i32(r.area()); // (i32.const 20)
}

pub mod marker {
    use clone::Clone;

    #[lang = "sized"]
    #[fundamental]
    pub trait Sized { }

    #[lang = "copy"]
    pub trait Copy : Clone { }
}

pub mod clone {
    use marker::Sized;

    pub trait Clone : Sized {
        fn clone(&self) -> Self;
    }

    // probably shouldn't have to trans this
    pub fn assert_receiver_is_clone<T: Clone + ?Sized>(_: &T) {}

    macro_rules! clone_impl {
        ($t:ty) => {
            impl Clone for $t {
                /// Returns a deep copy of the value.
                #[inline]
                fn clone(&self) -> $t { *self }
            }
        }
    }

    clone_impl! { i32 }
}

// #[lang = "mul"]
// pub trait Mul<RHS=Self> {
//     type Output;
//     fn mul(self, rhs: RHS) -> Self::Output;
// }

// impl Mul for i32 {
//     type Output = i32;
//     fn mul(self, rhs: i32) -> Self::Output { self * rhs }
// }

// access to the wasm "spectest" module test printing functions
mod wasm {
    pub fn print_i32(i: i32) {
        unsafe { _print_i32(i); }
    }

    extern {
        fn _print_i32(i: i32);
    }
}
