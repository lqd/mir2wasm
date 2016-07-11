#![feature(lang_items, main, fundamental, no_core)]
#![no_core]

#[main]
fn main() {
    let x = 6;
    wasm::print_i32(x); // (i32.const 6)

    let i = match x {
        i @ 1 ... 5 => i,
        _ => -2
    };
    wasm::print_i32(i); // (i32.const -2)    

    let i = match x {
        1 | 2 => -1,
        3 => -2,
        6 => -3,
        _ => -4
    };
    wasm::print_i32(i); // (i32.const -3)
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
