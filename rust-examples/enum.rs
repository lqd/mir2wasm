#![feature(lang_items, no_core, main)]
#![allow(dead_code)]
#![no_core]

#[lang="sized"]
trait Sized {}

#[lang="copy"]
trait Copy {}

// access to the wasm "spectest" module test printing functions
mod wasm {
    pub fn print_i32(i: i32) {
        unsafe { _print_i32(i); }
    }

    extern {
        fn _print_i32(i: i32);
    }
}

enum Tag {
    A(i32),
    B(i32),
    C,
}

#[main]
fn main() {
    let t = Tag::B(17);
    let i = match t {
        Tag::A(_) => 1,
        Tag::B(i) => i,
        _ => 3
    };
    wasm::print_i32(i); // (i32.const 17)
}
