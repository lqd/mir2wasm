#![feature(fundamental, lang_items, no_core, main)]
#![allow(dead_code)]
#![no_core]

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

mod wasm {
    // access to the wasm "spectest" module test printing functions
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

#[derive(Clone, Copy)]
enum Ordering {
    Less = -1,
    Equal = 0,
    Greater = 1,
}

enum Ordering2 {
    Less = 0x1,
    Equal = 0x2,
    Greater = 0x3,
}

impl Ordering {
    fn print_consume(self) {
        wasm::print_i32(self as i32);
    }

    fn print_ref(&self) {
        wasm::print_i32(*self as i32);
    }
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

    Ordering::Less.print_consume();
    // Ordering::Less.print_ref(); // creates a promoted block

    let x = Ordering::Greater;
    x.print_ref();

    wasm::print_i32(-500);

    wasm::print_i32(Ordering::Less as i32);
    wasm::print_i32(Ordering::Equal as i32);
    wasm::print_i32(Ordering::Greater as i32);

    wasm::print_i32(-1000);

    wasm::print_i32(Ordering2::Less as i32);
    wasm::print_i32(Ordering2::Equal as i32);
    wasm::print_i32(Ordering2::Greater as i32);

    wasm::print_i32(-2000);

    let x = Ordering::Greater;
    match &x {
        &Ordering::Less => wasm::print_i32(111),
        &Ordering::Equal => wasm::print_i32(222),
        &Ordering::Greater => wasm::print_i32(333),
    }
    match x {
        Ordering::Less => wasm::print_i32(Ordering::Less as i32),
        Ordering::Equal => wasm::print_i32(Ordering::Equal as i32),
        Ordering::Greater => wasm::print_i32(Ordering::Greater as i32),
    }

    wasm::print_i32(-3000);

    let x = Ordering2::Equal;
    match &x {
        &Ordering2::Less => wasm::print_i32(111),
        &Ordering2::Equal => wasm::print_i32(222),
        &Ordering2::Greater => wasm::print_i32(333),
    }
    match x {
        Ordering2::Less => wasm::print_i32(Ordering2::Less as i32),
        Ordering2::Equal => wasm::print_i32(Ordering2::Equal as i32),
        Ordering2::Greater => wasm::print_i32(Ordering2::Greater as i32),
    }
}
