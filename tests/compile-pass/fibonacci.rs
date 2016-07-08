#![feature(intrinsics, lang_items, main, no_core, fundamental)]
#![no_core]

#[lang = "sized"]
#[fundamental]
pub trait Sized { }

#[lang = "copy"]
pub trait Copy : Clone { }

pub trait Clone : Sized { }

#[lang = "eq"]
pub trait PartialEq<Rhs: ?Sized = Self> {
    fn eq(&self, other: &Rhs) -> bool;

    #[inline]
    fn ne(&self, other: &Rhs) -> bool { !self.eq(other) }
}

impl PartialEq for i32 {
    #[inline]
    fn eq(&self, other: &i32) -> bool { (*self) == (*other) }
    #[inline]
    fn ne(&self, other: &i32) -> bool { (*self) != (*other) }
}

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;
    fn add(self, rhs: RHS) -> Self::Output;
}

impl Add for i32 {
    type Output = i32;
    fn add(self, rhs: i32) -> Self::Output { self + rhs }
}

#[lang = "sub"]
pub trait Sub<RHS=Self> {
    type Output;
    fn sub(self, rhs: RHS) -> Self::Output;
}

impl Sub for i32 {
    type Output = i32;
    fn sub(self, rhs: i32) -> Self::Output { self - rhs }
}

#[lang = "add_assign"]
pub trait AddAssign<Rhs=Self> {
    fn add_assign(&mut self, Rhs);
}

impl AddAssign for i32 {
    #[inline]
    fn add_assign(&mut self, other: i32) { *self += other }
}

fn fibonacci_recursive(n: i32) -> i32 {
    if n == 0 || n == 1 {
        n
    } else {
        fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
    }
}

fn fibonacci_iterative(n: i32) -> i32 {
    let mut current = 0;
    let mut next = 1;

    let mut iterator = 0;
    loop {
        if iterator == n {
            break;
        }

        let tmp = current + next;
        current = next;
        next = tmp;

        iterator += 1;
    }

    current
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

// Unusual example just to test trait methods
trait Fibonacci {
    fn fibonacci(&self) -> Self;
}

impl Fibonacci for i32 {
    fn fibonacci(&self) -> i32 {
        fibonacci_iterative(*self)
    }
}

#[main]
fn main() {
    let result = fibonacci_recursive(10);
    wasm::print_i32(result); // (i32.const 55)

    let result = fibonacci_iterative(25);
    wasm::print_i32(result); // (i32.const 75025)

    let result = fibonacci_recursive(25);
    wasm::print_i32(result); // a slower (i32.const 75025)

    // trait example
    let nth = 20;
    let result = nth.fibonacci();
    wasm::print_i32(result); // (i32.const 6765)

    // the following two, however, create 'promoted' blocks, which are not yet implemented
    // let result = Fibonacci::fibonacci(&10);
    // let result = 10.fibonacci();
}
