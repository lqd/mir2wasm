#![feature(intrinsics, lang_items, start, no_core, fundamental)]
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

impl PartialEq for isize {
    #[inline]
    fn eq(&self, other: &isize) -> bool { (*self) == (*other) }
    #[inline]
    fn ne(&self, other: &isize) -> bool { (*self) != (*other) }
}

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;
    fn add(self, rhs: RHS) -> Self::Output;
}

impl Add for isize {
    type Output = isize;
    fn add(self, rhs: isize) -> Self::Output { self + rhs }
}

#[lang = "sub"]
pub trait Sub<RHS=Self> {
    type Output;
    fn sub(self, rhs: RHS) -> Self::Output;
}

impl Sub for isize {
    type Output = isize;
    fn sub(self, rhs: isize) -> Self::Output { self - rhs }
}

fn fibonacci_recursive(n: isize) -> isize {
    if n == 0 || n == 1 {
        n
    } else {
        fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
    }
}

#[start]
fn main(i: isize, _: *const *const u8) -> isize {
    fibonacci_recursive(i)
}
