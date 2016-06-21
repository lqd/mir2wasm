#![feature(intrinsics, lang_items, start, no_core, libc, fundamental)]
#![no_core]

#[lang = "sized"]
#[fundamental]
pub trait Sized { }

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

#[lang = "mul"]
pub trait Mul<RHS=Self> {
    type Output;
    fn mul(self, rhs: RHS) -> Self::Output;
}

impl Mul for isize {
    type Output = isize;
    fn mul(self, rhs: isize) -> Self::Output { self * rhs }
}

#[lang = "div"]
pub trait Div<RHS=Self> {
    type Output;
    fn div(self, rhs: RHS) -> Self::Output;
}

impl Div for isize {
    type Output = isize;
    fn div(self, rhs: isize) -> Self::Output { self / rhs }
}

fn test() {
    main(1, 0 as _);
}

#[start]
fn main(i: isize, _: *const *const u8) -> isize {
    ((i + 3) * 2 - 2) / 3
}
