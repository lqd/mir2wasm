#![feature(intrinsics, lang_items, main, no_core, fundamental)]
#![no_core]

// use clone::Clone;
use marker::Sized;

#[main]
fn main() {
    wasm::print_i32(-100);
    for i in 0..10 {
        wasm::print_i32(i);
    }
    wasm::print_i32(-100);
}

#[lang = "add_assign"]
pub trait AddAssign<Rhs=Self> {
    fn add_assign(&mut self, Rhs);
}

impl AddAssign for i32 {
    #[inline]
    fn add_assign(&mut self, other: i32) { *self += other }
}

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

pub mod ops {
    // #[derive(Clone, PartialEq, Eq, Hash)]  // not Copy -- see #27186
    pub struct Range<Idx> {
        /// The lower bound of the range (inclusive).
        pub start: Idx,
        /// The upper bound of the range (exclusive).
        pub end: Idx,
    }
}

pub mod iter {
    use marker::Sized;
    use clone::Clone;
    use option::Option::{self, Some, None};
    use ops::*;
    // use cmp::Ordering::*;
    // use cmp::*;

    pub trait IntoIterator {
        type Item;
        type IntoIter: Iterator<Item=Self::Item>;
        fn into_iter(self) -> Self::IntoIter;
    }

    pub trait Iterator {
        type Item;
        fn next(&mut self) -> Option<Self::Item>;
    }

    pub struct RangeIterator {
        idx: i32,
        // range: Range<i32>, // struct ds struct!
        start: i32,
        end: i32,
    }

    impl Iterator for RangeIterator {
        type Item = i32;
        fn next(&mut self) -> Option<Self::Item> {
            ::wasm::print_i32(-111);
            ::wasm::print_i32(self.idx); // 0 normalement, là bien 1
            // ::wasm::print_i32(self.range.start); // 65741
            // ::wasm::print_i32(self.range.end); // 0 au lieu de 10
            ::wasm::print_i32(self.start); // 1 ok
            ::wasm::print_i32(self.end); // 1 au lieu de 10
            ::wasm::print_i32(-111);
            if self.idx == self.end {
                ::wasm::print_i32(-12345);
                return None;
            }
            let result = self.idx;
            self.idx += 1;
            Some(result)
        }
    }

    impl IntoIterator for Range<i32> {
        type Item = i32;
        type IntoIter = RangeIterator;
        fn into_iter(self) -> Self::IntoIter {
            ::wasm::print_i32(-666);
            ::wasm::print_i32(self.start);
            ::wasm::print_i32(self.end);
            ::wasm::print_i32(-666);
            RangeIterator {
                idx: 0, // test
                // range: self,
                start: self.start,
                end: self.end,
            }
        }
    }
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

    // pub fn assert_receiver_is_clone<T: Clone + ?Sized>(_: &T) {}
    //
    // macro_rules! clone_impl {
    //     ($t:ty) => {
    //         impl Clone for $t {
    //             /// Returns a deep copy of the value.
    //             #[inline]
    //             fn clone(&self) -> $t { *self }
    //         }
    //     }
    // }
    //
    // clone_impl! { i32 }
}

mod option {
    // #[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord/*, Debug, Hash*/)]
    pub enum Option<T> {
        /// No value
        None,
        /// Some value `T`
        Some(T)
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
