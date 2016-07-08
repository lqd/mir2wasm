#![feature(lang_items, no_core, main, fundamental, intrinsics)]
#![no_core]

#[main]
fn main() {
    let x: i32 = 0;
    let y: i32 = 1;

    // (i32.const 0)
    wasm::print_i32((x == y) as i32);
    wasm::print_i32((x.eq(&y)) as i32);

    // (i32.const 1)
    wasm::print_i32((x != y) as i32);
    wasm::print_i32((x.ne(&y)) as i32);

    // (i32.const 1)
    wasm::print_i32((x < y) as i32);
    wasm::print_i32((x.lt(&y)) as i32);

    // (i32.const 1)
    wasm::print_i32((x <= y) as i32);
    wasm::print_i32((x.le(&y)) as i32);

    // (i32.const 0)
    wasm::print_i32((x > y) as i32);
    wasm::print_i32((x.gt(&y)) as i32);

    // (i32.const 0)
    wasm::print_i32((x >= y) as i32);
    wasm::print_i32((x.ge(&y)) as i32);
}

#[cold] #[inline(never)] // this is the slow path, always
#[lang = "panic"]
pub fn panic(_: &(&'static str, &'static str, u32)) -> ! {
    // Use Arguments::new_v1 instead of format_args!("{}", expr) to potentially
    // reduce size overhead. The format_args! macro uses str's Display trait to
    // write expr, which calls Formatter::pad, which must accommodate string
    // truncation and padding (even though none is used here). Using
    // Arguments::new_v1 may allow the compiler to omit Formatter::pad from the
    // output binary, saving up to a few kilobytes.
    // let (expr, file, line) = *expr_file_line;
    panic_fmt()
}

#[lang = "panic_fmt"] fn panic_fmt() -> ! { loop {} }

pub mod marker {
    use clone::Clone;

    #[lang = "sized"]
    #[fundamental]
    pub trait Sized { }

    #[lang = "copy"]
    pub trait Copy : Clone { }
}

use cmp::*;

pub mod clone {
    use marker::Sized;

    pub trait Clone : Sized {
        fn clone(&self) -> Self;
    }
}

// pub trait Hash { }

mod option {
    // #[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord/*, Debug, Hash*/)]
    #[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord/*, Debug, Hash*/)]
    pub enum Option<T> {
        /// No value
        None,
        /// Some value `T`
        Some(T)
    }
}

pub mod intrinsics {
    extern "rust-intrinsic" {
        /// Returns the value of the discriminant for the variant in 'v',
        /// cast to a `u64`; if `T` has no discriminant, returns 0.
        pub fn discriminant_value<T>(v: &T) -> u64;
    }
}

pub mod cmp {
    use marker::Sized;
    use option::Option::{self, Some};
    use self::Ordering::*;

    #[lang = "eq"]
    pub trait PartialEq<Rhs: ?Sized = Self> {
        fn eq(&self, other: &Rhs) -> bool;

        #[inline]
        fn ne(&self, other: &Rhs) -> bool { !self.eq(other) }
    }

    pub trait Eq: PartialEq<Self> {
        #[doc(hidden)]
        #[inline(always)]
        fn assert_receiver_is_total_eq(&self) {}
    }

    #[derive(Clone, Copy, PartialEq, /*Debug, Hash*/)]
    pub enum Ordering {
        Less = -1,
        Equal = 0,
        Greater = 1,
    }

    impl Ordering {
        #[inline]
        pub fn reverse(self) -> Ordering {
            match self {
                Less => Greater,
                Equal => Equal,
                Greater => Less,
            }
        }
    }

    #[inline]
    pub fn min<T: Ord>(v1: T, v2: T) -> T {
        if v1 <= v2 { v1 } else { v2 }
    }

    #[inline]
    pub fn max<T: Ord>(v1: T, v2: T) -> T {
        if v2 >= v1 { v2 } else { v1 }
    }

    pub trait Ord: Eq + PartialOrd<Self> {
        fn cmp(&self, other: &Self) -> Ordering;
    }

    impl Eq for Ordering {}

    impl Ord for Ordering {
        #[inline]
        fn cmp(&self, other: &Ordering) -> Ordering {
            (*self as i32).cmp(&(*other as i32))
        }
    }

    impl PartialOrd for Ordering {
        #[inline]
        fn partial_cmp(&self, other: &Ordering) -> Option<Ordering> {
            (*self as i32).partial_cmp(&(*other as i32))
        }
    }

    #[lang = "ord"]
    pub trait PartialOrd<Rhs: ?Sized = Self>: PartialEq<Rhs> {
        fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;

        #[inline]
        fn lt(&self, other: &Rhs) -> bool {
            match self.partial_cmp(other) {
                Some(Less) => true,
                _ => false,
            }
        }

        #[inline]
        fn le(&self, other: &Rhs) -> bool {
            match self.partial_cmp(other) {
                Some(Less) | Some(Equal) => true,
                _ => false,
            }
        }

        #[inline]
        fn gt(&self, other: &Rhs) -> bool {
            match self.partial_cmp(other) {
                Some(Greater) => true,
                _ => false,
            }
        }

        #[inline]
        fn ge(&self, other: &Rhs) -> bool {
            match self.partial_cmp(other) {
                Some(Greater) | Some(Equal) => true,
                _ => false,
            }
        }
    }

    // Implementation of PartialEq, Eq, PartialOrd and Ord for primitive types
    mod impls {
        use super::{PartialOrd, Ord, PartialEq, Eq, Ordering};
        use super::Ordering::{Less, Greater, Equal};

        use option::Option::{self, Some};
        use marker::Sized;

        // use super::Sized;
        // use super::Option;
        // use super::Option::{Some, None};

        // use super::Ordering::{Less, Greater, Equal};
        // use super::super::*;
        // use super::*;
        // use super::super::Option::{Some/*, None*/};

        macro_rules! partial_eq_impl {
            ($($t:ty)*) => ($(

                impl PartialEq for $t {
                    #[inline]
                    fn eq(&self, other: &$t) -> bool { (*self) == (*other) }
                    #[inline]
                    fn ne(&self, other: &$t) -> bool { (*self) != (*other) }
                }
            )*)
        }


        impl PartialEq for () {
            #[inline]
            fn eq(&self, _other: &()) -> bool { true }
            #[inline]
            fn ne(&self, _other: &()) -> bool { false }
        }

        // partial_eq_impl! {
        //     bool char usize u8 u16 u32 u64 isize i8 i16 i32 i64 f32 f64
        // }

        partial_eq_impl! {
            bool i32 u8 isize //char usize u16 u32 u64 i8 i16 i64 f32 f64
        }

        macro_rules! eq_impl {
            ($($t:ty)*) => ($(

                impl Eq for $t {}
            )*)
        }

        // eq_impl! { () bool char usize u8 u16 u32 u64 isize i8 i16 i32 i64 }

        eq_impl! {
            () bool i32 u8 isize //char usize u16 u32 u64 i8 i16 i64
        }

        macro_rules! partial_ord_impl {
            ($($t:ty)*) => ($(

                impl PartialOrd for $t {
                    #[inline]
                    fn partial_cmp(&self, other: &$t) -> Option<Ordering> {
                        match (self <= other, self >= other) {
                            (false, false) => None,
                            (false, true) => Some(Greater),
                            (true, false) => Some(Less),
                            (true, true) => Some(Equal),
                        }
                    }
                    #[inline]
                    fn lt(&self, other: &$t) -> bool { (*self) < (*other) }
                    #[inline]
                    fn le(&self, other: &$t) -> bool { (*self) <= (*other) }
                    #[inline]
                    fn ge(&self, other: &$t) -> bool { (*self) >= (*other) }
                    #[inline]
                    fn gt(&self, other: &$t) -> bool { (*self) > (*other) }
                }
            )*)
        }


        impl PartialOrd for () {
            #[inline]
            fn partial_cmp(&self, _: &()) -> Option<Ordering> {
                Some(Equal)
            }
        }


        impl PartialOrd for bool {
            #[inline]
            fn partial_cmp(&self, other: &bool) -> Option<Ordering> {
                (*self as u8).partial_cmp(&(*other as u8))
            }
        }

        // partial_ord_impl! { f32 f64 }

        macro_rules! ord_impl {
            ($($t:ty)*) => ($(

                impl PartialOrd for $t {
                    #[inline]
                    fn partial_cmp(&self, other: &$t) -> Option<Ordering> {
                        Some(self.cmp(other))
                    }
                    #[inline]
                    fn lt(&self, other: &$t) -> bool { (*self) < (*other) }
                    #[inline]
                    fn le(&self, other: &$t) -> bool { (*self) <= (*other) }
                    #[inline]
                    fn ge(&self, other: &$t) -> bool { (*self) >= (*other) }
                    #[inline]
                    fn gt(&self, other: &$t) -> bool { (*self) > (*other) }
                }


                impl Ord for $t {
                    #[inline]
                    fn cmp(&self, other: &$t) -> Ordering {
                        if *self == *other { Equal }
                        else if *self < *other { Less }
                        else { Greater }
                    }
                }
            )*)
        }


        impl Ord for () {
            #[inline]
            fn cmp(&self, _other: &()) -> Ordering { Equal }
        }


        impl Ord for bool {
            #[inline]
            fn cmp(&self, other: &bool) -> Ordering {
                (*self as u8).cmp(&(*other as u8))
            }
        }

        // ord_impl! { char usize u8 u16 u32 u64 isize i8 i16 i32 i64 }

        ord_impl! {
            i32 u8 isize // char usize u16 u32 u64 i8 i16 i64
        }

        // & pointers


        impl<'a, 'b, A: ?Sized, B: ?Sized> PartialEq<&'b B> for &'a A where A: PartialEq<B> {
            #[inline]
            fn eq(&self, other: & &'b B) -> bool { PartialEq::eq(*self, *other) }
            #[inline]
            fn ne(&self, other: & &'b B) -> bool { PartialEq::ne(*self, *other) }
        }

        impl<'a, 'b, A: ?Sized, B: ?Sized> PartialOrd<&'b B> for &'a A where A: PartialOrd<B> {
            #[inline]
            fn partial_cmp(&self, other: &&'b B) -> Option<Ordering> {
                PartialOrd::partial_cmp(*self, *other)
            }
            #[inline]
            fn lt(&self, other: & &'b B) -> bool { PartialOrd::lt(*self, *other) }
            #[inline]
            fn le(&self, other: & &'b B) -> bool { PartialOrd::le(*self, *other) }
            #[inline]
            fn ge(&self, other: & &'b B) -> bool { PartialOrd::ge(*self, *other) }
            #[inline]
            fn gt(&self, other: & &'b B) -> bool { PartialOrd::gt(*self, *other) }
        }

        impl<'a, A: ?Sized> Ord for &'a A where A: Ord {
            #[inline]
            fn cmp(&self, other: & &'a A) -> Ordering { Ord::cmp(*self, *other) }
        }

        impl<'a, A: ?Sized> Eq for &'a A where A: Eq {}

        // &mut pointers


        impl<'a, 'b, A: ?Sized, B: ?Sized> PartialEq<&'b mut B> for &'a mut A where A: PartialEq<B> {
            #[inline]
            fn eq(&self, other: &&'b mut B) -> bool { PartialEq::eq(*self, *other) }
            #[inline]
            fn ne(&self, other: &&'b mut B) -> bool { PartialEq::ne(*self, *other) }
        }

        impl<'a, 'b, A: ?Sized, B: ?Sized> PartialOrd<&'b mut B> for &'a mut A where A: PartialOrd<B> {
            #[inline]
            fn partial_cmp(&self, other: &&'b mut B) -> Option<Ordering> {
                PartialOrd::partial_cmp(*self, *other)
            }
            #[inline]
            fn lt(&self, other: &&'b mut B) -> bool { PartialOrd::lt(*self, *other) }
            #[inline]
            fn le(&self, other: &&'b mut B) -> bool { PartialOrd::le(*self, *other) }
            #[inline]
            fn ge(&self, other: &&'b mut B) -> bool { PartialOrd::ge(*self, *other) }
            #[inline]
            fn gt(&self, other: &&'b mut B) -> bool { PartialOrd::gt(*self, *other) }
        }

        impl<'a, A: ?Sized> Ord for &'a mut A where A: Ord {
            #[inline]
            fn cmp(&self, other: &&'a mut A) -> Ordering { Ord::cmp(*self, *other) }
        }

        impl<'a, A: ?Sized> Eq for &'a mut A where A: Eq {}


        impl<'a, 'b, A: ?Sized, B: ?Sized> PartialEq<&'b mut B> for &'a A where A: PartialEq<B> {
            #[inline]
            fn eq(&self, other: &&'b mut B) -> bool { PartialEq::eq(*self, *other) }
            #[inline]
            fn ne(&self, other: &&'b mut B) -> bool { PartialEq::ne(*self, *other) }
        }


        impl<'a, 'b, A: ?Sized, B: ?Sized> PartialEq<&'b B> for &'a mut A where A: PartialEq<B> {
            #[inline]
            fn eq(&self, other: &&'b B) -> bool { PartialEq::eq(*self, *other) }
            #[inline]
            fn ne(&self, other: &&'b B) -> bool { PartialEq::ne(*self, *other) }
        }
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

// mod intrinsics {
// use self::Ordering::*;
// use super::Option::{self, Some};

// impl Clone for Ordering {}
// impl Copy for Ordering {}
// impl<Rhs> PartialEq<Rhs> for Ordering {
//     fn eq(&self, other: &Rhs) -> bool {
//         self == other
//     }
// }
