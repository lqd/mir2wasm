#![feature(intrinsics, lang_items, no_core, fundamental)]
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

pub mod ops {
    /// A version of the call operator that takes a by-value receiver.
    #[lang = "fn_once"]
    #[fundamental] // so that regex can rely that `&str: !FnMut`
    pub trait FnOnce<Args> {
        /// The returned type after the call operator is used.
        type Output;

        /// This is called when the call operator is used.
        extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
    }

    #[lang = "drop"]
    pub trait Drop {
        fn drop(&mut self);
    }
}

use self::Option::*;
use ops::*;

#[derive(Clone, Copy)]
enum Option<T> {
    None,
    Some(T)
}

// Panic and assert
#[lang = "panic"] fn panic() -> ! { loop {} }

macro_rules! panic {
    () => (
        panic!("explicit panic")
    );
    ($msg:expr) => ({
        $crate::panic()
    });
}

impl<T> Option<T> {
    #[inline]
    pub fn unwrap(self) -> T {
        match self {
            Some(val) => val,
            None => panic!("called `Option::unwrap()` on a `None` value"),
        }
    }
}

fn main() {
    let o = Some(5);
    let i = o.unwrap();
}
