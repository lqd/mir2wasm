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

use self::Option::*;

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
    let a : Option<i32> = Some(5i32);
    let b : Option<i64> = Some(5i64);

    a.unwrap();
    b.unwrap();
}
