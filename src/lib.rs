#![feature(
    btree_range,
    collections,
    collections_bound,
    core_intrinsics,
    filling_drop,
    question_mark,
    rustc_private,
)]

#[macro_use] extern crate rustc;
extern crate rustc_mir;
extern crate syntax;

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate log;
extern crate env_logger;

pub mod error;
pub mod trans;
//mod binaryen;
