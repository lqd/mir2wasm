#![feature(
    question_mark,
    rustc_private,
)]

#[macro_use] extern crate rustc;
extern crate rustc_mir;
extern crate syntax;

extern crate libc;

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate log;
extern crate env_logger;

pub mod error;
pub mod trans;
mod binaryen;
