#![feature(rustc_private, custom_attribute, link_args)]
#![allow(unused_attributes)]

extern crate env_logger;
extern crate getopts;
extern crate mir2wasm;
extern crate rustc;
extern crate rustc_driver;

use mir2wasm::trans;
use rustc::session::Session;
use rustc_driver::{driver, CompilerCalls};

// FIXME: C++ static linkage hacks. How do you do this for real?!
#[link_args = "-lstdc++ -static-libstdc++"]
extern { }

struct MiriCompilerCalls;

impl<'a> CompilerCalls<'a> for MiriCompilerCalls {
    fn build_controller(
        &mut self,
        _: &Session,
        _: &getopts::Matches
    ) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();

        control.after_analysis.callback = Box::new(|state| {
            state.session.abort_if_errors();
            trans::translate_crate(state.tcx.unwrap(), state.mir_map.unwrap())
                .unwrap(); // FIXME
        });

        control
    }
}

#[miri_run]
fn main() {
    env_logger::init().unwrap();

    let args: Vec<String> = std::env::args().collect();
    rustc_driver::run_compiler(&args, &mut MiriCompilerCalls);
}
