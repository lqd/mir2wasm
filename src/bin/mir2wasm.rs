#![feature(rustc_private, custom_attribute, link_args)]
#![allow(unused_attributes)]

extern crate env_logger;
extern crate getopts;
extern crate mir2wasm;
extern crate rustc;
extern crate rustc_driver;

// FIXME: C++ static linkage hacks. How do you do this for real?!
#[link_args = "-lstdc++ -static-libstdc++"]
extern { }

use mir2wasm::trans;
use rustc::session::Session;
use rustc_driver::{driver, CompilerCalls};
use std::process;

struct MiriCompilerCalls;

impl<'a> CompilerCalls<'a> for MiriCompilerCalls {
    fn build_controller(
        &mut self,
        _: &Session,
        _: &getopts::Matches
    ) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();

        control.after_analysis.stop = rustc_driver::Compilation::Stop;
        control.after_analysis.callback = Box::new(|state| {
            state.session.abort_if_errors();

            let entry_fn = state.session.entry_fn.borrow();
            let entry_fn = if let Some((node_id, _)) = *entry_fn { Some(node_id) } else { None };
            trans::trans_crate(&state.tcx.unwrap(), state.mir_map.unwrap(), entry_fn)
                .unwrap(); // FIXME
        });

        control
    }
}

#[miri_run]
fn main() {
    env_logger::init().unwrap();

    let mut args: Vec<String> = std::env::args().collect();
    args.push("--target=arm-unknown-linux-gnueabi".to_string());
    match rustc_driver::run_compiler(&args, &mut MiriCompilerCalls) {
        (Ok(_), _) => process::exit(0),
        (Err(code), _) => process::exit(code as i32)
    }
}
