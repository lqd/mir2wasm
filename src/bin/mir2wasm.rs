#![feature(rustc_private, link_args)]

extern crate env_logger;
extern crate getopts;
extern crate mir2wasm;
extern crate rustc;
extern crate rustc_driver;

// FIXME: C++ static linkage hacks. How do you do this for real?!
#[link_args = "-lstdc++ -static-libstdc++"]
extern { }

use mir2wasm::trans::{self, WasmTransOptions};
use rustc::session::Session;
use rustc_driver::{driver, CompilerCalls};
use std::process;

struct WasmCompilerCalls {
    options: WasmTransOptions
}

impl WasmCompilerCalls {
    fn new(options: WasmTransOptions) -> WasmCompilerCalls {
        WasmCompilerCalls {
            options: options
        }
    }
}

impl<'a> CompilerCalls<'a> for WasmCompilerCalls {
    fn build_controller(
        &mut self,
        _: &Session,
        _: &getopts::Matches
    ) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();
        let options = self.options;

        control.after_analysis.stop = rustc_driver::Compilation::Stop;
        control.after_analysis.callback = Box::new(move |state| {
            state.session.abort_if_errors();

            let entry_fn = state.session.entry_fn.borrow();
            let entry_fn = if let Some((node_id, _)) = *entry_fn { Some(node_id) } else { None };
            trans::trans_crate(&state.tcx.unwrap(), state.mir_map.unwrap(), entry_fn, &options)
                .unwrap(); // FIXME
        });

        control
    }
}

fn main() {
    env_logger::init().unwrap();

    let wasm_compiler_args = ["--run", "-O", "-q", "-h", "--help"];
    let rustc_args : Vec<String> =
        std::env::args().filter(|arg| !wasm_compiler_args.contains(&arg.as_ref())).collect();

    // TODO: use a command line parsing library
    let mut options = WasmTransOptions::new();
    for flag in std::env::args().filter(|arg| wasm_compiler_args.contains(&arg.as_ref())) {
        match flag.as_ref() {
            "--run" => { options.interpret = true }
            "-O" => { options.optimize = true }
            "-q" => { options.print = false }
            "-h" | "--help" => {
                let usage = "mir2wasm [OPTIONS] INPUT \n\n\
                             Options: \n    \
                             -h, --help    Display this message \n    \
                             -O            Optimize the compiled wast module \n    \
                             --run         Run the compiled module through the interpreter, without printing it \n    \
                             -q            Don't print the compiled wast module";
                println!("usage: {}", usage);
                process::exit(0);
            }
            _ => panic!("unexpected compiler flag: {}", flag)
        }
    }

    let mut compiler_calls = WasmCompilerCalls::new(options);
    match rustc_driver::run_compiler(&rustc_args, &mut compiler_calls) {
        (Ok(_), _) => process::exit(0),
        (Err(code), _) => process::exit(code as i32)
    }
}
