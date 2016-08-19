#![feature(rustc_private, link_args)]

extern crate env_logger;
extern crate getopts;
#[macro_use]
extern crate log;
extern crate mir2wasm;
extern crate rustc;
extern crate rustc_driver;

// FIXME: C++ static linkage hacks. How do you do this for real?!
#[link_args = "-lstdc++ -static-libstdc++"]
extern { }

use getopts::{optflag, getopts};
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

    let opts = &[
        optflag("r", "run", "run the compiled module through the interpreter, without printing it"),
        optflag("q", "", "do not print the compiled wast module"),
        optflag("O", "", "optimize the compiled wast module"),
        optflag("h", "help", "display this help message"),
    ];

    fn is_wasm_arg(s: &String, opts: &[getopts::OptGroup]) -> bool {
        for o in opts {
            if s.starts_with("--") && &s[2..] == &o.long_name {
                return true;
            }
            if s.starts_with("-") && &s[1..] == &o.short_name {
                return true;
            }
        }
        return false;
    };

    let args : Vec<String> = std::env::args().collect();
    info!("command line: {:?}", args);

    let rustc_args : Vec<String> =
        std::env::args().filter(|arg| !is_wasm_arg(arg, opts)).collect();
    let wasm_args : Vec<String> =
        std::env::args().filter(|arg| is_wasm_arg(arg, opts)).collect();

    let mut options = WasmTransOptions::new();

    let matches = getopts(&wasm_args[..], opts).expect("could not parse command line arguments");

    if matches.opt_present("h") {
        print!("{}", getopts::usage("Usage: mir2wasm [options]", opts));
        return;
    }
    if matches.opt_present("r") {
        options.interpret = true;
    }
    if matches.opt_present("O") {
        options.optimize = true;
    }
    if matches.opt_present("q") {
        options.print = false;
    }
    
    let mut compiler_calls = WasmCompilerCalls::new(options);
    match rustc_driver::run_compiler(&rustc_args, &mut compiler_calls) {
        (Ok(_), _) => process::exit(0),
        (Err(code), _) => process::exit(code as i32)
    }
}
