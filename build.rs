extern crate cmake;

use std::process::Command;
use std::path::Path;

fn main() {
    if !Path::new("binaryen/.git").exists() {
        let _ = Command::new("git").args(&["submodule", "update", "--init"])
                        .status();
    }
    let dst = cmake::build("binaryen");

    println!("cargo:rustc-link-search=native={}/build/lib", dst.display());
    println!("cargo:rustc-link-lib=static=binaryen");
    println!("cargo:rustc-link-lib=static=passes");
    println!("cargo:rustc-link-lib=static=support");
    println!("cargo:rustc-link-lib=static=emscripten-optimizer");
    println!("cargo:rustc-link-lib=static=asmjs");
}
