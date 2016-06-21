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

    print_deps(Path::new("binaryen"));
}

fn print_deps(path: &Path) {
    for e in path.read_dir().unwrap().filter_map(|e| e.ok()) {
        let file_type = e.file_type().unwrap();
        if file_type.is_dir() {
            print_deps(&e.path());
        } else {
            println!("cargo:rerun-if-changed={}", e.path().display());
        }
    }
}
