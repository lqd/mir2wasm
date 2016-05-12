extern crate cmake;

fn main() {
    let dst = cmake::build("binaryen");

    println!("cargo:rustc-link-search=native={}/build/lib", dst.display());
    println!("cargo:rustc-link-lib=static=binaryen");
    println!("cargo:rustc-link-lib=static=passes");
    println!("cargo:rustc-link-lib=static=support");
    println!("cargo:rustc-link-lib=static=emscripten-optimizer");
    println!("cargo:rustc-link-lib=static=asmjs");
}
