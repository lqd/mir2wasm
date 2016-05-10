# Miri

An experimental compiler from [Rust] to [WebAssembly], based on rustc + Rust [MIR].

## Download Rust nightly

I currently recommend that you install [rustup] and then use it to
install the current rustc nightly version:

```sh
git clone https://github.com/brson/mir2wasm.git
cd mir2wasm
rustup override nightly
```

## Build && run

```sh
cargo build
```

```sh
cargo run -- --sysroot=`rustc --print sysroot` hello.rs
```

## License

Licensed under either of
  * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
    http://www.apache.org/licenses/LICENSE-2.0)
  * MIT license ([LICENSE-MIT](LICENSE-MIT) or
    http://opensource.org/licenses/MIT) at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual licensed as above, without any
additional terms or conditions.

[Rust]: https://www.rust-lang.org/
[WebAssembly]: https://webassembly.github.io/
[MIR]: https://github.com/rust-lang/rfcs/blob/master/text/1211-mir.md
[rustup]: https://www.rustup.rs
