#![feature(inclusive_range_syntax)]

extern crate compiletest_rs as compiletest;
#[macro_use]
extern crate log;

use std::fs::File;
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{PathBuf, Path};
use std::str;
use std::str::FromStr;


/// Gets a vector of strings that are expected to be in the output of
/// this test.
///
/// Test source may include comments that indicate text that must
/// occur in the output when it is run. This text must be prefixed by
/// `//~`. For example:
/// 
/// ```
/// //~ (i32.const 6)
/// wasm::print_i32(6)
/// ```
///
/// This function will extract all of the test strings from the `//~`
/// comments.
fn get_expected_outputs(filename: &Path) -> Vec<String> {
    let mut outputs = Vec::new();
    let file = File::open(filename).expect("could not open file");
    let file = BufReader::new(file);
    
    for line in file.lines() {
        let line = line.unwrap();

        let separator = "//~";
        
        match line.find(separator) {
            Some(i) => {
                let pattern = line[(i + separator.len())...line.len()-1].trim();
                debug!("Found pattern `{}` in {}", pattern, filename.display());
                outputs.push(String::from_str(pattern).unwrap())
            },
            None => continue
        }
    }
    
    outputs
}

/// Checks whether the stdout bytes includes all of the expected
/// strings in the right order.
///
/// This allows some flexibility. The test strings do not have to be
/// consecutive, just in the right order. However, only one test
/// string is allowed per line.
fn match_stdout(stdout: &Vec<u8>, expected: &Vec<String>) -> Result<(), ()> {
    let mut stdout = str::from_utf8(stdout).unwrap().lines();

    for expect in expected {
        loop {
            match stdout.next() {
                Some(line) => {
                    if line.contains(expect) {
                        break; // continue the for loop, we found the string we were looking for.
                    } else {
                        continue; // go on to the next line
                    }
                },
                None => {
                    let stderr = std::io::stderr();
                    writeln!(stderr.lock(), "expected string {} not found", expect).unwrap();
                    return Err(())
                }
            }
        }
    }
    
    // If we made it to here, we found all the strings we were looking for.
    Ok(())
}

#[test] #[ignore]
fn compile_fail() {
    let sysroot = &find_sysroot();
    let flags = format!("--sysroot {} -Dwarnings", sysroot);
    for_all_targets(sysroot, |target| {
        let mut config = compiletest::default_config();
        config.host_rustcflags = Some(flags.clone());
        config.mode = "compile-fail".parse().expect("Invalid mode");
        config.run_lib_path =
            Path::new(sysroot).join("lib").join("rustlib").join(&target).join("lib");
        config.rustc_path = "target/debug/mir2wasm".into();
        config.src_base = PathBuf::from("tests/compile-fail".to_string());
        config.target = target.to_owned();
        config.target_rustcflags = Some(flags.clone());
        compiletest::run_tests(&config);
    });
}

fn should_ignore(filename: &Path) -> bool {
    let mut file = File::open(filename).expect("could not open file");
    let mut source = String::new();

    file.read_to_string(&mut source).expect("could not read file");

    return source.contains("xfail")
}

struct TestSuite<'a> {
    name: &'a str,
    run: bool,
    path: String,
}

impl<'a> TestSuite<'a> {
    fn new(name: &str) -> TestSuite {
        TestSuite {
            name: name,
            run: false,
            path: format!("tests/{}", name)
        }
    }

    fn set_run(&mut self, run: bool) -> &'a mut TestSuite {
        self.run = run; self
    }

    fn path(&mut self, path: &str) -> &'a mut TestSuite {
        self.path = String::from_str(path).unwrap(); self
    }

    fn run(&self) {
        let sysroot = find_sysroot();
        let path = &self.path;

        for_all_targets(&sysroot, |target| {
            let (mut pass, mut fail, mut ignored) = (0, 0, 0);

            for file in std::fs::read_dir(&path).unwrap() {
                let file = file.unwrap();
                let path = file.path();

                if !file.metadata().unwrap().is_file() || !path.to_str().unwrap().ends_with(".rs") {
                    continue;
                }

                if should_ignore(&path) {
                    ignored += 1;
                    continue;
                }

                let stderr = std::io::stderr();
                write!(stderr.lock(), "test [{}] {} ... ", self.name, path.display()).unwrap();
                let mut cmd = std::process::Command::new("target/debug/mir2wasm");
                cmd.arg(&path);
                cmd.arg("-Dwarnings");
                if self.run {
                    cmd.arg("--run");
                }
                let libs = Path::new(&sysroot).join("lib");
                let sysroot = libs.join("rustlib").join(&target).join("lib");
                let paths = std::env::join_paths(&[libs, sysroot]).unwrap();
                cmd.env(compiletest::procsrv::dylib_env_var(), paths);

                match cmd.output() {
                    Ok(ref output) if output.status.success() => {
                        if self.run {
                            let expected = get_expected_outputs(&path);
                            match match_stdout(&output.stdout, &expected) {
                                Ok(()) => {
                                    writeln!(stderr.lock(), "ok").unwrap();
                                    pass += 1;
                                },
                                Err(()) => {
                                    writeln!(stderr.lock(), "Test execution failed: {}",
                                             &path.display()).unwrap();
                                    fail += 1;
                                }
                            }
                        } else {
                            writeln!(stderr.lock(), "ok").unwrap();
                            pass += 1;
                        }
                    }
                    Ok(output) => {
                        writeln!(stderr.lock(), "FAILED with exit code {:?}",
                                 output.status.code()).unwrap();
                        writeln!(stderr.lock(), "stdout: \n {}",
                                 std::str::from_utf8(&output.stdout).unwrap()).unwrap();
                        writeln!(stderr.lock(), "stderr: \n {}",
                                 std::str::from_utf8(&output.stderr).unwrap()).unwrap();
                        fail += 1;
                    }
                    Err(e) => {
                        writeln!(stderr.lock(), "FAILED: {}", e).unwrap();
                        fail += 1;
                    },
                }
            }
            let stderr = std::io::stderr();
            writeln!(stderr.lock(), "[{}] {} passed; {} failed; {} ignored",
                     self.name, pass, fail, ignored).unwrap();
            if fail > 0 {
                panic!("some compile-pass tests failed")
            }
        });
    }
}

#[test]
fn compile_pass() {
    TestSuite::new("compile-pass").run()
}

#[test]
fn run_compile_pass() {
    // TODO(eholk): This is a temporary test just to make sure we get
    // some coverage on our compile-pass tests. Eventually we should
    // fold compile-pass into run-pass and completely remove the
    // distinction.
    TestSuite::new("run-compile-pass").path("tests/compile-pass").set_run(true).run()
}

#[test]
fn run_pass() {
    TestSuite::new("run-pass").set_run(true).run()
}

fn for_all_targets<F: FnMut(String)>(sysroot: &str, mut f: F) {
    for target in std::fs::read_dir(format!("{}/lib/rustlib/", sysroot)).unwrap() {
        let target = target.unwrap();
        if !target.metadata().unwrap().is_dir() {
            continue;
        }
        let target = target.file_name().into_string().unwrap();
        if target == "etc" {
            continue;
        }
        let stderr = std::io::stderr();
        writeln!(stderr.lock(), "running tests for target {}", target).unwrap();
        f(target);
    }
}

#[test]
fn empty_test() {
    // show the test harness is running by getting at least one
    // successful test.
}

fn find_sysroot() -> String {
    // Taken from https://github.com/Manishearth/rust-clippy/pull/911.
    let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
    let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
    match (home, toolchain) {
        (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
        _ => option_env!("RUST_SYSROOT")
            .expect("need to specify RUST_SYSROOT env var or use rustup or multirust")
            .to_owned(),
    }
}
