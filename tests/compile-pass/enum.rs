#![feature(lang_items, no_core)]
#![allow(dead_code)]
#![no_core]

#[lang="sized"]
trait Sized {}

#[lang="copy"]
trait Copy {}

enum Tag {
    A(isize),
    B(isize)
}

fn main() {
    let a = Tag::A(5);
    match a {
        Tag::A(i) => i,
        Tag::B(i) => i,
    };
}
