extern crate html2text;
use std::io;

fn main() {
    let mut stdin = io::stdin();

    println!("{}", html2text::from_read(&mut stdin.lock()));
}