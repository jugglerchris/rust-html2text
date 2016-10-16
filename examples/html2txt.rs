extern crate html2text;
use std::io;

fn main() {
    let stdin = io::stdin();

    println!("{}", html2text::from_read(&mut stdin.lock(), 12));
}