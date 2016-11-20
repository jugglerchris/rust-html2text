extern crate html2text;
extern crate termion;
use std::io;

fn main() {
    let stdin = io::stdin();

    println!("{:?}", html2text::from_read_rich(&mut stdin.lock(), 80));
}