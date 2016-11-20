extern crate html2text;
extern crate termion;
use std::io::{self, Write};

fn main() {
    let stdin = io::stdin();

    let annotated = html2text::from_read_rich(&mut stdin.lock(), 80);
    for line in annotated {
        for (s, tag) in line.iter() {
            write!(io::stdout(), "{}", s);
        }
        write!(io::stdout(), "\n");
    }
}