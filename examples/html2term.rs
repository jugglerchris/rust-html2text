extern crate html2text;
extern crate termion;
use std::io::{self, Write};
use std::fmt::Display;
use html2text::render::text_renderer::{RichAnnotation};

fn to_style(tag: &Vec<RichAnnotation>) -> Box<Display> {
    if tag.len() > 0 {
        Box::new(termion::style::Underline)
    } else {
        Box::new(termion::style::Reset)
    }
}

fn main() {
    let stdin = io::stdin();

    let annotated = html2text::from_read_rich(&mut stdin.lock(), 80);
    for line in annotated {
        for (s, tag) in line.iter() {
            let style = to_style(tag);
            write!(io::stdout(), "{}{}{}", style, s, termion::style::Reset);
        }
        write!(io::stdout(), "\n");
    }
}