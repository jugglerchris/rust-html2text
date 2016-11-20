extern crate html2text;
extern crate termion;
use std::io::{self, Write};
use html2text::render::text_renderer::{RichAnnotation};

fn to_style(tag: &Vec<RichAnnotation>) -> String {
    let mut style = String::new();

    for ann in tag {
        match *ann {
            RichAnnotation::Default => (),
            RichAnnotation::Link(_) => {
                style.push_str(&format!("{}", termion::style::Underline));
            },
            RichAnnotation::Image => {
                style.push_str(&format!("{}", termion::color::Fg(termion::color::LightBlue)));
            },
        }
    }
    style
}

fn main() {
    let stdin = io::stdin();

    let (width, _) = termion::terminal_size().unwrap();

    let annotated = html2text::from_read_rich(&mut stdin.lock(), width as usize);
    for line in annotated {
        for (s, tag) in line.iter() {
            let style = to_style(tag);
            write!(io::stdout(), "{}{}{}", style, s, termion::style::Reset).unwrap();
        }
        write!(io::stdout(), "\n").unwrap();
    }
}