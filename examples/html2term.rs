extern crate html2text;
extern crate termion;
extern crate argparse;
use std::io::{self, Write};
use html2text::render::text_renderer::{RichAnnotation};
use argparse::{ArgumentParser, Store, StoreOption};

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
    let mut filename = String::new();
    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut filename)
          .add_argument("filename", Store, "Set HTML filename");
        ap.parse_args_or_exit();
    }

    let (width, _) = termion::terminal_size().unwrap();

    let mut file = std::fs::File::open(filename).expect("Tried to open file");
    let annotated = html2text::from_read_rich(&mut file, width as usize);
    for line in annotated {
        for (s, tag) in line.iter() {
            let style = to_style(tag);
            write!(io::stdout(), "{}{}{}", style, s, termion::style::Reset).unwrap();
        }
        write!(io::stdout(), "\n").unwrap();
    }
}