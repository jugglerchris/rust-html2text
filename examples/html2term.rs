extern crate html2text;
extern crate termion;
extern crate argparse;
use std::io::{self, Write};
use html2text::render::text_renderer::{RichAnnotation};
use argparse::{ArgumentParser, Store};
use termion::input::TermRead;
use termion::event::Key;
use termion::raw::IntoRawMode;
use termion::cursor::Goto;

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
    let mut filename = String::new();
    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut filename)
          .add_argument("filename", Store, "Set HTML filename");
        ap.parse_args_or_exit();
    }

    let (width, height) = termion::terminal_size().unwrap();
    let (width, height) = (width as usize, height as usize);

    let mut file = std::fs::File::open(filename).expect("Tried to open file");
    let annotated = html2text::from_read_rich(&mut file, width as usize);

    let mut keys = io::stdin().keys();
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let mut pos = 0;
    let mut cursor_x = 1;
    let mut cursor_y = 1;
    loop {
        let maxpos = std::cmp::min(pos+height, annotated.len());
        write!(stdout, "{}", termion::clear::All).unwrap();
        for (i, line) in annotated[pos..maxpos].iter().enumerate() {
            write!(stdout, "{}", Goto(1, i as u16 +1)).unwrap();
            for (s, tag) in line.iter() {
                let style = to_style(tag);
                write!(stdout, "{}{}{}", style, s, termion::style::Reset).unwrap();
            }
        }
        write!(stdout, "{}", Goto(cursor_x, cursor_y)).unwrap();
        stdout.flush().unwrap();
        if let Some(Ok(k)) = keys.next() {
            match k {
                Key::Char('q') => break,
                Key::Char('j') => {
                    if cursor_y < height as u16 {
                        cursor_y += 1;
                    }
                },
                Key::Char('k') => {
                    if cursor_y > 1 {
                        cursor_y -= 1;
                    }
                },
                Key::Char('h') => {
                    if cursor_x > 1 {
                        cursor_x -= 1;
                    }
                },
                Key::Char('l') => {
                    if cursor_x < width as u16 {
                        cursor_x += 1;
                    }
                },
                Key::Char(' ') | Key::PageDown => {
                    if pos + height-1 < annotated.len() {
                        pos += height-1;
                    }
                },
                Key::PageUp => {
                    if pos >= height-1 {
                        pos -= height-1;
                    }
                },
                _ => {},
            }
        }
    }
}