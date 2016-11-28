extern crate html2text;
extern crate termion;
extern crate argparse;
extern crate unicode_width;
use std::io::{self, Write};
use html2text::render::text_renderer::{RichAnnotation,TaggedLine};
use argparse::{ArgumentParser, Store};
use termion::input::TermRead;
use termion::event::Key;
use termion::raw::IntoRawMode;
use termion::cursor::Goto;
use unicode_width::UnicodeWidthStr;

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

struct LinkMap {
    lines: Vec<Vec<Option<String>>>,  // lines[y][x] => Some(URL) or None
}

impl LinkMap {
    pub fn link_at(&self, x: usize, y: usize) -> Option<&str> {
        if let Some(ref linevec) = self.lines.get(y) {
            if let Some(&Some(ref text)) = linevec.get(x) {
                return Some(&text)
            }
        }
        None
    }
}

fn link_from_tag(tag: &Vec<RichAnnotation>) -> Option<String> {
    let mut link = None;
    for annotation in tag {
        if let RichAnnotation::Link(ref text) = *annotation {
            link = Some(text.clone());
        }
    }
    link
}

fn find_links(lines: &Vec<TaggedLine<Vec<RichAnnotation>>>) -> LinkMap {
    let mut map = Vec::new();
    for line in lines {
        let mut linevec = Vec::new();

        for (s, tag) in line.iter() {
            let link = link_from_tag(tag);
            for _ in 0..UnicodeWidthStr::width(s) {
                linevec.push(link.clone());
            }
        }

        map.push(linevec);
    }
    LinkMap {
        lines: map,
    }
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

    let link_map = find_links(&annotated);

    let mut keys = io::stdin().keys();
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let mut pos = 0;
    // 1-based screen co-ordinates
    let mut cursor_x = 1;
    let mut cursor_y = 1;
    loop {
        let opt_url = link_map.link_at(cursor_x as usize - 1, cursor_y as usize +pos-1);
        let maxpos = std::cmp::min(pos+height, annotated.len());
        write!(stdout, "{}", termion::clear::All).unwrap();
        for (i, line) in annotated[pos..maxpos].iter().enumerate() {
            let mut x = 0;
            write!(stdout, "{}", Goto(1, i as u16 +1)).unwrap();
            for (s, tag) in line.iter() {
                let style = to_style(tag);
                let link = link_from_tag(tag);
                match (opt_url, link) {
                    (Some(ref t1), Some(ref t2)) if t1 == t2 => {
                        write!(stdout, "{}", termion::style::Invert).unwrap();
                    },
                    _ => (),
                }
                write!(stdout, "{}{}{}", style, s, termion::style::Reset).unwrap();
            }
        }
        write!(stdout, "{}", Goto(cursor_x, cursor_y)).unwrap();
        stdout.flush().unwrap();
        if let Some(Ok(k)) = keys.next() {
            match k {
                Key::Char('q') => break,
                Key::Char('j') | Key::Down => {
                    if cursor_y < height as u16 {
                        cursor_y += 1;
                    }
                },
                Key::Char('k') | Key::Up => {
                    if cursor_y > 1 {
                        cursor_y -= 1;
                    }
                },
                Key::Char('h') | Key::Left => {
                    if cursor_x > 1 {
                        cursor_x -= 1;
                    }
                },
                Key::Char('l') | Key::Right => {
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
                Key::Char('\t') => {
                },
                _ => {},
            }
        }
    }
}