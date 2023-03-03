#[cfg(unix)]
extern crate argparse;
extern crate html2text;
#[cfg(unix)]
extern crate termion;
#[cfg(unix)]
extern crate unicode_width;
#[cfg(unix)]
mod top {
    use ::html2text;
    use ::std;
    use ::termion;
    use argparse::{ArgumentParser, Store};
    use html2text::render::text_renderer::{RichAnnotation, TaggedLine, TaggedLineElement};
    use std::collections::HashMap;
    use std::io::{self, Write};
    use termion::cursor::Goto;
    use termion::event::Key;
    use termion::input::TermRead;
    use termion::raw::IntoRawMode;
    use termion::screen::AlternateScreen;
    use unicode_width::UnicodeWidthStr;

    fn to_style(tag: &Vec<RichAnnotation>) -> String {
        let mut style = String::new();

        for ann in tag {
            match *ann {
                RichAnnotation::Default => (),
                RichAnnotation::Link(_) => {
                    style.push_str(&format!("{}", termion::style::Underline));
                }
                RichAnnotation::Image(_) => {
                    style.push_str(&format!(
                        "{}",
                        termion::color::Fg(termion::color::LightBlue)
                    ));
                }
                RichAnnotation::Emphasis => {
                    style.push_str(&format!(
                        "{}",
                        termion::color::Fg(termion::color::LightGreen)
                    ));
                }
                RichAnnotation::Strong => {
                    style.push_str(&format!(
                        "{}",
                        termion::color::Fg(termion::color::LightGreen)
                    ));
                }
                RichAnnotation::Strikeout => (),
                RichAnnotation::Code => {
                    style.push_str(&format!(
                        "{}",
                        termion::color::Fg(termion::color::LightYellow)
                    ));
                }
                RichAnnotation::Preformat(is_cont) => {
                    if is_cont {
                        style.push_str(&format!(
                            "{}",
                            termion::color::Fg(termion::color::LightMagenta)
                        ));
                    } else {
                        style.push_str(&format!("{}", termion::color::Fg(termion::color::Magenta)));
                    }
                }
            }
        }
        style
    }

    struct LinkMap {
        lines: Vec<Vec<Option<String>>>, // lines[y][x] => Some(URL) or None
    }

    impl LinkMap {
        pub fn link_at(&self, x: usize, y: usize) -> Option<&str> {
            if let Some(ref linevec) = self.lines.get(y) {
                if let Some(&Some(ref text)) = linevec.get(x) {
                    return Some(&text);
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

            for ts in line.tagged_strings() {
                let link = link_from_tag(&ts.tag);
                for _ in 0..UnicodeWidthStr::width(ts.s.as_str()) {
                    linevec.push(link.clone());
                }
            }

            map.push(linevec);
        }
        LinkMap { lines: map }
    }

    struct FragMap {
        start_xy: HashMap<String, (usize, usize)>,
    }

    fn find_frags(lines: &Vec<TaggedLine<Vec<RichAnnotation>>>) -> FragMap {
        use self::TaggedLineElement::*;

        let mut map = HashMap::new();
        let mut y = 0;
        for line in lines {
            let mut x = 0;
            for tli in line.iter() {
                match tli {
                    FragmentStart(fragname) => {
                        map.insert(fragname.to_string(), (x, y));
                    }
                    Str(ts) => {
                        x += UnicodeWidthStr::width(ts.s.as_str());
                    }
                }
            }
            y += 1;
        }
        FragMap { start_xy: map }
    }

    pub fn main() {
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
        let frag_map = find_frags(&annotated);

        let mut keys = io::stdin().keys();

        // max_y is the largest (0-based) index of a real document line.
        let max_y = annotated.len() - 1;
        // top_y is the (0-based) index of the document line shown at
        // the top of the visible screen.
        let mut top_y = 0;
        // doc_x and doc_y are the logical (0-based) x and y of the
        // cursor position within the document.
        let mut doc_x = 0;
        let mut doc_y = 0;

        let mut screen = AlternateScreen::from(io::stdout().into_raw_mode().unwrap());

        loop {
            // Sanity-check the current screen position. max_y should
            // be small enough that no blank lines beyond the end of
            // the document are visible on screen (except when the
            // document is shorter than a screenful); large enough
            // that the cursor isn't off the bottom of the visible
            // screen; and small enough that the cursor isn't off the
            // top.
            if max_y >= height - 1 {
                top_y = std::cmp::min(top_y, max_y - (height - 1));
            }
            if doc_y >= height - 1 {
                top_y = std::cmp::max(top_y, doc_y - (height - 1));
            }
            top_y = std::cmp::min(top_y, doc_y);

            let opt_url = link_map.link_at(doc_x, doc_y);
            let vis_y_limit = std::cmp::min(top_y + height, max_y + 1);
            write!(screen, "{}", termion::clear::All).unwrap();
            for (i, line) in annotated[top_y..vis_y_limit].iter().enumerate() {
                write!(screen, "{}", Goto(1, i as u16 + 1)).unwrap();
                for ts in line.tagged_strings() {
                    let style = to_style(&ts.tag);
                    let link = link_from_tag(&ts.tag);
                    match (opt_url, link) {
                        (Some(ref t1), Some(ref t2)) if t1 == t2 => {
                            write!(screen, "{}", termion::style::Invert).unwrap();
                        }
                        _ => (),
                    }
                    write!(screen, "{}{}{}", style, ts.s, termion::style::Reset).unwrap();
                }
            }

            // 1-based screen coordinates
            let cursor_x = (doc_x + 1) as u16;
            let cursor_y = (doc_y - top_y + 1) as u16;
            write!(screen, "{}", Goto(cursor_x, cursor_y)).unwrap();

            screen.flush().unwrap();
            if let Some(Ok(k)) = keys.next() {
                match k {
                    Key::Char('q') => break,
                    Key::Char('j') | Key::Down => {
                        if doc_y < max_y {
                            doc_y += 1;
                        }
                    }
                    Key::Char('k') | Key::Up => {
                        if doc_y > 0 {
                            doc_y -= 1;
                        }
                    }
                    Key::Char('h') | Key::Left => {
                        if doc_x > 0 {
                            doc_x -= 1;
                        }
                    }
                    Key::Char('l') | Key::Right => {
                        if doc_x + 1 < width {
                            doc_x += 1;
                        }
                    }
                    Key::Char(' ') | Key::PageDown => {
                        // Ideally, move both the cursor and the top
                        // visible line down by a whole page
                        doc_y += height;
                        top_y += height;

                        // But bound the cursor within the document
                        doc_y = std::cmp::min(doc_y, max_y);

                        // And the standard bounds checking for top_y
                        // will take care of the rest of the special
                        // cases.
                    }
                    Key::PageUp => {
                        // Ideally, move both the cursor and the top
                        // visible line up by a whole page. But bound
                        // both at zero.
                        doc_y = std::cmp::max(doc_y, height) - height;
                        top_y = std::cmp::max(top_y, height) - height;
                    }
                    Key::Home => {
                        doc_y = 0;
                    }
                    Key::End => {
                        doc_y = max_y;
                    }
                    Key::Char('\t') => {}
                    Key::Char('\r') | Key::Char('\n') => {
                        if let Some(url) = opt_url {
                            if url.starts_with("#") {
                                let start = frag_map.start_xy.get(&url[1..]);
                                if let Some((x, y)) = start {
                                    doc_x = *x;
                                    doc_y = *y;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

#[cfg(not(unix))]
mod top {
    pub fn main() {}
}

fn main() {
    top::main()
}
