#[cfg(unix)]
extern crate argparse;
#[cfg(unix)]
extern crate unicode_width;
#[cfg(unix)]
mod top {
    #[cfg(feature = "css")]
    use argparse::StoreFalse;
    use argparse::{ArgumentParser, Store};
    use html2text::render::{RichAnnotation, TaggedLine, TaggedLineElement};
    use std::collections::HashMap;
    use std::io::{self, Write};
    use termion::cursor::Goto;
    use termion::event::Key;
    use termion::input::TermRead;
    use termion::raw::IntoRawMode;
    use termion::screen::IntoAlternateScreen;
    use unicode_width::UnicodeWidthStr;

    fn to_style(tag: &[RichAnnotation]) -> String {
        use termion::color::*;
        let mut style = String::new();

        for ann in tag {
            match *ann {
                RichAnnotation::Default => (),
                RichAnnotation::Link(_) => {
                    style.push_str(&format!("{}", termion::style::Underline));
                }
                RichAnnotation::Image(_) => {
                    style.push_str(&format!("{}", Fg(LightBlue)));
                }
                RichAnnotation::Emphasis => {
                    style.push_str(&format!("{}", Fg(LightGreen)));
                }
                RichAnnotation::Strong => {
                    style.push_str(&format!("{}", Fg(LightGreen)));
                }
                RichAnnotation::Strikeout => (),
                RichAnnotation::Code => {
                    style.push_str(&format!("{}", Fg(LightYellow)));
                }
                RichAnnotation::Preformat(is_cont) => {
                    if is_cont {
                        style.push_str(&format!("{}", Fg(LightMagenta)));
                    } else {
                        style.push_str(&format!("{}", Fg(Magenta)));
                    }
                }
                RichAnnotation::Colour(col) => {
                    style.push_str(&format!("{}", Fg(Rgb(col.r, col.g, col.b))));
                }
                RichAnnotation::BgColour(col) => {
                    style.push_str(&format!("{}", Bg(Rgb(col.r, col.g, col.b))));
                }
                _ => todo!(),
            }
        }
        style
    }

    struct LinkMap {
        lines: Vec<Vec<Option<String>>>, // lines[y][x] => Some(URL) or None
    }

    impl LinkMap {
        pub fn link_at(&self, x: usize, y: usize) -> Option<&str> {
            if let Some(linevec) = self.lines.get(y) {
                if let Some(Some(text)) = linevec.get(x) {
                    return Some(text);
                }
            }
            None
        }
    }

    fn link_from_tag(tag: &[RichAnnotation]) -> Option<String> {
        let mut link = None;
        for annotation in tag {
            if let RichAnnotation::Link(text) = annotation {
                link = Some(text.clone());
            }
        }
        link
    }

    fn find_links(lines: &[TaggedLine<Vec<RichAnnotation>>]) -> LinkMap {
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

    fn find_frags(lines: &[TaggedLine<Vec<RichAnnotation>>]) -> FragMap {
        use self::TaggedLineElement::*;

        let mut map = HashMap::new();
        for (y, line) in lines.iter().enumerate() {
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
        }
        FragMap { start_xy: map }
    }

    struct Options {
        #[cfg(feature = "css")]
        use_css: bool,
    }

    impl Options {
        fn new() -> Options {
            Options {
                #[cfg(feature = "css")]
                use_css: true,
            }
        }
    }

    pub fn main() {
        let mut filename = String::new();
        #[allow(unused_mut)]
        let mut options = Options::new();
        {
            let mut ap = ArgumentParser::new();
            ap.refer(&mut filename)
                .add_argument("filename", Store, "Set HTML filename");
            #[cfg(feature = "css")]
            ap.refer(&mut options.use_css)
                .add_option(&["--no-css"], StoreFalse, "Disable CSS");
            ap.parse_args_or_exit();
        }

        let (width, height) = termion::terminal_size().unwrap();
        let (width, height) = (width as usize, height as usize);

        let mut file = std::fs::File::open(filename).expect("Tried to open file");

        let dom = html2text::config::plain()
            .parse_html(&mut file)
            .expect("Failed to parse HTML");

        let mut keys = io::stdin().keys();

        // top_y is the (0-based) index of the document line shown at
        // the top of the visible screen.
        let mut top_y = 0;
        // doc_x and doc_y are the logical (0-based) x and y of the
        // cursor position within the document.
        let mut doc_x = 0;
        let mut doc_y = 0;

        let mut screen = io::stdout()
            .into_raw_mode()
            .unwrap()
            .into_alternate_screen()
            .unwrap();

        let mut annotated = rerender(&dom, &[], width, &options);

        let link_map = find_links(&annotated);
        let frag_map = find_frags(&annotated);

        let mut inspect_path = vec![];

        loop {
            // max_y is the largest (0-based) index of a real document line.
            let max_y = annotated.len() - 1;

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
            let mut vis_y_limit = std::cmp::min(top_y + height, max_y + 1);
            if !inspect_path.is_empty() {
                vis_y_limit -= 1;
            }
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
            if !inspect_path.is_empty() {
                let mut pth = String::from("top ");
                let mut node = dom.document.clone();

                for &idx in &inspect_path {
                    node = node.nth_child(idx).unwrap();
                    pth.push_str(&format!("> {}", node.element_name().unwrap()));
                }
                write!(
                    screen,
                    "{}{}{:?}",
                    Goto(1, vis_y_limit as u16),
                    pth,
                    &inspect_path
                )
                .unwrap();
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
                        if inspect_path.is_empty() {
                            if doc_y < max_y {
                                doc_y += 1;
                            }
                        } else {
                            *inspect_path.last_mut().unwrap() += 1;
                            if dom.get_node_by_path(&inspect_path).is_none() {
                                // No next node - undo.
                                *inspect_path.last_mut().unwrap() -= 1;
                            } else {
                                annotated = rerender(&dom, &inspect_path, width, &options);
                            }
                        }
                    }
                    Key::Char('k') | Key::Up => {
                        if inspect_path.is_empty() {
                            doc_y = doc_y.saturating_sub(1);
                        } else if *inspect_path.last().unwrap() > 1 {
                            *inspect_path.last_mut().unwrap() -= 1;
                            annotated = rerender(&dom, &inspect_path, width, &options);
                        }
                    }
                    Key::Char('h') | Key::Left => {
                        if inspect_path.is_empty() {
                            doc_x = doc_x.saturating_sub(1);
                        } else if inspect_path.len() > 1 {
                            inspect_path.pop();
                            annotated = rerender(&dom, &inspect_path, width, &options);
                        }
                    }
                    Key::Char('l') | Key::Right => {
                        if inspect_path.is_empty() {
                            if doc_x + 1 < width {
                                doc_x += 1;
                            }
                        } else {
                            inspect_path.push(1);
                            if dom.get_node_by_path(&inspect_path).is_none() {
                                inspect_path.pop();
                            } else {
                                annotated = rerender(&dom, &inspect_path, width, &options);
                            }
                        }
                    }
                    Key::Char(' ') | Key::PageDown | Key::Ctrl('f') => {
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
                    Key::PageUp | Key::Ctrl('b') => {
                        // Ideally, move both the cursor and the top
                        // visible line up by a whole page. But bound
                        // both at zero.
                        doc_y = std::cmp::max(doc_y, height) - height;
                        top_y = std::cmp::max(top_y, height) - height;
                    }
                    Key::Home | Key::Char('g') => {
                        doc_y = 0;
                    }
                    Key::End | Key::Char('G') => {
                        doc_y = max_y;
                    }
                    Key::Char('\t') => {}
                    Key::Char('\r') | Key::Char('\n') => {
                        if let Some(url) = opt_url {
                            if let Some(u) = url.strip_prefix('#') {
                                let start = frag_map.start_xy.get(u);
                                if let Some((x, y)) = start {
                                    doc_x = *x;
                                    doc_y = *y;
                                }
                            }
                        }
                    }
                    #[cfg(feature = "css_ext")]
                    Key::Char('I') => {
                        // Enter/leave inspect mode
                        if inspect_path.is_empty() {
                            inspect_path.push(1);
                        } else {
                            inspect_path.clear();
                        }
                        annotated = rerender(&dom, &inspect_path, width, &options);
                    }
                    _ => {}
                }
            }
        }
    }

    fn rerender(
        dom: &html2text::RcDom,
        inspect_path: &[usize],
        width: usize,
        #[allow(unused)] options: &Options,
    ) -> Vec<TaggedLine<Vec<RichAnnotation>>> {
        let config = html2text::config::rich();
        #[cfg(feature = "css")]
        let config = if options.use_css {
            config
                .use_doc_css()
                .add_agent_css(
                    r#"
                    img {
                        color: #77f;
                    }
                "#,
                )
                .unwrap()
        } else {
            config
        };
        if inspect_path.is_empty() {
            let render_tree = config
                .dom_to_render_tree(dom)
                .expect("Failed to build render tree");
            config
                .render_to_lines(render_tree, width)
                .expect("Failed to render")
        } else {
            #[cfg(feature = "css_ext")]
            {
                let mut path_selector = String::new();
                for &idx in &inspect_path[1..] {
                    path_selector.push_str(&format!(" > :nth-child({})", idx));
                }
                let config = config
                    .add_agent_css(
                        &(format!(
                            r#"
                    html {} {{
                        color: white !important;
                        background-color: black !important;
                        display: x-raw-dom;
                    }}
                "#,
                            path_selector
                        )),
                    )
                    .expect("Invalid CSS");
                let render_tree = config
                    .dom_to_render_tree(dom)
                    .expect("Failed to build render tree");
                config
                    .render_to_lines(render_tree, width)
                    .expect("Failed to render")
            }
            #[cfg(not(feature = "css_ext"))]
            unreachable!()
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
