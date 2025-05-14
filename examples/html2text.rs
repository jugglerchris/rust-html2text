extern crate argparse;
extern crate html2text;
use argparse::{ArgumentParser, Store, StoreOption, StoreTrue};
use html2text::config::{self, Config};
use html2text::render::{TextDecorator, TrivialDecorator};
use log::trace;
use std::io;
use std::io::Write;

#[cfg(unix)]
use html2text::render::RichAnnotation;
#[cfg(unix)]
fn default_colour_map(
    annotations: &[RichAnnotation],
    s: &str,
    use_css_colours: bool,
    no_default_colours: bool,
) -> String {
    use termion::color::*;
    use RichAnnotation::*;
    // Explicit CSS colours override any other colours
    let mut have_explicit_colour = no_default_colours;
    let mut start = Vec::new();
    let mut finish = Vec::new();
    trace!("default_colour_map: str={s}, annotations={annotations:?}");
    for annotation in annotations.iter() {
        match annotation {
            Default => {}
            Link(_) => {
                start.push(format!("{}", termion::style::Underline));
                finish.push(format!("{}", termion::style::Reset));
            }
            Image(_) => {
                if !have_explicit_colour {
                    start.push(format!("{}", Fg(Blue)));
                    finish.push(format!("{}", Fg(Reset)));
                }
            }
            Emphasis => {
                start.push(format!("{}", termion::style::Bold));
                finish.push(format!("{}", termion::style::Reset));
            }
            Strong => {
                if !have_explicit_colour {
                    start.push(format!("{}", Fg(LightYellow)));
                    finish.push(format!("{}", Fg(Reset)));
                }
            }
            Strikeout => {
                if !have_explicit_colour {
                    start.push(format!("{}", Fg(LightBlack)));
                    finish.push(format!("{}", Fg(Reset)));
                }
            }
            Code => {
                if !have_explicit_colour {
                    start.push(format!("{}", Fg(Blue)));
                    finish.push(format!("{}", Fg(Reset)));
                }
            }
            Preformat(_) => {
                if !have_explicit_colour {
                    start.push(format!("{}", Fg(Blue)));
                    finish.push(format!("{}", Fg(Reset)));
                }
            }
            Colour(c) => {
                if use_css_colours {
                    start.push(format!("{}", Fg(Rgb(c.r, c.g, c.b))));
                    finish.push(format!("{}", Fg(Reset)));
                    have_explicit_colour = true;
                }
            }
            BgColour(c) => {
                if use_css_colours {
                    start.push(format!("{}", Bg(Rgb(c.r, c.g, c.b))));
                    finish.push(format!("{}", Bg(Reset)));
                }
            }
            _ => {}
        }
    }
    // Reverse the finish sequences
    finish.reverse();
    let mut result = start.join("");
    result.push_str(s);
    for s in finish {
        result.push_str(&s);
    }
    trace!("default_colour_map: output={result}");
    result
}

#[cfg(feature = "css_ext")]
fn do_syntect_highlight<'t>(text: &'t str, language: &str) -> Vec<(html2text::TextStyle, &'t str)> {
    use html2text::{Colour, TextStyle};
    use syntect::{
        easy::HighlightLines, highlighting::ThemeSet, parsing::SyntaxSet, util::LinesWithEndings,
    };

    let ps = SyntaxSet::load_defaults_newlines();
    let ts = ThemeSet::load_defaults();

    let syntax = ps.find_syntax_by_extension(&language).unwrap();
    let mut h = HighlightLines::new(syntax, &ts.themes["Solarized (dark)"]);

    let mut results = Vec::new();
    for line in LinesWithEndings::from(&text) {
        let ranges: Vec<(syntect::highlighting::Style, &str)> =
            h.highlight_line(line, &ps).unwrap();

        fn convert(c: syntect::highlighting::Color) -> Colour {
            Colour {
                r: c.r,
                g: c.g,
                b: c.b,
            }
        }
        for (sty, text) in ranges {
            results.push((
                TextStyle::colours(convert(sty.foreground), convert(sty.background)),
                text,
            ));
        }
    }
    results
}

fn update_config<T: TextDecorator>(mut config: Config<T>, flags: &Flags) -> Config<T> {
    if let Some(wrap_width) = flags.wrap_width {
        config = config.max_wrap_width(wrap_width);
    }
    #[cfg(feature = "css")]
    if flags.use_css {
        config = config.use_doc_css();
    }
    #[cfg(feature = "css")]
    if !flags.agent_css.is_empty() {
        config = config.add_agent_css(&flags.agent_css).expect("Invalid CSS");
    }
    #[cfg(feature = "css_ext")]
    if flags.syntax_highlight {
        config = config
            .register_highlighter("rs", Box::new(|text| do_syntect_highlight(text, "rs")))
            .register_highlighter("html", Box::new(|text| do_syntect_highlight(text, "html")));
    }
    match (flags.link_footnotes, flags.no_link_footnotes) {
        (true, true) => {
            eprintln!("Error: can't specify both --link-footnotes and --no-link-footnotes");
            std::process::exit(1);
        }
        (true, false) => config = config.link_footnotes(true),
        (false, true) => config = config.link_footnotes(false),
        (false, false) => {}
    };
    if flags.pad_width {
        config = config.pad_block_width();
    }
    config
}

fn translate<R>(input: R, flags: Flags, literal: bool) -> String
where
    R: io::Read,
{
    #[cfg(unix)]
    {
        if flags.use_colour {
            let conf = config::rich();
            let conf = update_config(conf, &flags);
            #[cfg(feature = "css")]
            let use_css_colours = !flags.ignore_css_colours;
            #[cfg(not(feature = "css"))]
            let use_css_colours = false;
            #[cfg(feature = "css")]
            let use_only_css = flags.use_only_css;
            #[cfg(not(feature = "css"))]
            let use_only_css = false;
            return conf
                .coloured(input, flags.width, move |anns, s| {
                    default_colour_map(anns, s, use_css_colours, use_only_css)
                })
                .unwrap();
        }
    }
    #[cfg(feature = "css")]
    {
        if flags.show_css {
            let conf = config::plain();
            let conf = update_config(conf, &flags);
            let dom = conf.parse_html(input).unwrap();
            return html2text::dom_to_parsed_style(&dom).expect("Parsing CSS");
        }
    }
    if flags.show_dom {
        let conf = config::plain();
        let conf = update_config(conf, &flags);
        let dom = conf.parse_html(input).unwrap();
        dom.as_dom_string()
    } else if flags.show_render {
        let conf = config::plain();
        let conf = update_config(conf, &flags);
        let dom = conf.parse_html(input).unwrap();
        let rendertree = conf.dom_to_render_tree(&dom).unwrap();
        rendertree.to_string()
    } else if literal {
        let conf = config::with_decorator(TrivialDecorator::new());
        let conf = update_config(conf, &flags);
        conf.string_from_read(input, flags.width).unwrap()
    } else {
        let conf = config::plain();
        let conf = update_config(conf, &flags);
        conf.string_from_read(input, flags.width).unwrap()
    }
}

#[derive(Debug)]
struct Flags {
    width: usize,
    wrap_width: Option<usize>,
    #[allow(unused)]
    use_colour: bool,
    #[cfg(feature = "css")]
    use_css: bool,
    #[cfg(feature = "css")]
    ignore_css_colours: bool,
    #[cfg(feature = "css")]
    use_only_css: bool,
    show_dom: bool,
    show_render: bool,
    #[cfg(feature = "css")]
    show_css: bool,
    pad_width: bool,
    link_footnotes: bool,
    no_link_footnotes: bool,
    #[cfg(feature = "css_ext")]
    syntax_highlight: bool,
    #[cfg(feature = "css")]
    agent_css: String,
}

fn main() {
    #[cfg(feature = "html_trace")]
    env_logger::init();

    let mut infile: Option<String> = None;
    let mut outfile: Option<String> = None;
    let mut flags = Flags {
        width: 80,
        wrap_width: None,
        use_colour: false,
        #[cfg(feature = "css")]
        use_css: false,
        #[cfg(feature = "css")]
        ignore_css_colours: false,
        #[cfg(feature = "css")]
        use_only_css: false,
        show_dom: false,
        show_render: false,
        #[cfg(feature = "css")]
        show_css: false,
        #[cfg(feature = "css")]
        agent_css: Default::default(),
        pad_width: false,
        link_footnotes: false,
        no_link_footnotes: false,
        #[cfg(feature = "css_ext")]
        syntax_highlight: false,
    };
    let mut literal: bool = false;

    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut infile).add_argument(
            "infile",
            StoreOption,
            "Input HTML file (default is standard input)",
        );
        ap.refer(&mut flags.width).add_option(
            &["-w", "--width"],
            Store,
            "Column width to format to (default is 80)",
        );
        ap.refer(&mut flags.wrap_width).add_option(
            &["-W", "--wrap-width"],
            StoreOption,
            "Maximum text wrap width (default same as width)",
        );
        ap.refer(&mut outfile).add_option(
            &["-o", "--output"],
            StoreOption,
            "Output file (default is standard output)",
        );
        ap.refer(&mut literal).add_option(
            &["-L", "--literal"],
            StoreTrue,
            "Output only literal text (no decorations)",
        );
        ap.refer(&mut flags.pad_width).add_option(
            &["--pad-width"],
            StoreTrue,
            "Pad blocks to their full width",
        );
        ap.refer(&mut flags.link_footnotes).add_option(
            &["--link-footnotes"],
            StoreTrue,
            "Enable link footnotes",
        );
        ap.refer(&mut flags.no_link_footnotes).add_option(
            &["--no-link-footnotes"],
            StoreTrue,
            "Enable link footnotes",
        );
        #[cfg(unix)]
        ap.refer(&mut flags.use_colour).add_option(
            &["--colour"],
            StoreTrue,
            "Use ANSI terminal colours",
        );
        #[cfg(feature = "css")]
        ap.refer(&mut flags.use_css)
            .add_option(&["--css"], StoreTrue, "Enable CSS");
        #[cfg(feature = "css")]
        ap.refer(&mut flags.ignore_css_colours)
            .add_option(&["--ignore-css-colour"], StoreTrue, "With --css, ignore CSS colour information (still hides elements with e.g. display: none)");
        #[cfg(feature = "css")]
        ap.refer(&mut flags.use_only_css).add_option(
            &["--only-css"],
            StoreTrue,
            "Don't use default non-CSS colours",
        );
        ap.refer(&mut flags.show_dom).add_option(
            &["--show-dom"],
            StoreTrue,
            "Show the parsed HTML DOM instead of rendered output",
        );
        ap.refer(&mut flags.show_render).add_option(
            &["--show-render"],
            StoreTrue,
            "Show the computed render tree instead of the rendered output",
        );
        #[cfg(feature = "css")]
        ap.refer(&mut flags.show_css).add_option(
            &["--show-css"],
            StoreTrue,
            "Show the parsed CSS instead of rendered output",
        );
        #[cfg(feature = "css")]
        ap.refer(&mut flags.agent_css).add_option(
            &["--agent-css"],
            Store,
            "Add some CSS rules (to the agent spreadsheet)",
        );
        #[cfg(feature = "css_ext")]
        ap.refer(&mut flags.syntax_highlight).add_option(
            &["--syntax"],
            StoreTrue,
            "Enable syntax highlighting of <pre> blocks.",
        );
        ap.parse_args_or_exit();
    }

    let data = match infile {
        None => {
            let stdin = io::stdin();

            translate(&mut stdin.lock(), flags, literal)
        }
        Some(name) => {
            let mut file = std::fs::File::open(name).expect("Tried to open file");
            translate(&mut file, flags, literal)
        }
    };

    match outfile {
        None => {
            print!("{}", data);
        }
        Some(name) => {
            let mut file = std::fs::File::create(name).expect("Tried to create file");
            write!(file, "{}", data).unwrap();
        }
    };
}
