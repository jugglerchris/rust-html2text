extern crate argparse;
extern crate html2text;
use argparse::{ArgumentParser, Store, StoreOption, StoreTrue};
use html2text::config::{Config, self};
use html2text::render::text_renderer::{TextDecorator, TrivialDecorator};
use std::io;
use std::io::Write;
use log::trace;

#[cfg(unix)]
use html2text::render::text_renderer::RichAnnotation;
#[cfg(unix)]
use termion;

#[cfg(unix)]
fn default_colour_map(annotations: &[RichAnnotation], s: &str) -> String {
    use termion::color::*;
    use RichAnnotation::*;
    // Explicit CSS colours override any other colours
    let mut have_explicit_colour = false;
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
                start.push(format!("{}", Fg(Rgb(c.r, c.g, c.b))));
                finish.push(format!("{}", Fg(Reset)));
                have_explicit_colour = true;
            }
            BgColour(c) => {
                start.push(format!("{}", Bg(Rgb(c.r, c.g, c.b))));
                finish.push(format!("{}", Bg(Reset)));
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

fn update_config<T: TextDecorator>(mut config: Config<T>, flags: &Flags) -> Config<T> {
    if let Some(wrap_width) = flags.wrap_width {
        config = config.max_wrap_width(wrap_width);
    }
    #[cfg(feature = "css")]
    if flags.use_css {
        config = config.use_doc_css();
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
            return conf.coloured(input, flags.width, default_colour_map)
                    .unwrap()
        }
    }
    if literal {
        let conf = config::with_decorator(TrivialDecorator::new());
        let conf = update_config(conf, &flags);
        conf.string_from_read(input, flags.width)
            .unwrap()
    } else {
        let conf = config::plain();
        let conf = update_config(conf, &flags);
        conf.string_from_read(input, flags.width)
            .unwrap()
    }
}

struct Flags {
    width: usize,
    wrap_width: Option<usize>,
    #[allow(unused)]
    use_colour: bool,
    #[cfg(feature = "css")]
    use_css: bool,
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
        #[cfg(unix)]
        ap.refer(&mut flags.use_colour)
            .add_option(&["--colour"], StoreTrue, "Use ANSI terminal colours");
        #[cfg(feature = "css")]
        ap.refer(&mut flags.use_css)
            .add_option(&["--css"], StoreTrue, "Enable CSS");
        ap.parse_args_or_exit();
    }

    let data = match infile {
        None => {
            let stdin = io::stdin();
            let data = translate(&mut stdin.lock(), flags, literal);
            data
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
