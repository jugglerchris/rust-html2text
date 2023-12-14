extern crate argparse;
extern crate html2text;
use argparse::{ArgumentParser, Store, StoreOption, StoreTrue};
use std::io;
use std::io::Write;

#[cfg(feature = "ansi_colours")]
use html2text::render::text_renderer::RichAnnotation;
#[cfg(feature = "ansi_colours")]
use termion;

#[cfg(feature = "ansi_colours")]
fn default_colour_map(annotations: &[RichAnnotation], s: &str) -> String {
    use termion::color::*;
    use RichAnnotation::*;
    // Explicit CSS colours override any other colours
    let mut have_explicit_colour = false;
    let mut start = Vec::new();
    let mut finish = Vec::new();
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
    result
}

fn translate<R>(input: R, width: usize, literal: bool, _use_colour: bool) -> String
where
    R: io::Read,
{
    #[cfg(feature = "ansi_colours")]
    {
        if _use_colour {
            return html2text::from_read_coloured(input, width, default_colour_map).unwrap();
        };
    }
    if literal {
        let decorator = html2text::render::text_renderer::TrivialDecorator::new();
        html2text::from_read_with_decorator(input, width, decorator)
    } else {
        html2text::from_read(input, width)
    }
}

fn main() {
    let mut infile: Option<String> = None;
    let mut outfile: Option<String> = None;
    let mut width: usize = 80;
    let mut literal: bool = false;
    #[allow(unused)]
    let mut use_colour = false;

    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut infile).add_argument(
            "infile",
            StoreOption,
            "Input HTML file (default is standard input)",
        );
        ap.refer(&mut width).add_option(
            &["-w", "--width"],
            Store,
            "Column width to format to (default is 80)",
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
        #[cfg(feature = "ansi_colours")]
        ap.refer(&mut use_colour)
            .add_option(&["--colour"], StoreTrue, "Use ANSI terminal colours");
        ap.parse_args_or_exit();
    }

    let data = match infile {
        None => {
            let stdin = io::stdin();
            let data = translate(&mut stdin.lock(), width, literal, use_colour);
            data
        }
        Some(name) => {
            let mut file = std::fs::File::open(name).expect("Tried to open file");
            translate(&mut file, width, literal, use_colour)
        }
    };

    match outfile {
        None => {
            println!("{}", data);
        }
        Some(name) => {
            let mut file = std::fs::File::create(name).expect("Tried to create file");
            write!(file, "{}", data).unwrap();
        }
    };
}
