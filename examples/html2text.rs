extern crate argparse;
extern crate html2text;
use argparse::{ArgumentParser, Store, StoreOption, StoreTrue};
use std::io;
use std::io::Write;

fn translate<R>(input: R, width: usize, literal: bool) -> String
where
    R: io::Read,
{
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
        ap.parse_args_or_exit();
    }

    let data = match infile {
        None => {
            let stdin = io::stdin();
            let data = translate(&mut stdin.lock(), width, literal);
            data
        }
        Some(name) => {
            let mut file = std::fs::File::open(name).expect("Tried to open file");
            translate(&mut file, width, literal)
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
