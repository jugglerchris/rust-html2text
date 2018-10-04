extern crate html2text;
extern crate argparse;
use std::io;
use std::io::Write;
use argparse::{ArgumentParser, Store, StoreOption};


fn main() {
    let mut infile : Option<String> = None;
    let mut outfile : Option<String> = None;
    let mut width : usize = 80;

    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut infile)
            .add_argument("infile", StoreOption, "Input HTML file (default is standard input)");
        ap.refer(&mut width)
            .add_option(&["-w", "--width"], Store, "Column width to format to (default is 80)");
        ap.refer(&mut outfile)
            .add_option(&["-o", "--output"], StoreOption, "Output file (default is standard output)");
        ap.parse_args_or_exit();
    }

    let data = match infile {
        None => {
            let stdin = io::stdin();
            let data = html2text::from_read(&mut stdin.lock(), width);
            data
        },
        Some(name) => {
            let mut file = std::fs::File::open(name).expect("Tried to open file");
            html2text::from_read(&mut file, width)
        },
    };

    match outfile {
        None => {
            println!("{}", data);
        },
        Some(name) => {
            let mut file = std::fs::File::create(name).expect("Tried to create file");
            write!(file, "{}", data).unwrap();
        },
    };
}
