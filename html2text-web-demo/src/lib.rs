use wasm_bindgen::prelude::wasm_bindgen;

use ratzilla::ratatui::{
    style::{Color, Style, Stylize},
    text::{Text, Line, Span},
    widgets::{Block, Paragraph},
    Frame,
    Terminal,
};

use html2text::render::TextDecorator;
use ratzilla::{DomBackend, WebRenderer};

#[wasm_bindgen]
pub struct Config {
    css: bool,
    colour: bool,
}

#[wasm_bindgen]
impl Config {
    pub fn new() -> Self {
        Config {
            css: false,
            colour: false,
        }
    }

    pub fn use_colour(&mut self) {
        self.colour = true;
    }

    pub fn use_css(&mut self) {
        self.css = true;
    }

    fn update_conf<D: TextDecorator>(&self, conf: html2text::config::Config<D>) -> html2text::config::Config<D> {
        let conf = if self.css {
            conf.use_doc_css()
        } else {
            conf
        };
        conf
            .unicode_strikeout(false)
    }
}

fn do_render_colour(f: &mut Frame, config: &Config, input: &[u8]) {
    let area = f.area();

    let conf = config.update_conf(html2text::config::rich());

    let lines = conf.lines_from_read(input, area.width as usize - 2).unwrap();
    let mut out = Text::default();
    for line in lines {
        let mut term_line = Line::default();
        for piece in line.tagged_strings() {
            let span = Span::from(dbg!(piece.s.clone()));
            let mut style = Style::new();
            for attr in &piece.tag {
                use html2text::render::RichAnnotation::*;
                match attr {
                    Default | Link(_) | Image(_) | Code | Preformat(_) => {}
                    Emphasis => {
                        style = style.italic();
                    }
                    Strong => {
                        style = style.bold();
                    }
                    Strikeout => {
                        style = style.crossed_out();
                    }
                    Colour(col) => {
                        style = style.fg(Color::Rgb(col.r, col.g, col.b));
                    }
                    BgColour(col) => {
                        style = style.bg(Color::Rgb(col.r, col.g, col.b));
                    }
                    _ => {}
                }
            }
            term_line.push_span(span.style(style));
        }
        out.push_line(term_line);
    }
    f.render_widget(
        Paragraph::new(out).block(Block::bordered().title("HTML").border_style(Color::Yellow)),
        f.area());
}

#[wasm_bindgen]
pub fn format_html(config: Config, input: &str) {
    let backend = DomBackend::new_by_id("lib").unwrap();
    let mut terminal = Terminal::new(backend).unwrap();

    let inp = input.to_string();
    terminal.draw(move |f| {
        if config.colour {
            do_render_colour(f, &config, inp.as_bytes());
        } else {
            let area = f.area();

            let conf = config.update_conf(html2text::config::plain());
            let output = conf.string_from_read(inp.as_bytes(), area.width as usize).unwrap();

            f.render_widget(
                Paragraph::new(output),
                f.area());
        }
    });
}
