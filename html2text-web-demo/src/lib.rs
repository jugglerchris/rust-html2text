use std::io;
use wasm_bindgen::prelude::wasm_bindgen;

use ratzilla::ratatui::{
    style::Color,
    widgets::{Block, Paragraph},
    Terminal,
};
use ratzilla::{DomBackend, WebRenderer};

#[wasm_bindgen(start)]
fn run() {
    let backend = DomBackend::new().unwrap();
    let terminal = Terminal::new(backend).unwrap();

    terminal.draw_web(move |f| {
        f.render_widget(
            Paragraph::new(format!("Hello, world!"))
            .block(
                Block::bordered().title("Hi")
                .border_style(Color::Yellow)
            ),
            f.area());
    });
}

#[wasm_bindgen]
pub fn format_html(input: &str) {
    let backend = DomBackend::new().unwrap();
    let terminal = Terminal::new(backend).unwrap();

    let input_s = input.to_string();
    terminal.draw_web(move |f| {
        f.render_widget(
            Paragraph::new(input_s.clone())
            .block(
                Block::bordered().title("Hi")
                .border_style(Color::Yellow)
            ),
            f.area());
    });
}
