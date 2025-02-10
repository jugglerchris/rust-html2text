use wasm_bindgen::prelude::wasm_bindgen;

use ratzilla::ratatui::{
    style::Color,
    widgets::{Block, Paragraph},
    Terminal,
};
use ratzilla::{DomBackend, WebRenderer};

#[wasm_bindgen]
pub fn format_html(input: &str) {
    let backend = DomBackend::new_by_id("lib").unwrap();
    let terminal = Terminal::new(backend).unwrap();

    let mut inp = input.to_string();
    terminal.draw_web(move |f| {
        let area = f.area();
        let output = html2text::from_read(inp.as_bytes(), area.width as usize).unwrap();
        f.render_widget(
            Paragraph::new(output)
            .block(
                Block::bordered().title("Foo")
                .border_style(Color::Yellow)
            ),
            f.area());
    });
}
