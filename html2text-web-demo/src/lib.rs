use wasm_bindgen::prelude::wasm_bindgen;

use ratzilla::ratatui::{
    style::{Color, Style, Stylize},
    text::{Text, Line, Span},
    widgets::{Block, Paragraph},
    Frame,
    Terminal,
};

use html2text::{
    config::ImageRenderMode,
    render::TextDecorator,
};
use ratzilla::DomBackend;

#[derive(Default)]
#[wasm_bindgen]
pub struct Config {
    css: bool,
    colour: bool,
    user_css: Option<String>,
    agent_css: Option<String>,
    pad_block_width: bool,
    wrap_width: Option<usize>,
    allow_overflow: bool,
    min_wrap_width: Option<usize>,
    raw_mode: bool,
    no_borders: bool,
    no_link_wrap: bool,
    unicode_so: bool,
    do_decorate: bool,
    link_footnotes: bool,
    image_mode: ImageRenderMode,
}

#[wasm_bindgen]
impl Config {
    pub fn new() -> Self {
        Config {
            ..Default::default()
        }
    }

    pub fn use_colour(&mut self) {
        self.colour = true;
    }

    pub fn use_css(&mut self) {
        self.css = true;
    }

    pub fn add_user_css(&mut self, css: String) {
        if css.trim().is_empty() {
            self.user_css = None;
        } else {
            self.user_css = Some(css);
        }
    }

    pub fn add_agent_css(&mut self, css: String) {
        if css.trim().is_empty() {
            self.agent_css = None;
        } else {
            self.agent_css = Some(css);
        }
    }

    pub fn pad_block_width(&mut self) {
        self.pad_block_width = true;
    }

    pub fn max_wrap_width(&mut self, width: usize) {
        self.wrap_width = Some(width);
    }

    pub fn allow_overflow(&mut self) {
        self.allow_overflow = true;
    }

    pub fn min_wrap_width(&mut self, width: usize) {
        self.min_wrap_width = Some(width);
    }
    pub fn raw_mode(&mut self) {
        self.raw_mode = true;
    }
    pub fn no_borders(&mut self) {
        self.no_borders = true;
    }
    pub fn no_link_wrap(&mut self) {
        self.no_link_wrap = true;
    }
    pub fn unicode_so(&mut self) {
        self.unicode_so = true;
    }
    pub fn do_decorate(&mut self) {
        self.do_decorate = true;
    }
    pub fn link_footnotes(&mut self, value: bool) {
        self.link_footnotes = value;
    }

    pub fn image_mode(&mut self, value: &str) {
        match value {
            "ignore" => self.image_mode = ImageRenderMode::IgnoreEmpty,
            "always" => self.image_mode = ImageRenderMode::ShowAlways,
            "replace" => self.image_mode = ImageRenderMode::Replace("XX"),
            "filename" =>  self.image_mode = ImageRenderMode::Filename,
            _ => self.image_mode = ImageRenderMode::IgnoreEmpty,
        }
    }

    fn update_conf<D: TextDecorator>(&self, conf: html2text::config::Config<D>) -> Result<html2text::config::Config<D>, String> {
        let mut conf = if self.css {
            conf.use_doc_css()
        } else {
            conf
        };
        if let Some(user_css) = &self.user_css {
            conf = conf.add_css(user_css).map_err(|e| format!("{}", e))?;
        }
        if let Some(agent_css) = &self.agent_css {
            conf = conf.add_agent_css(agent_css).map_err(|e| format!("{}", e))?;
        }
        if self.pad_block_width {
            conf = conf.pad_block_width();
        }
        if let Some(width) = self.wrap_width {
            conf = conf.max_wrap_width(width);
        }
        if self.allow_overflow {
            conf = conf.allow_width_overflow();
        }
        if let Some(width) = self.min_wrap_width {
            conf = conf.min_wrap_width(width);
        }
        if self.raw_mode {
            conf = conf.raw_mode(true);
        }
        if self.no_borders {
            conf = conf.no_table_borders();
        }
        if self.no_link_wrap {
            conf = conf.no_link_wrapping();
        }
        if self.unicode_so {
            conf = conf.unicode_strikeout(true);
        }
        if self.do_decorate {
            conf = conf.do_decorate();
        }
        conf = conf.link_footnotes(self.link_footnotes);
        if self.image_mode != ImageRenderMode::IgnoreEmpty {
            conf = conf.empty_img_mode(self.image_mode);
        }
        Ok(conf
            .unicode_strikeout(false))
    }
}

fn do_render_colour(f: &mut Frame, config: &Config, input: &[u8]) -> Result<(), String> {
    let area = f.area();

    let conf = config.update_conf(html2text::config::rich())?;

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
    Ok(())
}

#[wasm_bindgen]
pub fn format_html(config: Config, input: &str) -> Result<(), String> {
    let backend = DomBackend::new_by_id("lib").unwrap();
    let mut terminal = Terminal::new(backend).unwrap();

    let inp = input.to_string();
    terminal.draw(move |f| {
        if config.colour {
            do_render_colour(f, &config, inp.as_bytes()).unwrap();
        } else {
            let area = f.area();

            let conf = config.update_conf(html2text::config::plain()).unwrap();
            let output = conf.string_from_read(inp.as_bytes(), area.width as usize).unwrap();

            f.render_widget(
                Paragraph::new(output),
                f.area());
        }
    }).map_err(|e| format!("{e}"))?;
    Ok(())
}
