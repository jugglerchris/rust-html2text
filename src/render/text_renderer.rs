use unicode_width::{UnicodeWidthStr,UnicodeWidthChar};
use super::Renderer;

/// State corresponding to a partially constructed line.
struct PartialLine {
    text: String,
    /// The width in character cells of the text, or
    /// current position.
    pos: usize,
}

impl PartialLine {
    pub fn new() -> PartialLine {
        PartialLine {
            text: String::new(),
            pos: 0,
        }
    }
}


/// A renderer which just outputs plain text.
pub struct TextRenderer {
    width: usize,
    lines: Vec<String>,
    /// True at the end of a block, meaning we should add
    /// a blank line if any other text is added.
    at_block_end: bool,
    partial_line: Option<PartialLine>,
}

impl TextRenderer {
    /// Construct a new empty TextRenderer.
    pub fn new(width: usize) -> TextRenderer {
        html_trace!("new({})", width);
        TextRenderer {
            width: width,
            lines: Vec::new(),
            at_block_end: false,
            partial_line: None,
        }
    }

    /// Take the partial line out of self, or create one
    /// if there wasn't one.
    fn take_partial_line(&mut self) -> PartialLine {
        if let Some(p) = self.partial_line.take() {
            p
        } else {
            PartialLine::new()
        }
    }

    pub fn add_subblock(&mut self, s: &str) {
        html_trace!("add_subblock({}, {})", self.width, s);
        self.lines.extend(s.lines().map(|l| l.into()));
    }

    fn push_line(&mut self, s: String) {
        self.lines.push(s);
    }
    fn flush_line(&mut self) {
        if let Some(s) = self.partial_line.take() {
            if s.text.len() > 0 {
                self.push_line(s.text);
            }
        }
    }

    pub fn into_string(mut self) -> String {
        self.flush_line();
        let mut result = String::new();
        for s in self.lines.into_iter() {
            result.push_str(&s);
            result.push('\n');
        }
        html_trace!("into_string({}, {:?})", self.width, result);
        result
    }

    fn into_lines(mut self) -> Vec<String> {
        self.flush_line();
        self.lines
    }
}

impl Renderer for TextRenderer {
    type Sub = TextRenderer;

    fn add_empty_line(&mut self) {
        html_trace!("add_empty_line()");
        self.flush_line();
        self.lines.push("".into());
        html_trace_quiet!("add_empty_line: at_block_end <- false");
        self.at_block_end = false;
        html_trace_quiet!("add_empty_line: new lines: {:?}", self.lines);
    }

    fn new_sub_renderer(&self, width: usize) -> Self::Sub {
        TextRenderer::new(width)
    }

    fn start_block(&mut self) {
        html_trace!("start_block({})", self.width);
        self.flush_line();
        if self.lines.len() > 0 {
            self.add_empty_line();
        }
        html_trace_quiet!("start_block; at_block_end <- false");
        self.at_block_end = false;
    }

    fn add_preformatted_block(&mut self, text: &str) {
        html_trace!("add_block({}, {})", self.width, text);
        self.start_block();
        self.add_subblock(text);
        html_trace_quiet!("add_block: at_block_end <- true");
        self.at_block_end = true;
    }

    fn end_block(&mut self) {
        self.at_block_end = true;
    }

    fn add_inline_text(&mut self, text: &str) {
        html_trace!("add_inline_text({}, {})", self.width, text);
        let mut partial = self.take_partial_line();
        for word in text.split_whitespace() {
            if self.width <= (partial.pos + 1) {
                self.push_line(partial.text);
                partial = self.take_partial_line();
            }
            let space_left = self.width - partial.pos - 1;
            let word_width = UnicodeWidthStr::width(word);
            if word_width <= space_left {
                /* It fits; no problem.  Add a space if not at the
                 * start of line.*/
                if partial.pos > 0 {
                    partial.text.push(' ');
                    partial.pos += 1;
                }
                partial.text.push_str(word);
                partial.pos += word_width;
                continue;
            }

            /* It doesn't fit.  If we're not at the start of the line,
             * then go to a new line. */
            if partial.pos > 0 {
                self.push_line(partial.text);
                partial = self.take_partial_line();
            }

            /* We're now at the start of a line. */
            if word_width > self.width {
                /* It doesn't fit at all on the line, so break it. */
                for c in word.chars() {
                    let c_width = UnicodeWidthChar::width(c).unwrap();
                    if c_width + partial.pos > self.width {
                        /* Break here */
                        self.push_line(partial.text);
                        partial = self.take_partial_line();
                    }
                    /* This might happen with really narrow spaces... */
                    assert!(c_width <= self.width);

                    partial.text.push(c);
                    partial.pos += c_width;
                }
            } else {
                partial.text.push_str(word);
                partial.pos += word_width;
            }
        }
        self.partial_line = Some(partial);
    }

    fn width(&self) -> usize {
        self.width
    }

    fn add_block_line(&mut self, line: &str)
    {
        self.add_subblock(line);
    }

    fn append_subrender<'a, I>(&mut self, other: Self::Sub,
                               prefixes: I)
                           where I:Iterator<Item=&'a str>
    {
        self.flush_line();
        self.lines.extend(other.into_lines()
                               .into_iter()
                               .zip(prefixes)
                               .map(|(line, prefix)| prefix.to_string() + &line));
    }

    fn append_columns<I>(&mut self, cols: I, separator: &str)
                           where I:IntoIterator<Item=Self::Sub> {
        self.flush_line();

        let line_sets = cols.into_iter()
                            .map(|sub_r| {
                                let width = sub_r.width;
                                (width, sub_r.into_lines()
                                             .into_iter()
                                             .map(|line| format!("{: <width$}", line, width=width))
                                             .collect())
                                 })
                            .collect::<Vec<(usize, Vec<String>)>>();

        let cell_height = line_sets.iter()
                                   .map(|&(_, ref v)| v.len())
                                   .max().unwrap_or(0);
        let spaces:String = (0..self.width).map(|_| ' ').collect();
        for i in 0..cell_height {
            let mut line = String::new();
            for (cellno, &(width, ref ls)) in line_sets.iter().enumerate() {
                let piece = ls.get(i).map(|s| s.as_str()).unwrap_or(&spaces[0..width]);
                line.push_str(piece);
                if cellno != line_sets.len()-1 {
                    line.push('|')
                }
            }
            self.add_block_line(&line);
        }
    }

    fn empty(&self) -> bool {
        self.lines.len() == 0 && self.partial_line.is_none()
    }

    fn text_len(&self) -> usize {
        let mut result = 0;
        for line in &self.lines {
            result += UnicodeWidthStr::width(line.as_str());
        }
        if let Some(ref line) = self.partial_line {
            result += line.pos;
        }
        result
    }
}