use unicode_width::{UnicodeWidthStr,UnicodeWidthChar};
use super::Renderer;
use std::mem;
use std::fmt::Debug;

/// A wrapper around a String with extra metadata.
#[derive(Debug)]
struct TaggedString<T:Debug> {
    s: String,
    tag: T,
}


/// A type to build up wrapped text, allowing extra metadata for
/// spans.
#[derive(Debug)]
struct WrappedBlock<T:Clone+Eq+Debug> {
    width: usize,
    text: Vec<Vec<TaggedString<T>>>,
    textlen: usize,
    line: Vec<TaggedString<T>>,
    linelen: usize,
    spacetag: Option<T>,         // Tag for the whitespace before the current word
    word: Vec<TaggedString<T>>,  // The current word (with no whitespace).
    wordlen: usize,
}

impl<T:Clone+Eq+Debug> WrappedBlock<T> {
    pub fn new(width: usize) -> WrappedBlock<T> {
        WrappedBlock {
            width: width,
            text: Vec::new(),
            textlen: 0,
            line: Vec::new(),
            linelen: 0,
            spacetag: None,
            word: Vec::new(),
            wordlen: 0,
        }
    }

    fn flush_word(&mut self) {
        /* Finish the word. */
        if self.word.len() > 0 {
            let space_in_line = self.width - self.linelen;
            let space_needed = self.wordlen +
                        if self.linelen > 0 { 1 } else { 0 }; // space
            if space_needed <= space_in_line {
                if self.linelen > 0 {
                    self.line.push(TaggedString{s: " ".into(), tag: self.spacetag.take().unwrap()});
                    self.linelen += 1;
                }
                self.line.extend(self.word.drain(..));
                self.linelen += self.wordlen;
                self.wordlen = 0;
            } else {
                /* Start a new line */
                self.flush_line();
                if self.wordlen <= self.width {
                    let mut new_word = Vec::new();
                    mem::swap(&mut new_word, &mut self.word);
                    mem::swap(&mut self.line, &mut new_word);
                    self.linelen = self.wordlen;
                    self.wordlen = 0;
                } else {
                    /* We need to split the word. */
                    let mut wordbits = self.word.drain(..);
                    /* Note: there's always at least one piece */
                    let mut opt_piece = wordbits.next();
                    let mut lineleft = self.width;
                    while let Some(piece) = opt_piece.take() {
                        let w = UnicodeWidthStr::width(piece.s.as_str());
                        if w <= lineleft {
                            self.line.push(piece);
                            lineleft -= w;
                            self.linelen += w;
                            opt_piece = wordbits.next();
                        } else {
                            /* Split into two */
                            let mut split_idx = 0;
                            for (idx,c) in piece.s.char_indices() {
                                let c_w = UnicodeWidthChar::width(c).unwrap();
                                if c_w <= lineleft {
                                    lineleft -= c_w;
                                } else {
                                    split_idx = idx;
                                    break;
                                }
                            }
                            self.line.push(TaggedString{
                                s: piece.s[..split_idx].into(),
                                tag: piece.tag.clone(),
                            });
                            {
                                let mut tmp_line = Vec::new();
                                mem::swap(&mut tmp_line, &mut self.line);
                                self.text.push(tmp_line);
                            }
                            lineleft = self.width;
                            self.linelen = 0;
                            opt_piece = Some(TaggedString{
                                s: piece.s[split_idx..].into(),
                                tag: piece.tag,
                            });
                        }
                    }
                }
            }
        }
    }

    fn flush_line(&mut self) {
        if self.line.len() > 0 {
            let mut tmp_line = Vec::new();
            mem::swap(&mut tmp_line, &mut self.line);
            self.text.push(tmp_line);
            self.linelen = 0;
        }
    }

    fn flush(&mut self) {
        self.flush_word();
        self.flush_line();
    }

    /// Consume self and return a vector of lines.
    pub fn into_untagged_lines(mut self) -> Vec<String> {
        self.flush();

        let mut result = Vec::new();
        for line in self.text.into_iter() {
            let mut line_s = String::new();
            for TaggedString{ s, .. } in line.into_iter() {
                line_s.push_str(&s);
            }
            result.push(line_s);
        }
        result
    }

    pub fn add_text(&mut self, text: &str, tag: T) {
        for c in text.chars() {
            if c.is_whitespace() {
                /* Whitespace is mostly ignored, except to terminate words. */
                self.flush_word();
                self.spacetag = Some(tag.clone());
            } else {
                /* Not whitespace; add to the current word. */
                if self.word.len() > 0 && self.word[self.word.len()-1].tag == tag {
                    /* Just add a character to the last piece */
                    let last_idx = self.word.len() - 1;
                    self.word[last_idx].s.push(c);
                } else {
                    /* Starting a new word; easy. */
                    let mut s = String::new();
                    s.push(c);
                    self.word.push(TaggedString{ s: s, tag: tag.clone()});
                }
                self.wordlen += UnicodeWidthChar::width(c).unwrap();
            }
        }
    }

    pub fn text_len(&self) -> usize {
        self.textlen + self.linelen + self.wordlen
    }
}

/// Allow decorating/styling text.
pub trait TextDecorator {
    /// An annotation which can be added to text, and which will
    /// be attached to spans of text.
    type Annotation;

    /// Return an annotation and rendering prefix for a link.
    fn decorate_link_start(&mut self, url: &str) -> (String, Option<Self::Annotation>);

    /// Return a suffix for after a link.
    fn decorate_link_end(&mut self) -> String;

    /// Finish with a document, and return extra lines (eg footnotes)
    /// to add to the rendered text.
    fn finalise(self) -> Vec<String>;
}

/// A renderer which just outputs plain text.
pub struct TextRenderer<D:TextDecorator+Clone> {
    width: usize,
    lines: Vec<String>,
    /// True at the end of a block, meaning we should add
    /// a blank line if any other text is added.
    at_block_end: bool,
    wrapping: Option<WrappedBlock<()>>,
    decorator: Option<D>,
}

impl<D:TextDecorator+Clone> TextRenderer<D> {
    /// Construct a new empty TextRenderer.
    pub fn new(width: usize, decorator: D) -> TextRenderer<D> {
        html_trace!("new({})", width);
        TextRenderer {
            width: width,
            lines: Vec::new(),
            at_block_end: false,
            wrapping: None,
            decorator: Some(decorator),
        }
    }

    /// Get the current line wrapping context (and create if
    /// needed).
    fn current_text(&mut self) -> &mut WrappedBlock<()> {
        if self.wrapping.is_none() {
            self.wrapping = Some(WrappedBlock::new(self.width));
        }
        self.wrapping.as_mut().unwrap()
    }

    pub fn add_subblock(&mut self, s: &str) {
        html_trace!("add_subblock({}, {})", self.width, s);
        self.lines.extend(s.lines().map(|l| l.into()));
    }

    fn flush_wrapping(&mut self) {
        if let Some(w) = self.wrapping.take() {
            self.lines.extend(w.into_untagged_lines())
        }
    }

    pub fn into_string(self) -> String {
        let mut result = String::new();
        for s in self.into_lines() {
            result.push_str(&s);
            result.push('\n');
        }
        html_trace!("into_string({}, {:?})", self.width, result);
        result
    }

    fn into_lines(mut self) -> Vec<String> {
        self.flush_wrapping();
        // And add the links
        let mut trailer = self.decorator.take().unwrap().finalise();
        if trailer.len() > 0 {
            self.start_block();
            for line in trailer.drain((0..)) {
                /* Hard wrap */
                let mut output = String::new();
                let mut pos = 0;
                for c in line.chars() {
                    // FIXME: should we percent-escape?  This is probably
                    // an invalid URL to start with.
                    let c = match c {
                        '\n' => ' ',
                        x => x,
                    };
                    let c_width = UnicodeWidthChar::width(c).unwrap();
                    if pos + c_width > self.width {
                        let mut tmp_s = String::new();
                        mem::swap(&mut output, &mut tmp_s);
                        self.lines.push(tmp_s);
                        output.push(c);
                        pos = c_width;
                    } else {
                        output.push(c);
                        pos += c_width;
                    }
                }
                self.lines.push(output);
            }
        }
        self.lines
    }
}

impl<D:TextDecorator+Clone> Renderer for TextRenderer<D> {
    type Sub = Self;
    fn add_empty_line(&mut self) {
        html_trace!("add_empty_line()");
        self.flush_wrapping();
        self.lines.push("".into());
        html_trace_quiet!("add_empty_line: at_block_end <- false");
        self.at_block_end = false;
        html_trace_quiet!("add_empty_line: new lines: {:?}", self.lines);
    }

    fn new_sub_renderer(&self, width: usize) -> Self {
        TextRenderer::new(width, self.decorator.as_ref().unwrap().clone())
    }

    fn start_block(&mut self) {
        html_trace!("start_block({})", self.width);
        self.flush_wrapping();
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
        if self.at_block_end && text.chars().all(char::is_whitespace) {
            // Ignore whitespace between blocks.
            return;
        }
        if self.at_block_end {
            self.start_block();
        }
        let mut partial = self.current_text();
        partial.add_text(text, ());
    }

    fn width(&self) -> usize {
        self.width
    }

    fn add_block_line(&mut self, line: &str)
    {
        self.add_subblock(line);
    }

    fn append_subrender<'a, I>(&mut self, other: Self,
                               prefixes: I)
                           where I:Iterator<Item=&'a str>
    {
        self.flush_wrapping();
        self.lines.extend(other.into_lines()
                               .into_iter()
                               .zip(prefixes)
                               .map(|(line, prefix)| prefix.to_string() + &line));
    }

    fn append_columns<I>(&mut self, cols: I, separator: char)
                           where I:IntoIterator<Item=Self> {
        self.flush_wrapping();

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
                    line.push(separator)
                }
            }
            self.add_block_line(&line);
        }
    }

    fn empty(&self) -> bool {
        self.lines.len() == 0 && self.wrapping.is_none()
    }

    fn text_len(&self) -> usize {
        let mut result = 0;
        for line in &self.lines {
            result += UnicodeWidthStr::width(line.as_str());
        }
        if let Some(ref w) = self.wrapping {
            result += w.text_len();
        }
        result
    }

    fn start_link(&mut self, target: &str)
    {
        if let Some((s, _annotation)) = self.decorator.as_mut().map(|d| d.decorate_link_start(target)) {
            self.add_inline_text(&s);
        }
    }
    fn end_link(&mut self)
    {
        if let Some(s) = self.decorator.as_mut().map(|d| d.decorate_link_end()) {
            self.add_inline_text(&s);
        }
    }
}

#[derive(Clone)]
pub struct PlainDecorator {
    links: Vec<String>,
}

impl PlainDecorator {
    pub fn new() -> PlainDecorator {
        PlainDecorator {
            links: Vec::new(),
        }
    }
}

impl TextDecorator for PlainDecorator {
    type Annotation = ();

    fn decorate_link_start(&mut self, url: &str) -> (String, Option<Self::Annotation>)
    {
        self.links.push(url.to_string());
        ("[".to_string(), None)
    }

    fn decorate_link_end(&mut self) -> String
    {
        format!("][{}]", self.links.len())
    }

    fn finalise(self) -> Vec<String> {
        self.links.into_iter().enumerate().map(|(idx,s)| format!("[{}] {}", idx+1, s)).collect()
    }
}