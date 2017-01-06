//! Implementations of the `Renderer` trait.
//!
//! This module implements helpers and concrete types for rendering from HTML
//! into different text formats.

use unicode_width::{UnicodeWidthStr,UnicodeWidthChar};
use super::Renderer;
use std::mem;
use std::vec;
use std::fmt::Debug;

/// A wrapper around a String with extra metadata.
#[derive(Debug)]
pub struct TaggedString<T:Debug> {
    s: String,
    tag: T,
}

/// A line of tagged text (composed of a set of `TaggedString`s).
#[derive(Debug)]
pub struct TaggedLine<T:Debug+Eq+PartialEq+Clone> {
    v: Vec<TaggedString<T>>,
}

impl<T:Debug+Eq+PartialEq+Clone+Default> TaggedLine<T> {
    /// Create an empty `TaggedLine`.
    pub fn new() -> TaggedLine<T> {
        TaggedLine {
            v: Vec::new(),
        }
    }

    /// Create a new TaggedLine from a string and tag.
    pub fn from_string(s: String, tag: &T) -> TaggedLine<T> {
        TaggedLine {
            v: vec![TaggedString{ s: s, tag: tag.clone() }],
        }
    }

    /// Join the line into a String ignoring the tags.
    pub fn into_string(self) -> String {
        let mut s = String::new();
        for ts in self.v {
            s.push_str(&ts.s);
        }
        s
    }

    /// Return true if the line is non-empty
    pub fn is_empty(&self) -> bool {
        self.v.len() == 0
    }

    /// Add a new fragment to the line
    pub fn push(&mut self, ts: TaggedString<T>) {
        if !self.v.is_empty() && self.v.last().unwrap().tag == ts.tag {
            self.v.last_mut().unwrap().s.push_str(&ts.s);
        } else {
            self.v.push(ts);
        }
    }

    /// Add a new fragment to the start of the line
    pub fn insert_front(&mut self, ts: TaggedString<T>) {
        self.v.insert(0, ts);
    }

    /// Add text with a particular tag to self
    pub fn push_char(&mut self, c: char, tag: &T) {
        if !self.v.is_empty() && self.v.last().unwrap().tag == *tag {
            self.v.last_mut().unwrap().s.push(c);
        } else {
            let mut s = String::new();
            s.push(c);
            self.v.push(TaggedString { s: s, tag: tag.clone() });
        }
    }

    /// Drain tl and use to extend self.
    pub fn consume(&mut self, tl: &mut TaggedLine<T>) {
        for ts in tl.v.drain(..) {
            self.push(ts);
        }
    }

    /// Drain the contained items
    pub fn drain_all(&mut self) -> vec::Drain<TaggedString<T>> {
        self.v.drain(..)
    }

    /// Iterator over the chars in this line.
    #[cfg_attr(feature="clippy", allow(needless_lifetimes))]
    pub fn chars<'a>(&'a self) -> Box<Iterator<Item=char>+'a> {
        Box::new(self.v.iter().flat_map(|ts| ts.s.chars()))
    }

    /// Iterator over (string, tag) pairs
    pub fn iter<'a>(&'a self) -> Box<Iterator<Item=(&str, &T)>+'a> {
        Box::new(self.v.iter().map(|ts| (ts.s.as_str(), &ts.tag)))
    }

    /// Return the width of the line in cells
    pub fn width(&self) -> usize {
        let mut result = 0;
        for ts in &self.v {
            result += UnicodeWidthStr::width(ts.s.as_str());
        }
        result
    }

    /// Pad this line to width with spaces (or if already at least this wide, do
    /// nothing).
    pub fn pad_to(&mut self, width: usize) {
        let my_width = self.width();
        if width > my_width {
            self.v.push(TaggedString{
                s: format!("{: <width$}", "", width=width-my_width),
                tag: T::default(),
            });
        }
    }
}

/// A type to build up wrapped text, allowing extra metadata for
/// spans.
#[derive(Debug)]
struct WrappedBlock<T:Clone+Eq+Debug+Default> {
    width: usize,
    text: Vec<TaggedLine<T>>,
    textlen: usize,
    line: TaggedLine<T>,
    linelen: usize,
    spacetag: Option<T>,         // Tag for the whitespace before the current word
    word: TaggedLine<T>,         // The current word (with no whitespace).
    wordlen: usize,
}

impl<T:Clone+Eq+Debug+Default> WrappedBlock<T> {
    pub fn new(width: usize) -> WrappedBlock<T> {
        WrappedBlock {
            width: width,
            text: Vec::new(),
            textlen: 0,
            line: TaggedLine::new(),
            linelen: 0,
            spacetag: None,
            word: TaggedLine::new(),
            wordlen: 0,
        }
    }

    fn flush_word(&mut self) {
        /* Finish the word. */
        html_trace_quiet!("flush_word: word={:?}, linelen={}", self.word, self.linelen);
        if !self.word.is_empty() {
            let space_in_line = self.width - self.linelen;
            let space_needed = self.wordlen +
                        if self.linelen > 0 { 1 } else { 0 }; // space
            if space_needed <= space_in_line {
                if self.linelen > 0 {
                    self.line.push(TaggedString{s: " ".into(), tag: self.spacetag.take().unwrap()});
                    self.linelen += 1;
                }
                self.line.consume(&mut self.word);
                self.linelen += self.wordlen;
            } else {
                /* Start a new line */
                self.flush_line();
                if self.wordlen <= self.width {
                    let mut new_word = TaggedLine::new();
                    mem::swap(&mut new_word, &mut self.word);
                    mem::swap(&mut self.line, &mut new_word);
                    self.linelen = self.wordlen;
                } else {
                    /* We need to split the word. */
                    let mut wordbits = self.word.drain_all();
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
                                let mut tmp_line = TaggedLine::new();
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
        self.wordlen = 0;
    }

    fn flush_line(&mut self) {
        if !self.line.is_empty() {
            let mut tmp_line = TaggedLine::new();
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
    /*
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
    */

    /// Consume self and return vector of lines including annotations.
    pub fn into_lines(mut self) -> Vec<TaggedLine<T>> {
        self.flush();

        self.text
    }

    pub fn add_text(&mut self, text: &str, tag: &T) {
        html_trace!("WrappedBlock::add_text({}), {:?}", text, tag);
        for c in text.chars() {
            if c.is_whitespace() {
                /* Whitespace is mostly ignored, except to terminate words. */
                self.flush_word();
                self.spacetag = Some(tag.clone());
            } else {
                /* Not whitespace; add to the current word. */
                self.word.push_char(c, tag);
                self.wordlen += UnicodeWidthChar::width(c).unwrap();
            }
            html_trace_quiet!("  Added char {:?}, wordlen={}", c, self.wordlen);
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
    type Annotation: Eq+PartialEq+Debug+Clone+Default;

    /// Return an annotation and rendering prefix for a link.
    fn decorate_link_start(&mut self, url: &str) -> (String, Self::Annotation);

    /// Return a suffix for after a link.
    fn decorate_link_end(&mut self) -> String;

    /// Return an annotation and rendering prefix for em
    fn decorate_em_start(&mut self) -> (String, Self::Annotation);

    /// Return a suffix for after an em.
    fn decorate_em_end(&mut self) -> String;

    /// Return an annotation and rendering prefix for a link.
    fn decorate_image(&mut self, title: &str) -> (String, Self::Annotation);

    /// Return a new decorator of the same type which can be used
    /// for sub blocks.
    fn make_subblock_decorator(&self) -> Self;

    /// Finish with a document, and return extra lines (eg footnotes)
    /// to add to the rendered text.
    fn finalise(self) -> Vec<TaggedLine<Self::Annotation>>;
}

/// A space on a horizontal row.
#[derive(Copy,Clone)]
pub enum BorderSegHoriz {
    /// Pure horizontal line
    Straight,
    /// Joined with a line above
    JoinAbove,
    /// Joins with a line below
    JoinBelow,
    /// Joins both ways
    JoinCross,
}

/// A dividing line between table rows which tracks intersections
/// with vertical lines.
pub struct BorderHoriz {
    /// The segments for the line.
    pub segments: Vec<BorderSegHoriz>,
}

impl BorderHoriz {
    /// Create a new blank border line.
    pub fn new(width: usize) -> BorderHoriz {
        BorderHoriz {
            segments: vec![BorderSegHoriz::Straight; width],
        }
    }
}

/// A renderer which just outputs plain text with
/// annotations depending on a decorator.
pub struct TextRenderer<D:TextDecorator> {
    width: usize,
    lines: Vec<TaggedLine<Vec<D::Annotation>>>,
    /// True at the end of a block, meaning we should add
    /// a blank line if any other text is added.
    at_block_end: bool,
    wrapping: Option<WrappedBlock<Vec<D::Annotation>>>,
    decorator: Option<D>,
    ann_stack: Vec<D::Annotation>,
    border_top: Option<BorderHoriz>,
    border_bottom: Option<BorderHoriz>,
}

impl<D:TextDecorator> TextRenderer<D> {
    /// Construct a new empty TextRenderer.
    pub fn new(width: usize, decorator: D) -> TextRenderer<D> {
        html_trace!("new({})", width);
        TextRenderer {
            width: width,
            lines: Vec::new(),
            at_block_end: false,
            wrapping: None,
            decorator: Some(decorator),
            ann_stack: Vec::new(),
            border_top: None,
            border_bottom: None,
        }
    }

    /// Get the current line wrapping context (and create if
    /// needed).
    fn current_text(&mut self) -> &mut WrappedBlock<Vec<D::Annotation>> {
        if self.wrapping.is_none() {
            self.wrapping = Some(WrappedBlock::new(self.width));
        }
        self.wrapping.as_mut().unwrap()
    }

    /// Add a prerendered (multiline) string with the current annotations.
    pub fn add_subblock(&mut self, s: &str) {
        html_trace!("add_subblock({}, {})", self.width, s);
        let tag = self.ann_stack.clone();
        self.lines.extend(s.lines().map(|l| {
            let mut line = TaggedLine::new();
            line.push(TaggedString{s: l.into(), tag: tag.clone()});
            line
        }));
    }

    /// Flushes the current wrapped block into the lines.
    fn flush_wrapping(&mut self) {
        if let Some(w) = self.wrapping.take() {
            self.lines.extend(w.into_lines())
        }
        if let Some(line) = self.border_bottom.take() {
            self.add_subblock(
                &line.segments
                     .into_iter()
                     .map(|seg| match seg {
                           _ => '-',
                          })
                     .collect::<String>());
        }
    }

    /// Consumes this renderer and return a multiline `String` with the result.
    pub fn into_string(self) -> String {
        let mut result = String::new();
        #[cfg(feature="html_trace")]
        let width: usize = self.width;
        for line in self.into_lines() {
            result.push_str(&line.into_string());
            result.push('\n');
        }
        html_trace!("into_string({}, {:?})", width, result);
        result
    }

    /// Returns a `Vec` of `TaggedLine`s with therendered text.
    pub fn into_lines(mut self) -> Vec<TaggedLine<Vec<D::Annotation>>> {
        self.flush_wrapping();
        // And add the links
        let mut trailer = self.decorator.take().unwrap().finalise();
        if !trailer.is_empty() {
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
                        self.lines.push(TaggedLine::from_string(tmp_s, &vec![]));
                        output.push(c);
                        pos = c_width;
                    } else {
                        output.push(c);
                        pos += c_width;
                    }
                }
                self.lines.push(TaggedLine::from_string(output, &vec![]));
            }
        }
        self.lines
    }
}

impl<D:TextDecorator> Renderer for TextRenderer<D> {
    type Sub = Self;
    fn add_empty_line(&mut self) {
        html_trace!("add_empty_line()");
        self.flush_wrapping();
        self.lines.push(TaggedLine::new());
        html_trace_quiet!("add_empty_line: at_block_end <- false");
        self.at_block_end = false;
        html_trace_quiet!("add_empty_line: new lines: {:?}", self.lines);
    }

    fn new_sub_renderer(&self, width: usize) -> Self {
        TextRenderer::new(width, self.decorator.as_ref().unwrap().make_subblock_decorator())
    }

    fn start_block(&mut self) {
        html_trace!("start_block({})", self.width);
        self.flush_wrapping();
        if !self.lines.is_empty() {
            self.add_empty_line();
        }
        html_trace_quiet!("start_block; at_block_end <- false");
        self.at_block_end = false;
    }

    fn new_line(&mut self) {
        self.flush_wrapping();
    }

    fn add_horizontal_border(&mut self) {
        self.flush_wrapping();
        if self.lines.len() == 0 {
            match self.border_top.take() {
                b@Some(_) => { self.border_top = b; },
                _ => { self.border_top = Some(BorderHoriz::new(self.width)); },
            }
        } else {
            match self.border_bottom.take() {
                b@Some(_) => { self.border_bottom = b; },
                _ => { self.border_bottom = Some(BorderHoriz::new(self.width)); },
            }
        }
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
        // ensure wrapping is set
        let _ = self.current_text();
        self.wrapping.as_mut().unwrap().add_text(text, &self.ann_stack);
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
        let tag = self.ann_stack.clone();
        self.lines.extend(other.into_lines()
                               .into_iter()
                               .zip(prefixes)
                               .map(|(mut line, prefix)| {
                                   line.insert_front(TaggedString{
                                       s: prefix.to_string(),
                                       tag: tag.clone()
                                   });
                                   line
                                }));
    }

    fn append_columns<I>(&mut self, cols: I, separator: char)
                           where I:IntoIterator<Item=Self> {
        self.flush_wrapping();

        let mut line_sets = cols.into_iter()
                            .map(|sub_r| {
                                let width = sub_r.width;
                                (width, sub_r.into_lines()
                                             .into_iter()
                                             .map(|mut line| {line.pad_to(width); line})
                                             .collect())
                                 })
                            .collect::<Vec<(usize, Vec<TaggedLine<_>>)>>();

        let cell_height = line_sets.iter()
                                   .map(|&(_, ref v)| v.len())
                                   .max().unwrap_or(0);
        let spaces:String = (0..self.width).map(|_| ' ').collect();
        let last_cellno = line_sets.len()-1;
        for i in 0..cell_height {
            let mut line = TaggedLine::new();
            for (cellno, &mut (width, ref mut ls)) in line_sets.iter_mut().enumerate() {
                if let Some(piece) = ls.get_mut(i) {
                    line.consume(piece);
                } else {
                    line.push(TaggedString {
                        s: spaces[0..width].to_string(),
                        tag: self.ann_stack.clone(),
                    });
                }
                if cellno != last_cellno {
                    line.push_char(separator, &self.ann_stack);
                }
            }
            self.lines.push(line);
        }
    }

    fn empty(&self) -> bool {
        self.lines.is_empty() && self.wrapping.is_none()
    }

    fn text_len(&self) -> usize {
        let mut result = 0;
        for line in &self.lines {
            result += line.width();
        }
        if let Some(ref w) = self.wrapping {
            result += w.text_len();
        }
        result
    }

    fn start_link(&mut self, target: &str)
    {
        if let Some((s, annotation)) = self.decorator.as_mut().map(|d| d.decorate_link_start(target)) {
            self.ann_stack.push(annotation);
            self.add_inline_text(&s);
        }
    }
    fn end_link(&mut self)
    {
        if let Some(s) = self.decorator.as_mut().map(|d| d.decorate_link_end()) {
            self.add_inline_text(&s);
            self.ann_stack.pop();
        }
    }
    fn start_emphasis(&mut self)
    {
        if let Some((s, annotation)) = self.decorator.as_mut().map(|d| d.decorate_em_start()) {
            self.ann_stack.push(annotation);
            self.add_inline_text(&s);
        }
    }
    fn end_emphasis(&mut self)
    {
        if let Some(s) = self.decorator.as_mut().map(|d| d.decorate_em_end()) {
            self.add_inline_text(&s);
            self.ann_stack.pop();
        }
    }
    fn add_image(&mut self, title: &str)
    {
        if let Some((s, tag)) = self.decorator.as_mut().map(|d| d.decorate_image(title)) {
            self.ann_stack.push(tag);
            self.add_inline_text(&s);
            self.ann_stack.pop();
        }
    }
}

/// A decorator for use with `TextRenderer` which outputs plain UTF-8 text
/// with no annotations.  Markup is rendered as text characters or footnotes.
#[derive(Clone)]
pub struct PlainDecorator {
    links: Vec<String>,
}

impl PlainDecorator {
    /// Create a new `PlainDecorator`.
    #[cfg_attr(feature="clippy", allow(new_without_default_derive))]
    pub fn new() -> PlainDecorator {
        PlainDecorator {
            links: Vec::new(),
        }
    }
}

impl TextDecorator for PlainDecorator {
    type Annotation = ();

    fn decorate_link_start(&mut self, url: &str) -> (String, Self::Annotation)
    {
        self.links.push(url.to_string());
        ("[".to_string(), ())
    }

    fn decorate_link_end(&mut self) -> String
    {
        format!("][{}]", self.links.len())
    }

    fn decorate_em_start(&mut self) -> (String, Self::Annotation)
    {
        ("*".to_string(), ())
    }

    fn decorate_em_end(&mut self) -> String
    {
        "*".to_string()
    }

    fn decorate_image(&mut self, title: &str) -> (String, Self::Annotation)
    {
        (format!("[{}]", title), ())
    }

    fn finalise(self) -> Vec<TaggedLine<()>> {
        self.links.into_iter().enumerate().map(|(idx,s)|
            TaggedLine::from_string(format!("[{}] {}", idx+1, s), &())).collect()
    }

    fn make_subblock_decorator(&self) -> Self {
        PlainDecorator::new()
    }
}

/// A decorator to generate rich text (styled) rather than
/// pure text output.
#[derive(Clone)]
pub struct RichDecorator {
}

/// Annotation type for "rich" text.  Text is associated with a set of
/// these.
#[derive(PartialEq,Eq,Clone,Debug)]
pub enum RichAnnotation {
    /// Normal text.
    Default,
    /// A link with the target.
    Link(String),
    /// An image (attached to the title text)
    Image,
    /// Emphasised text, which might be rendered in bold or another colour.
    Emphasis,
}

impl Default for RichAnnotation {
    fn default() -> Self {
        RichAnnotation::Default
    }
}

impl RichDecorator {
    /// Create a new `RichDecorator`.
    #[cfg_attr(feature="clippy", allow(new_without_default_derive))]
    pub fn new() -> RichDecorator {
        RichDecorator {
        }
    }
}

impl TextDecorator for RichDecorator {
    type Annotation = RichAnnotation;

    fn decorate_link_start(&mut self, url: &str) -> (String, Self::Annotation)
    {
        ("".to_string(), RichAnnotation::Link(url.to_string()))
    }

    fn decorate_link_end(&mut self) -> String
    {
        "".to_string()
    }

    fn decorate_em_start(&mut self) -> (String, Self::Annotation)
    {
        ("".to_string(), RichAnnotation::Emphasis)
    }

    fn decorate_em_end(&mut self) -> String
    {
        "".to_string()
    }

    fn decorate_image(&mut self, title: &str) -> (String, Self::Annotation)
    {
        (title.to_string(), RichAnnotation::Image)
    }

    fn finalise(self) -> Vec<TaggedLine<RichAnnotation>> {
        Vec::new()
    }

    fn make_subblock_decorator(&self) -> Self {
        RichDecorator::new()
    }
}