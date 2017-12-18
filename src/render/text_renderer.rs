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
        assert!(width > 0);
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
            } else if let Some(charwidth) = UnicodeWidthChar::width(c) {
                /* Not whitespace; add to the current word. */
                self.word.push_char(c, tag);
                self.wordlen += charwidth;
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

    /// Return an annotation and rendering prefix for code
    fn decorate_code_start(&mut self) -> (String, Self::Annotation);

    /// Return a suffix for after an code.
    fn decorate_code_end(&mut self) -> String;

    /// Return an annotation for the initial part of a preformatted line
    fn decorate_preformat_first(&mut self) -> Self::Annotation;

    /// Return an annotation for a continuation line when a preformatted
    /// line doesn't fit.
    fn decorate_preformat_cont(&mut self) -> Self::Annotation;

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
#[derive(Copy,Clone,Debug)]
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
#[derive(Clone,Debug)]
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

    /// Make a join to a line above at the xth cell
    pub fn join_above(&mut self, x: usize) {
        use self::BorderSegHoriz::*;
        let prev = self.segments[x];
        self.segments[x] = match prev {
            Straight | JoinAbove => JoinAbove,
            JoinBelow | JoinCross => JoinCross,
        }
    }

    /// Make a join to a line below at the xth cell
    pub fn join_below(&mut self, x: usize) {
        use self::BorderSegHoriz::*;
        let prev = self.segments[x];
        self.segments[x] = match prev {
            Straight | JoinBelow => JoinBelow,
            JoinAbove | JoinCross => JoinCross,
        }
    }

    /// Merge a (possibly partial) border line below into this one.
    pub fn merge_from_below(&mut self, other: &BorderHoriz, pos: usize) {
        use self::BorderSegHoriz::*;
        for (idx, seg) in other.segments.iter().enumerate()
        {
            match *seg {
                Straight => (),
                JoinAbove | JoinBelow | JoinCross => { self.join_below(idx+pos); },
            }
        }
    }

    /// Merge a (possibly partial) border line above into this one.
    pub fn merge_from_above(&mut self, other: &BorderHoriz, pos: usize) {
        use self::BorderSegHoriz::*;
        for (idx, seg) in other.segments.iter().enumerate()
        {
            match *seg {
                Straight => (),
                JoinAbove | JoinBelow | JoinCross => { self.join_above(idx+pos); },
            }
        }
    }

    /// Return a string of spaces and vertical lines which would match
    /// just above this line.
    pub fn to_vertical_lines_above(&self) -> String {
        use self::BorderSegHoriz::*;
        self.segments
            .iter()
            .map(|seg| match *seg {
                          Straight | JoinBelow => ' ',
                          JoinAbove | JoinCross => '│',
                      })
            .collect()
    }

    /// Turn into a string with drawing characters
    pub fn into_string(self) -> String {
        self.segments
            .into_iter()
            .map(|seg| match seg {
                BorderSegHoriz::Straight => '─',
                BorderSegHoriz::JoinAbove => '┴',
                BorderSegHoriz::JoinBelow => '┬',
                BorderSegHoriz::JoinCross => '┼',
            })
            .collect::<String>()
    }

    /// Return a string without destroying self
    pub fn to_string(&self) -> String {
        self.clone().into_string()
    }
}

/// A line, which can either be text or a line.
#[derive(Debug)]
pub enum RenderLine<T:PartialEq+Eq+Clone+Debug+Default> {
    /// Some rendered text
    Text(TaggedLine<T>),
    /// A table border line
    Line(BorderHoriz),
}

impl<T:PartialEq+Eq+Clone+Debug+Default> RenderLine<T> {
    /// Turn the rendered line into a String
    pub fn into_string(self) -> String {
        match self {
            RenderLine::Text(tagged) => tagged.into_string(),
            RenderLine::Line(border) => border.into_string(),
        }
    }

    /// Convert into a `TaggedLine<T>`, if necessary squashing the
    /// BorderHoriz into one.
    pub fn into_tagged_line(self) -> TaggedLine<T> {
        match self {
            RenderLine::Text(tagged) => tagged,
            RenderLine::Line(border) => {
                let mut tagged = TaggedLine::new();
                tagged.push(TaggedString {
                    s: border.into_string(),
                    tag: T::default()
                });
                tagged
            }
        }
    }
}

/// A renderer which just outputs plain text with
/// annotations depending on a decorator.
pub struct TextRenderer<D:TextDecorator> {
    width: usize,
    lines: Vec<RenderLine<Vec<D::Annotation>>>,
    /// True at the end of a block, meaning we should add
    /// a blank line if any other text is added.
    at_block_end: bool,
    wrapping: Option<WrappedBlock<Vec<D::Annotation>>>,
    decorator: Option<D>,
    ann_stack: Vec<D::Annotation>,
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
            RenderLine::Text(line)
        }));
    }

    /// Flushes the current wrapped block into the lines.
    fn flush_wrapping(&mut self) {
        if let Some(w) = self.wrapping.take() {
            self.lines.extend(w.into_lines().into_iter().map(RenderLine::Text))
        }
    }

    /// Flush the wrapping text and border.  Only one should have
    /// anything to do.
    fn flush_all(&mut self) {
        self.flush_wrapping();
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
    pub fn into_lines(mut self) -> Vec<RenderLine<Vec<D::Annotation>>> {
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
                        self.lines.push(RenderLine::Text(TaggedLine::from_string(tmp_s, &vec![])));
                        output.push(c);
                        pos = c_width;
                    } else {
                        output.push(c);
                        pos += c_width;
                    }
                }
                self.lines.push(RenderLine::Text(TaggedLine::from_string(output, &vec![])));
            }
        }
        self.lines
    }
}

impl<D:TextDecorator> Renderer for TextRenderer<D> {
    type Sub = Self;
    fn add_empty_line(&mut self) {
        html_trace!("add_empty_line()");
        self.flush_all();
        self.lines.push(RenderLine::Text(TaggedLine::new()));
        html_trace_quiet!("add_empty_line: at_block_end <- false");
        self.at_block_end = false;
        html_trace_quiet!("add_empty_line: new lines: {:?}", self.lines);
    }

    fn new_sub_renderer(&self, width: usize) -> Self {
        assert!(width > 0);
        TextRenderer::new(width, self.decorator.as_ref().unwrap().make_subblock_decorator())
    }

    fn start_block(&mut self) {
        html_trace!("start_block({})", self.width);
        self.flush_all();
        if !self.lines.is_empty() {
            self.add_empty_line();
        }
        html_trace_quiet!("start_block; at_block_end <- false");
        self.at_block_end = false;
    }

    fn new_line(&mut self) {
        self.flush_all();
    }

    fn add_horizontal_border(&mut self) {
        self.flush_wrapping();
        self.lines.push(RenderLine::Line(BorderHoriz::new(self.width)));
    }

    fn add_preformatted_block(&mut self, text: &str) {
        html_trace!("add_block({}, {})", self.width, text);

        // Get the tags ready for normal and continuation lines.
        let mut tag_first = self.ann_stack.clone();
        let mut tag_cont = self.ann_stack.clone();
        tag_first.push(self.decorator.as_mut().unwrap().decorate_preformat_first());
        tag_cont.push(self.decorator.as_mut().unwrap().decorate_preformat_cont());
        // Drop mutability
        let tag_first = tag_first;
        let tag_cont = tag_cont;

        let width = self.width;
        self.start_block();

        /* We do actually want to wrap, but just a hard wrapping
         * at the end, and add a "continuation line" tag so that the
         * UI can show them differently.
         */
        for formatted_line in text.lines() {
            let mut acc = String::new();
            let mut cur_width = 0;
            let mut first = true;
            for c in formatted_line.chars() {
                if let Some(char_width) = UnicodeWidthChar::width(c) {
                    if cur_width + char_width > width {
                        let mut line = TaggedLine::new();
                        /* Push what we have */
                        line.push(TaggedString {
                            s: acc,
                            tag: if first { tag_first.clone() } else { tag_cont.clone() },
                        });
                        self.lines.push(RenderLine::Text(line));
                        acc = String::new();
                        cur_width = 0;
                        first = false;
                    }
                    acc.push(c);
                    cur_width += char_width;
                } else {
                    match c {
                        '\t' => {
                            let tab_stop = 8;
                            let wanted_pos = cur_width + tab_stop - (cur_width % tab_stop);
                            let spaces = if wanted_pos > width {
                                    width - cur_width
                                } else {
                                    wanted_pos - cur_width
                                };
                            acc.extend((0..spaces).map(|_| ' '));
                            cur_width += spaces;
                        },
                        _ => (),
                    }
                }
            }
            if acc.len() > 0 {
                let mut line = TaggedLine::new();
                /* Push what we have */
                line.push(TaggedString {
                    s: acc,
                    tag: if first { tag_first.clone() } else { tag_cont.clone() },
                });
                self.lines.push(RenderLine::Text(line));
            }
        }

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
                               .map(|(line, prefix)| {
                                   match line {
                                       RenderLine::Text(mut tline) => {
                                           tline.insert_front(TaggedString{
                                               s: prefix.to_string(),
                                               tag: tag.clone()
                                           });
                                           RenderLine::Text(tline)
                                       },
                                       RenderLine::Line(l) => {
                                           let mut tline = TaggedLine::new();
                                           tline.push(TaggedString {
                                               s: prefix.to_string(),
                                               tag: tag.clone()
                                           });
                                           tline.push(TaggedString {
                                               s: l.into_string(),
                                               tag: tag.clone()
                                           });
                                           RenderLine::Text(tline)
                                       }
                                   }
                                }));
    }

    fn append_columns_with_borders<I>(&mut self, cols: I, collapse: bool)
                           where I:IntoIterator<Item=Self> {
        self.flush_wrapping();

        let mut next_border = BorderHoriz::new(self.width);

        let mut line_sets = cols.into_iter()
                            .map(|sub_r| {
                                let width = sub_r.width;
                                (width, sub_r.into_lines()
                                             .into_iter()
                                             .map(|mut line| {
                                                 match line {
                                                     RenderLine::Text(ref mut tline) => {
                                                         tline.pad_to(width);
                                                     },
                                                     RenderLine::Line(_) => {},
                                                 }
                                                 line})
                                             .collect())
                                 })
                            .collect::<Vec<(usize, Vec<RenderLine<_>>)>>();

        // Join the vertical lines to all the borders
        {
            let mut pos = 0;
            if let &mut RenderLine::Line(ref mut prev_border) = self.lines.last_mut().unwrap() {
                for &(w, _) in &line_sets[..line_sets.len()-1] {
                    prev_border.join_below(pos+w);
                    next_border.join_above(pos+w);
                    pos += w + 1;
                }
            } else {
                panic!("Expected a border line");
            }
        }

        // If we're collapsing bottom borders, then the bottom border of a
        // nested table is being merged into the bottom border of the
        // containing cell.  If that cell happens not to be the tallest
        // cell in the row, then we need to extend any vertical lines
        // to the bottom.  We'll remember what to do when we update the
        // containing border.
        let mut column_padding = vec![None; line_sets.len()];

        // If we're collapsing borders, do so.
        if collapse {
            /* Collapse any top border */
            let mut pos = 0;
            for &mut (w, ref mut sublines) in &mut line_sets {
                let starts_border = if sublines.len() > 0 {
                    if let RenderLine::Line(_) = sublines[0] {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                if starts_border {
                    if let &mut RenderLine::Line(ref mut prev_border) = self.lines.last_mut().expect("No previous line") {
                        if let RenderLine::Line(line) = sublines.remove(0) {
                            prev_border.merge_from_below(&line, pos);
                        }
                    } else {
                        unreachable!();
                    }
                }
                pos += w + 1;
            }

            /* Collapse any bottom border */
            let mut pos = 0;
            for (col_no, &mut (w, ref mut sublines)) in line_sets.iter_mut().enumerate() {
                let ends_border = if sublines.len() > 0 {
                    if let Some(&RenderLine::Line(_)) = sublines.last() {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                if ends_border {
                    if let RenderLine::Line(line) = sublines.pop().unwrap() {
                        next_border.merge_from_above(&line, pos);
                        column_padding[col_no] = Some(line.to_vertical_lines_above())
                    }
                }
                pos += w + 1;
            }
        }

        let cell_height = line_sets.iter()
                                   .map(|&(_, ref v)| v.len())
                                   .max().unwrap_or(0);
        let spaces: String = (0..self.width).map(|_| ' ').collect();
        let last_cellno = line_sets.len()-1;
        for i in 0..cell_height {
            let mut line = TaggedLine::new();
            for (cellno, &mut (width, ref mut ls)) in line_sets.iter_mut().enumerate() {
                if let Some(piece) = ls.get_mut(i) {
                    match piece {
                        &mut RenderLine::Text(ref mut tline) => {
                            line.consume(tline);
                        },
                        &mut RenderLine::Line(ref bord) => {
                            line.push(TaggedString {
                                s: bord.to_string(),
                                tag: self.ann_stack.clone(),
                            });
                        },
                    };
                } else {
                    line.push(TaggedString {
                        s: column_padding[cellno].as_ref().map(|s| s.clone())
                                                 .unwrap_or_else(||spaces[0..width].to_string()),

                        tag: self.ann_stack.clone(),
                    });
                }
                if cellno != last_cellno {
                    line.push_char('│', &self.ann_stack);
                }
            }
            self.lines.push(RenderLine::Text(line));
        }
        self.lines.push(RenderLine::Line(next_border));
    }

    fn empty(&self) -> bool {
        self.lines.is_empty() && self.wrapping.is_none()
    }

    fn text_len(&self) -> usize {
        let mut result = 0;
        for line in &self.lines {
            result += match *line {
                RenderLine::Text(ref tline) => tline.width(),
                RenderLine::Line(_) => 0, // FIXME: should borders count?
            };
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
    fn start_code(&mut self)
    {
        if let Some((s, annotation)) = self.decorator.as_mut().map(|d| d.decorate_code_start()) {
            self.ann_stack.push(annotation);
            self.add_inline_text(&s);
        }
    }
    fn end_code(&mut self)
    {
        if let Some(s) = self.decorator.as_mut().map(|d| d.decorate_code_end()) {
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

    fn decorate_code_start(&mut self) -> (String, Self::Annotation)
    {
        ("`".to_string(), ())
    }

    fn decorate_code_end(&mut self) -> String
    {
        "`".to_string()
    }

    fn decorate_preformat_first(&mut self) -> Self::Annotation { () }
    fn decorate_preformat_cont(&mut self) -> Self::Annotation { () }

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
    /// Code
    Code,
    /// Preformatted; true if a continuation line for an overly-long line.
    Preformat(bool),
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

    fn decorate_code_start(&mut self) -> (String, Self::Annotation)
    {
        ("`".to_string(), RichAnnotation::Code)
    }

    fn decorate_code_end(&mut self) -> String
    {
        "`".to_string()
    }

    fn decorate_preformat_first(&mut self) -> Self::Annotation
    {
        RichAnnotation::Preformat(false)
    }

    fn decorate_preformat_cont(&mut self) -> Self::Annotation
    {
        RichAnnotation::Preformat(true)
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