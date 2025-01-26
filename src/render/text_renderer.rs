//! Implementations of the `Renderer` trait.
//!
//! This module implements helpers and concrete types for rendering from HTML
//! into different text formats.

use crate::Colour;
use crate::WhiteSpace;

use super::Renderer;
use super::Result;
use super::TooNarrow;
use std::cell::Cell;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;
use std::vec;
use std::{collections::LinkedList, fmt::Debug};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// Context to use during tree parsing.
/// This mainly gives access to a Renderer, but needs to be able to push
/// new ones on for nested structures.
#[derive(Clone, Debug)]
pub(crate) struct TextRenderer<D: TextDecorator> {
    subrender: Vec<SubRenderer<D>>,
    links: Vec<String>,
}

impl<D: TextDecorator> Deref for TextRenderer<D> {
    type Target = SubRenderer<D>;
    fn deref(&self) -> &Self::Target {
        self.subrender.last().expect("Underflow in renderer stack")
    }
}
impl<D: TextDecorator> DerefMut for TextRenderer<D> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.subrender
            .last_mut()
            .expect("Underflow in renderer stack")
    }
}

impl<D: TextDecorator> TextRenderer<D> {
    /// Construct new stack of renderer
    pub fn new(subrenderer: SubRenderer<D>) -> TextRenderer<D> {
        TextRenderer {
            subrender: vec![subrenderer],
            links: Vec::new(),
        }
    }

    // hack overloads start_link method otherwise coming from the Renderer trait
    // impl on SubRenderer
    /// Add link to global link collection
    pub fn start_link(&mut self, target: &str) -> Result<()> {
        self.links.push(target.to_string());
        self.subrender.last_mut().unwrap().start_link(target)?;
        Ok(())
    }

    /// Push a new builder onto the stack
    pub fn push(&mut self, builder: SubRenderer<D>) {
        self.subrender.push(builder);
    }

    /// Pop off the top builder and return it.
    /// Panics if empty
    pub fn pop(&mut self) -> SubRenderer<D> {
        self.subrender
            .pop()
            .expect("Attempt to pop a subrender from empty stack")
    }

    /// Pop off the only builder and return it.
    /// panics if there aren't exactly 1 available.
    pub fn into_inner(mut self) -> (SubRenderer<D>, Vec<String>) {
        assert_eq!(self.subrender.len(), 1);
        (
            self.subrender
                .pop()
                .expect("Attempt to pop a subrenderer from an empty stack"),
            self.links,
        )
    }
}

/// A wrapper around a String with extra metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct TaggedString<T> {
    /// The wrapped text.
    pub s: String,

    /// The metadata.
    pub tag: T,
}

impl<T: Debug + PartialEq> TaggedString<T> {
    /// Returns the tagged string’s display width in columns.
    ///
    /// See [`unicode_width::UnicodeWidthStr::width`][] for more information.
    ///
    /// [`unicode_width::UnicodeWidthStr::width`]: https://docs.rs/unicode-width/latest/unicode_width/trait.UnicodeWidthStr.html
    pub fn width(&self) -> usize {
        self.s.width()
    }
}

/// An element of a line of tagged text: either a TaggedString or a
/// marker appearing in between document characters.
#[derive(Clone, Debug, PartialEq)]
pub enum TaggedLineElement<T> {
    /// A string with tag information attached.
    Str(TaggedString<T>),

    /// A zero-width marker indicating the start of a named HTML fragment.
    FragmentStart(String),
}

impl<T> TaggedLineElement<T> {
    /// Return true if this element is non-empty.
    /// FragmentStart is considered empty.
    fn has_content(&self) -> bool {
        match self {
            TaggedLineElement::Str(_) => true,
            TaggedLineElement::FragmentStart(_) => false,
        }
    }
}

/// A line of tagged text (composed of a set of `TaggedString`s).
#[derive(Debug, Clone, PartialEq)]
pub struct TaggedLine<T> {
    v: Vec<TaggedLineElement<T>>,
    len: usize,
}

impl<T: Debug + Eq + PartialEq + Clone + Default> Default for TaggedLine<T> {
    fn default() -> Self {
        TaggedLine::new()
    }
}

impl<T: Debug + Eq + PartialEq + Clone + Default> TaggedLine<T> {
    /// Create an empty `TaggedLine`.
    pub fn new() -> TaggedLine<T> {
        TaggedLine {
            v: Vec::new(),
            len: 0,
        }
    }

    /// Create a new TaggedLine from a string and tag.
    pub fn from_string(s: String, tag: &T) -> TaggedLine<T> {
        let len = UnicodeWidthStr::width(s.as_str());
        TaggedLine {
            v: vec![TaggedLineElement::Str(TaggedString {
                s,
                tag: tag.clone(),
            })],
            len,
        }
    }

    /// Join the line into a String, ignoring the tags and markers.
    #[allow(clippy::inherent_to_string)]
    fn to_string(&self) -> String {
        let mut s = String::new();
        for tle in &self.v {
            if let TaggedLineElement::Str(ts) = tle {
                s.push_str(&ts.s);
            }
        }
        s
    }

    /// Return true if the line is non-empty
    fn is_empty(&self) -> bool {
        for elt in &self.v {
            if elt.has_content() {
                return false;
            }
        }
        true
    }

    /// Add a new tagged string fragment to the line
    fn push_str(&mut self, ts: TaggedString<T>) {
        use self::TaggedLineElement::Str;

        if !ts.s.is_empty() {
            self.len += UnicodeWidthStr::width(ts.s.as_str());
            if let Some(Str(ts_prev)) = self.v.last_mut() {
                if ts_prev.tag == ts.tag {
                    ts_prev.s.push_str(&ts.s);
                    return;
                }
            }
            self.v.push(Str(ts));
        }
    }

    /// Add a new general TaggedLineElement to the line
    fn push(&mut self, tle: TaggedLineElement<T>) {
        use self::TaggedLineElement::Str;

        if let Str(ts) = tle {
            self.push_str(ts);
        } else {
            self.v.push(tle);
        }
    }

    /// Push some whitespace
    fn push_ws(&mut self, len: usize, tag: &T) {
        use self::TaggedLineElement::Str;
        self.push(Str(TaggedString {
            s: " ".repeat(len),
            tag: tag.clone(),
        }));
    }

    /// Add a new fragment to the start of the line
    fn insert_front(&mut self, ts: TaggedString<T>) {
        use self::TaggedLineElement::Str;

        self.len += UnicodeWidthStr::width(ts.s.as_str());
        self.v.insert(0, Str(ts));
    }

    /// Add text with a particular tag to self
    fn push_char(&mut self, c: char, tag: &T) {
        use self::TaggedLineElement::Str;

        self.len += UnicodeWidthChar::width(c).unwrap_or(0);
        if let Some(Str(ts_prev)) = self.v.last_mut() {
            if ts_prev.tag == *tag {
                ts_prev.s.push(c);
                return;
            }
        }
        let mut s = String::new();
        s.push(c);
        self.v.push(Str(TaggedString {
            s,
            tag: tag.clone(),
        }));
    }

    /// Drain tl and use to extend self.
    fn consume(&mut self, tl: &mut TaggedLine<T>) {
        for ts in tl.v.drain(..) {
            self.push(ts);
        }
    }

    /// Remove the contained items
    fn remove_items(&mut self) -> impl Iterator<Item = TaggedLineElement<T>> {
        self.len = 0;
        std::mem::take(&mut self.v).into_iter()
    }

    /// Iterator over the chars in this line.
    pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
        use self::TaggedLineElement::Str;

        self.v.iter().flat_map(|tle| {
            if let Str(ts) = tle {
                ts.s.chars()
            } else {
                "".chars()
            }
        })
    }

    /// Iterator over TaggedLineElements
    pub fn iter(&self) -> impl Iterator<Item = &TaggedLineElement<T>> + '_ {
        self.v.iter()
    }

    /// Iterator over the tagged strings in this line, ignoring any fragments.
    pub fn tagged_strings(&self) -> impl Iterator<Item = &TaggedString<T>> {
        self.v.iter().filter_map(|tle| match tle {
            TaggedLineElement::Str(ts) => Some(ts),
            _ => None,
        })
    }

    /// Converts the tagged line into an iterator over the tagged strings in this line, ignoring
    /// any fragments.
    fn into_tagged_strings(self) -> impl Iterator<Item = TaggedString<T>> {
        self.v.into_iter().filter_map(|tle| match tle {
            TaggedLineElement::Str(ts) => Some(ts),
            _ => None,
        })
    }

    /// Return the width of the line in cells
    fn width(&self) -> usize {
        let result = self.tagged_strings().map(TaggedString::width).sum();
        debug_assert_eq!(self.len, result);
        result
    }

    /// Pad this line to width with spaces (or if already at least this wide, do
    /// nothing).
    fn pad_to(&mut self, width: usize, tag: &T) {
        let my_width = self.width();
        if width > my_width {
            self.push_ws(width - my_width, tag);
        }
    }
}

/// A type to build up wrapped text, allowing extra metadata for
/// spans.
#[derive(Debug, Clone)]
struct WrappedBlock<T> {
    width: usize,
    text: Vec<TaggedLine<T>>,
    line: TaggedLine<T>,
    spacetag: Option<T>, // Tag for the whitespace before the current word
    word: TaggedLine<T>, // The current word (with no whitespace).
    wordlen: usize,
    wslen: usize,
    pre_wrapped: bool, // If true, we've been forced to wrap a <pre> line.
    pad_blocks: bool,
    allow_overflow: bool,
}

impl<T: Clone + Eq + Debug + Default> WrappedBlock<T> {
    pub fn new(width: usize, pad_blocks: bool, allow_overflow: bool) -> WrappedBlock<T> {
        WrappedBlock {
            width,
            text: Vec::new(),
            line: TaggedLine::new(),
            spacetag: None,
            word: TaggedLine::new(),
            wordlen: 0,
            wslen: 0,
            pre_wrapped: false,
            pad_blocks,
            allow_overflow,
        }
    }

    fn flush_word(&mut self, ws_mode: WhiteSpace) -> Result<()> {
        use self::TaggedLineElement::Str;

        /* Finish the word. */
        html_trace_quiet!(
            "flush_word: word={:?}, linelen={}",
            self.word,
            self.line.len
        );

        if !self.word.is_empty() {
            self.pre_wrapped = false;
            let space_in_line = self.width - self.line.len;
            let space_needed = self.wslen + self.wordlen;
            if space_needed <= space_in_line {
                html_trace!("Got enough space");
                if self.wslen > 0 {
                    self.line.push(Str(TaggedString {
                        s: " ".repeat(self.wslen),
                        tag: self.spacetag.take().unwrap(),
                    }));
                    self.wslen = 0;
                }

                self.line.consume(&mut self.word);
                html_trace!("linelen increased by wordlen to {}", self.line.len);
            } else {
                html_trace!("Not enough space");
                // The column position inside (whitespace + word)
                if !ws_mode.do_wrap() {
                    // We're not word-wrapping, so output any portion that still
                    // fits.
                    if self.wslen >= space_in_line {
                        // Skip the whitespace
                        self.wslen -= space_in_line;
                    } else if self.wslen > 0 {
                        self.line
                            .push_ws(self.wslen, &self.spacetag.take().unwrap());
                        self.wslen = 0;
                    }
                } else {
                    // We're word-wrapping, so discard any whitespace.
                    self.spacetag = None;
                    self.wslen = 0;
                }
                /* Start a new line */
                self.flush_line();

                if ws_mode == WhiteSpace::Pre {
                    self.pre_wrapped = true;
                }

                // Write any remaining whitespace
                while self.wslen > 0 {
                    let to_copy = self.wslen.min(self.width);
                    self.line.push_ws(to_copy, self.spacetag.as_ref().unwrap());
                    if to_copy == self.width {
                        self.flush_line();
                    }
                    self.wslen -= to_copy;
                }
                self.spacetag = None;

                // At this point, either:
                // We're word-wrapping, and at the start of the line or
                // We're preformatted, and may have some whitespace at the start of the
                // line.  In either case we just keep outputing the word directly, hard
                // wrapping if needed.
                self.flush_word_hard_wrap()?;
            }
        }
        self.wordlen = 0;
        Ok(())
    }

    // Write the current word out, hard-wrapped.  (This may originally be pre-formatted,
    // or be a word which just doesn't fit on the line.)
    fn flush_word_hard_wrap(&mut self) -> Result<()> {
        use self::TaggedLineElement::Str;

        let mut lineleft = self.width - self.line.len;
        for element in self.word.remove_items() {
            if let Str(piece) = element {
                let w = piece.width();
                let mut wpos = 0; // Width of already-copied pieces
                let mut bpos = 0; // Byte position of already-copied pieces
                                  //
                while w - wpos > lineleft {
                    let mut split_idx = 0;
                    for (idx, c) in piece.s[bpos..].char_indices() {
                        let c_w = UnicodeWidthChar::width(c).unwrap();
                        if c_w <= lineleft {
                            lineleft -= c_w;
                            wpos += c_w;
                        } else {
                            // Check if we've made no progress, for example
                            // if the first character is 2 cells wide and we
                            // only have a width of 1.
                            if idx == 0 && self.line.width() == 0 {
                                if self.allow_overflow {
                                    split_idx = c.len_utf8();
                                    wpos += c_w;
                                    break;
                                } else {
                                    return Err(TooNarrow);
                                }
                            }
                            split_idx = idx;
                            break;
                        }
                    }
                    self.line.push(Str(TaggedString {
                        s: piece.s[bpos..bpos + split_idx].into(),
                        tag: piece.tag.clone(),
                    }));
                    bpos += split_idx;
                    self.force_flush_line();
                    lineleft = self.width;
                }
                if bpos == 0 {
                    self.line.push(Str(piece));
                    lineleft -= w;
                } else if bpos < piece.s.len() {
                    self.line.push(Str(TaggedString {
                        s: piece.s[bpos..].into(),
                        tag: piece.tag,
                    }));
                    lineleft -= w.saturating_sub(wpos);
                }
            }
        }
        Ok(())
    }

    fn flush_line(&mut self) {
        if !self.line.is_empty() {
            self.force_flush_line();
        }
    }

    fn force_flush_line(&mut self) {
        let mut tmp_line = TaggedLine::new();
        mem::swap(&mut tmp_line, &mut self.line);
        if self.pad_blocks {
            let tmp_tag;
            let tag = if let Some(st) = self.spacetag.as_ref() {
                st
            } else {
                tmp_tag = Default::default();
                &tmp_tag
            };
            tmp_line.pad_to(self.width, tag);
        }
        self.text.push(tmp_line);
    }

    fn flush(&mut self) -> Result<()> {
        self.flush_word(WhiteSpace::Normal)?;
        self.flush_line();
        Ok(())
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
    pub fn into_lines(mut self) -> Result<Vec<TaggedLine<T>>> {
        self.flush()?;

        Ok(self.text)
    }

    fn add_text(
        &mut self,
        text: &str,
        ws_mode: WhiteSpace,
        main_tag: &T,
        wrap_tag: &T,
    ) -> Result<()> {
        html_trace!("WrappedBlock::add_text({}), {:?}", text, main_tag);
        // We walk character by character.
        // 1. First, build up whitespace columns in self.wslen
        //    - In normal mode self.wslen will always be 0 or 1
        //    - If wslen > 0, then self.spacetag will always be set.
        // 2. Next build up a word (non-whitespace).
        // 2a. If the word gets too long for the line
        // 2b. If we get to more whitespace, output the first whitespace and the word
        //     and continue.
        let mut tag = if self.pre_wrapped { wrap_tag } else { main_tag };
        for c in text.chars() {
            html_trace!(
                "c = {:?} word={:?} linelen={} wslen={} line={:?}",
                c,
                self.word,
                self.line.len,
                self.wslen,
                self.line
            );
            if c.is_whitespace() && self.wordlen > 0 {
                self.flush_word(ws_mode)?;
            }

            if c.is_whitespace() {
                // We're just building up whitespace.
                if ws_mode.preserve_whitespace() {
                    match c {
                        '\n' => {
                            // End of line.  We have no words here, so just finish
                            // the line.
                            self.force_flush_line();
                            self.wslen = 0;
                            self.spacetag = None;
                            self.pre_wrapped = false;
                            // Hard new line, so back to main tag.
                            tag = main_tag;
                        }
                        '\t' => {
                            let tab_stop = 8;
                            let mut pos = self.line.len + self.wslen;
                            let mut at_least_one_space = false;
                            while pos % tab_stop != 0 || !at_least_one_space {
                                if pos >= self.width {
                                    self.flush_line();
                                } else {
                                    self.line.push_char(' ', tag);
                                    pos += 1;
                                    at_least_one_space = true;
                                }
                            }
                        }
                        _ => {
                            if let Some(cwidth) = UnicodeWidthChar::width(c) {
                                if (self.line.len + self.wslen + cwidth) > self.width {
                                    // In any case we can discard whitespace we've
                                    // built up, as it will be at the end of the line.
                                    self.wslen = 0;

                                    self.flush_line();
                                    if ws_mode.do_wrap() {
                                        // We're handling wrapping, so collapse
                                        self.pre_wrapped = false;
                                    } else {
                                        // Manual wrapping, keep the space.
                                        self.wslen += cwidth;
                                        self.spacetag = Some(tag.clone());
                                        self.pre_wrapped = true;
                                    }
                                } else {
                                    self.spacetag = Some(tag.clone());
                                    self.wslen += cwidth;
                                }
                            }
                        }
                    }
                } else {
                    // If not preserving whitespace, everything is collapsed,
                    // and the line won't start with whitespace.
                    if self.line.len > 0 && self.wslen == 0 {
                        self.spacetag = Some(tag.clone());
                        self.wslen = 1;
                    }
                }
            } else {
                // Non-whitespace character: add to the current word.
                if let Some(cwidth) = UnicodeWidthChar::width(c) {
                    self.wordlen += cwidth;
                    // Special case: detect wrapping preformatted line to switch
                    // the tag.
                    if ws_mode == WhiteSpace::Pre
                        && (self.line.len + self.wslen + self.wordlen > self.width)
                    {
                        self.pre_wrapped = true;
                        tag = wrap_tag;
                    }
                    self.word.push_char(c, tag);
                }
            }
        }
        Ok(())
    }

    fn add_element(&mut self, elt: TaggedLineElement<T>) {
        self.word.push(elt);
    }

    fn text_len(&self) -> usize {
        self.text.len() + self.line.len + self.wordlen
    }

    fn is_empty(&self) -> bool {
        self.text_len() == 0
    }
}

/// Allow decorating/styling text.
///
/// Decorating refers to adding extra text around the rendered version
/// of some elements, such as surrounding emphasised text with `*` like
/// in markdown: `Some *bold* text`.  The decorations are formatted and
/// wrapped along with the rest of the rendered text.  This is suitable
/// for rendering HTML to an environment where terminal attributes are
/// not available.
///
/// In addition, instances of `TextDecorator` can also return annotations
/// of an associated type `Annotation` which will be associated with spans of
/// text.  This can be anything from `()` as for `PlainDecorator` or a more
/// featured type such as `RichAnnotation`.  The annotated spans (`TaggedLine`)
/// can be used by application code to add e.g. terminal colours or underlines.
pub trait TextDecorator {
    /// An annotation which can be added to text, and which will
    /// be attached to spans of text.
    type Annotation: Eq + PartialEq + Debug + Clone + Default;

    /// Return an annotation and rendering prefix for a link.
    fn decorate_link_start(&mut self, url: &str) -> (String, Self::Annotation);

    /// Return a suffix for after a link.
    fn decorate_link_end(&mut self) -> String;

    /// Return an annotation and rendering prefix for em
    fn decorate_em_start(&self) -> (String, Self::Annotation);

    /// Return a suffix for after an em.
    fn decorate_em_end(&self) -> String;

    /// Return an annotation and rendering prefix for strong
    fn decorate_strong_start(&self) -> (String, Self::Annotation);

    /// Return a suffix for after a strong.
    fn decorate_strong_end(&self) -> String;

    /// Return an annotation and rendering prefix for strikeout
    fn decorate_strikeout_start(&self) -> (String, Self::Annotation);

    /// Return a suffix for after a strikeout.
    fn decorate_strikeout_end(&self) -> String;

    /// Return an annotation and rendering prefix for code
    fn decorate_code_start(&self) -> (String, Self::Annotation);

    /// Return a suffix for after a code.
    fn decorate_code_end(&self) -> String;

    /// Return an annotation for the initial part of a preformatted line
    fn decorate_preformat_first(&self) -> Self::Annotation;

    /// Return an annotation for a continuation line when a preformatted
    /// line doesn't fit.
    fn decorate_preformat_cont(&self) -> Self::Annotation;

    /// Return an annotation and rendering prefix for a link.
    fn decorate_image(&mut self, src: &str, title: &str) -> (String, Self::Annotation);

    /// Return prefix string of header in specific level.
    fn header_prefix(&self, level: usize) -> String;

    /// Return prefix string of quoted block.
    fn quote_prefix(&self) -> String;

    /// Return prefix string of unordered list item.
    fn unordered_item_prefix(&self) -> String;

    /// Return prefix string of ith ordered list item.
    fn ordered_item_prefix(&self, i: i64) -> String;

    /// Return a new decorator of the same type which can be used
    /// for sub blocks.
    fn make_subblock_decorator(&self) -> Self;

    /// Return an annotation corresponding to adding colour, or none.
    fn push_colour(&mut self, _: Colour) -> Option<Self::Annotation> {
        None
    }

    /// Pop the last colour pushed if we pushed one.
    fn pop_colour(&mut self) -> bool {
        false
    }

    /// Return an annotation corresponding to adding background colour, or none.
    fn push_bgcolour(&mut self, _: Colour) -> Option<Self::Annotation> {
        None
    }

    /// Pop the last background colour pushed if we pushed one.
    fn pop_bgcolour(&mut self) -> bool {
        false
    }

    /// Return an annotation and rendering prefix for superscript text
    fn decorate_superscript_start(&self) -> (String, Self::Annotation) {
        ("^{".into(), Default::default())
    }

    /// Return a suffix for after a superscript.
    fn decorate_superscript_end(&self) -> String {
        "}".into()
    }

    /// Finish with a document, and return extra lines (eg footnotes)
    /// to add to the rendered text.
    fn finalise(&mut self, links: Vec<String>) -> Vec<TaggedLine<Self::Annotation>>;
}

/// A space on a horizontal row.
#[derive(Copy, Clone, Debug)]
enum BorderSegHoriz {
    /// Pure horizontal line
    Straight,
    /// Joined with a line above
    JoinAbove,
    /// Joins with a line below
    JoinBelow,
    /// Joins both ways
    JoinCross,
    /// Horizontal line, but separating two table cells from a row
    /// which wouldn't fit next to each other.
    StraightVert,
}

/// A dividing line between table rows which tracks intersections
/// with vertical lines.
#[derive(Clone, Debug)]
pub(crate) struct BorderHoriz<T> {
    /// The segments for the line.
    segments: Vec<BorderSegHoriz>,
    /// The tag associated with the lines
    tag: T,
}

impl<T: Clone> BorderHoriz<T> {
    /// Create a new blank border line.
    pub fn new(width: usize, tag: T) -> Self {
        BorderHoriz {
            segments: vec![BorderSegHoriz::Straight; width],
            tag,
        }
    }

    /// Create a new blank border line.
    fn new_type(width: usize, linetype: BorderSegHoriz, tag: T) -> Self {
        BorderHoriz {
            segments: vec![linetype; width],
            tag,
        }
    }

    /// Stretch the line to at least the specified width
    fn stretch_to(&mut self, width: usize) {
        use self::BorderSegHoriz::*;
        while width > self.segments.len() {
            self.segments.push(Straight);
        }
    }

    /// Make a join to a line above at the xth cell
    fn join_above(&mut self, x: usize) {
        use self::BorderSegHoriz::*;
        self.stretch_to(x + 1);
        let prev = self.segments[x];
        self.segments[x] = match prev {
            Straight | JoinAbove => JoinAbove,
            JoinBelow | JoinCross => JoinCross,
            StraightVert => StraightVert,
        }
    }

    /// Make a join to a line below at the xth cell
    fn join_below(&mut self, x: usize) {
        use self::BorderSegHoriz::*;
        self.stretch_to(x + 1);
        let prev = self.segments[x];
        self.segments[x] = match prev {
            Straight | JoinBelow => JoinBelow,
            JoinAbove | JoinCross => JoinCross,
            StraightVert => StraightVert,
        }
    }

    /// Merge a (possibly partial) border line below into this one.
    fn merge_from_below(&mut self, other: &BorderHoriz<T>, pos: usize) {
        use self::BorderSegHoriz::*;
        for (idx, seg) in other.segments.iter().enumerate() {
            match *seg {
                Straight | StraightVert => (),
                JoinAbove | JoinBelow | JoinCross => {
                    self.join_below(idx + pos);
                }
            }
        }
    }

    /// Merge a (possibly partial) border line above into this one.
    fn merge_from_above(&mut self, other: &BorderHoriz<T>, pos: usize) {
        use self::BorderSegHoriz::*;
        for (idx, seg) in other.segments.iter().enumerate() {
            match *seg {
                Straight | StraightVert => (),
                JoinAbove | JoinBelow | JoinCross => {
                    self.join_above(idx + pos);
                }
            }
        }
    }

    /// Return a string of spaces and vertical lines which would match
    /// just above this line.
    fn to_vertical_lines_above(&self) -> String {
        use self::BorderSegHoriz::*;
        self.segments
            .iter()
            .map(|seg| match *seg {
                Straight | JoinBelow | StraightVert => ' ',
                JoinAbove | JoinCross => '│',
            })
            .collect()
    }

    /// Turn into a string with drawing characters
    #[allow(clippy::inherent_to_string)]
    fn to_string(&self) -> String {
        self.segments
            .iter()
            .map(|seg| match seg {
                BorderSegHoriz::Straight => '─',
                BorderSegHoriz::StraightVert => '/',
                BorderSegHoriz::JoinAbove => '┴',
                BorderSegHoriz::JoinBelow => '┬',
                BorderSegHoriz::JoinCross => '┼',
            })
            .collect::<String>()
    }
}

/// A line, which can either be text or a line.
#[derive(Clone, Debug)]
pub(crate) enum RenderLine<T> {
    /// Some rendered text
    Text(TaggedLine<T>),
    /// A table border line
    Line(BorderHoriz<T>),
}

impl<T: PartialEq + Eq + Clone + Debug + Default> RenderLine<T> {
    /// Turn the rendered line into a String
    #[allow(clippy::inherent_to_string)]
    fn to_string(&self) -> String {
        match self {
            RenderLine::Text(tagged) => tagged.to_string(),
            RenderLine::Line(border) => border.to_string(),
        }
    }

    /// Convert into a `TaggedLine<T>`, if necessary squashing the
    /// BorderHoriz into one.
    pub fn into_tagged_line(self) -> TaggedLine<T> {
        use self::TaggedLineElement::Str;

        match self {
            RenderLine::Text(tagged) => tagged,
            RenderLine::Line(border) => {
                let mut tagged = TaggedLine::new();
                let tag = border.tag.clone();
                tagged.push(Str(TaggedString {
                    s: border.to_string(),
                    tag,
                }));
                tagged
            }
        }
    }

    /// Return whether this line has any text content
    /// Borders do not count as text.
    fn has_content(&self) -> bool {
        match self {
            RenderLine::Text(line) => !line.is_empty(),
            RenderLine::Line(_) => false,
        }
    }
}

/// A renderer which just outputs plain text with
/// annotations depending on a decorator.
#[derive(Clone)]
pub(crate) struct SubRenderer<D: TextDecorator> {
    /// Text width
    width: usize,
    /// Rendering options
    pub options: RenderOptions,
    lines: LinkedList<RenderLine<Vec<D::Annotation>>>,
    /// True at the end of a block, meaning we should add
    /// a blank line if any other text is added.
    at_block_end: bool,
    wrapping: Option<WrappedBlock<Vec<D::Annotation>>>,
    decorator: D,
    ann_stack: Vec<D::Annotation>,
    text_filter_stack: Vec<fn(&str) -> Option<String>>,
    /// The depth of `<pre>` block stacking.
    pre_depth: usize,
    /// The current stack of whitespace wrapping setting
    ws_stack: Vec<WhiteSpace>,
}

impl<D: TextDecorator> std::fmt::Debug for SubRenderer<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("SubRenderer")
            .field("width", &self.width)
            .field("lines", &self.lines)
            //.field("decorator", &self.decorator)
            .field("ann_stack", &self.ann_stack)
            .field("ws_stack", &self.ws_stack)
            .field("wrapping", &self.wrapping)
            .finish()
    }
}

/// Rendering options.
#[derive(Clone)]
#[non_exhaustive]
pub(crate) struct RenderOptions {
    /// The maximum text wrap width.  If set, paragraphs of text will only be wrapped
    /// to that width or less, though the overall width can be larger (e.g. for indented
    /// blocks or side-by-side table cells).
    pub wrap_width: Option<usize>,

    /// If true, then allow the output to be wider than specified instead of returning
    /// `Err(TooNarrow)`.
    pub allow_width_overflow: bool,

    /// Whether to always pad lines out to the full width.
    /// This may give a better output when the parent block
    /// has a background colour set.
    pub pad_block_width: bool,

    /// Raw extraction, ensures text in table cells ends up rendered together
    /// This traverses tables as if they had a single column and every cell is its own row.
    pub raw: bool,

    /// Whether to draw table borders
    pub draw_borders: bool,

    /// Whether to wrap links as normal text
    pub wrap_links: bool,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            wrap_width: Default::default(),
            allow_width_overflow: Default::default(),
            pad_block_width: Default::default(),
            raw: false,
            draw_borders: true,
            wrap_links: true,
        }
    }
}

fn get_wrapping_or_insert<'w, D: TextDecorator>(
    wrapping: &'w mut Option<WrappedBlock<Vec<D::Annotation>>>,
    options: &RenderOptions,
    width: usize,
) -> &'w mut WrappedBlock<Vec<D::Annotation>> {
    wrapping.get_or_insert_with(|| {
        let wwidth = match options.wrap_width {
            Some(ww) => ww.min(width),
            None => width,
        };
        WrappedBlock::new(
            wwidth,
            options.pad_block_width,
            options.allow_width_overflow,
        )
    })
}

impl<D: TextDecorator> SubRenderer<D> {
    /// Render links as lines
    pub fn finalise(&mut self, links: Vec<String>) -> Vec<TaggedLine<D::Annotation>> {
        self.decorator.finalise(links)
    }

    /// Construct a new empty SubRenderer.
    pub fn new(width: usize, options: RenderOptions, decorator: D) -> SubRenderer<D> {
        html_trace!("new({})", width);
        SubRenderer {
            width,
            options,
            lines: LinkedList::new(),
            at_block_end: false,
            wrapping: None,
            decorator,
            ann_stack: Vec::new(),
            ws_stack: Vec::new(),
            pre_depth: 0,
            text_filter_stack: Vec::new(),
        }
    }

    /// Flushes the current wrapped block into the lines.
    fn flush_wrapping(&mut self) -> Result<()> {
        if let Some(w) = self.wrapping.take() {
            self.lines
                .extend(w.into_lines()?.into_iter().map(RenderLine::Text))
        }
        Ok(())
    }

    /// Flush the wrapping text and border.  Only one should have
    /// anything to do.
    fn flush_all(&mut self) -> Result<()> {
        self.flush_wrapping()?;
        Ok(())
    }

    /// Consumes this renderer and return a multiline `String` with the result.
    pub fn into_string(mut self) -> Result<String> {
        let mut result = String::new();
        self.flush_wrapping()?;
        for line in &self.lines {
            result.push_str(&line.to_string());
            result.push('\n');
        }
        html_trace!("into_string({}, {:?})", self.width, result);
        Ok(result)
    }

    #[cfg(feature = "html_trace")]
    /// Returns a string of the current builder contents (for testing).
    #[allow(clippy::inherent_to_string)]
    fn to_string(&self) -> String {
        let mut result = String::new();
        for line in &self.lines {
            result += &line.to_string();
            result.push('\n');
        }
        result
    }

    /// Wrap links to width
    pub fn fmt_links(&mut self, mut links: Vec<TaggedLine<D::Annotation>>) {
        for line in links.drain(..) {
            /* Hard wrap */
            let mut pos = 0;
            let mut wrapped_line = TaggedLine::new();
            for ts in line.into_tagged_strings() {
                // FIXME: should we percent-escape?  This is probably
                // an invalid URL to start with.
                let s = ts.s.replace('\n', " ");
                let tag = vec![ts.tag];

                let width = s.width();
                if self.options.wrap_links && pos + width > self.width {
                    // split the string and start a new line
                    let mut buf = String::new();
                    for c in s.chars() {
                        let c_width = UnicodeWidthChar::width(c).unwrap_or(0);
                        if pos + c_width > self.width {
                            if !buf.is_empty() {
                                wrapped_line.push_str(TaggedString {
                                    s: buf,
                                    tag: tag.clone(),
                                });
                                buf = String::new();
                            }

                            self.lines.push_back(RenderLine::Text(wrapped_line));
                            wrapped_line = TaggedLine::new();
                            pos = 0;
                        }
                        pos += c_width;
                        buf.push(c);
                    }
                    wrapped_line.push_str(TaggedString { s: buf, tag });
                } else {
                    wrapped_line.push_str(TaggedString {
                        s: s.to_owned(),
                        tag,
                    });
                    pos += width;
                }
            }
            self.lines.push_back(RenderLine::Text(wrapped_line));
        }
    }

    /// Returns a `Vec` of `TaggedLine`s with the rendered text.
    pub fn into_lines(mut self) -> Result<LinkedList<RenderLine<Vec<D::Annotation>>>> {
        self.flush_wrapping()?;
        Ok(self.lines)
    }

    fn add_horizontal_line(&mut self, line: BorderHoriz<Vec<D::Annotation>>) -> Result<()> {
        self.flush_wrapping()?;
        self.lines.push_back(RenderLine::Line(line));
        Ok(())
    }

    pub fn width_minus(&self, prefix_len: usize, min_width: usize) -> Result<usize> {
        let new_width = self.width.saturating_sub(prefix_len);
        if new_width < min_width && !self.options.allow_width_overflow {
            return Err(TooNarrow);
        }
        Ok(new_width.max(min_width))
    }

    fn ws_mode(&self) -> WhiteSpace {
        self.ws_stack.last().cloned().unwrap_or(WhiteSpace::Normal)
    }
}

fn filter_text_strikeout(s: &str) -> Option<String> {
    let mut result = String::new();
    for c in s.chars() {
        result.push(c);
        if UnicodeWidthChar::width(c).unwrap_or(0) > 0 {
            // This is a character with width (not a combining or other character)
            // so add a strikethrough combiner.
            result.push('\u{336}');
        }
    }
    Some(result)
}

impl<D: TextDecorator> Renderer for SubRenderer<D> {
    fn add_empty_line(&mut self) -> Result<()> {
        html_trace!("add_empty_line()");
        self.flush_all()?;
        self.lines.push_back(RenderLine::Text(TaggedLine::new()));
        html_trace_quiet!("add_empty_line: at_block_end <- false");
        self.at_block_end = false;
        html_trace_quiet!("add_empty_line: new lines: {:?}", self.lines);
        Ok(())
    }

    fn new_sub_renderer(&self, width: usize) -> Result<Self> {
        let mut result = SubRenderer::new(
            width,
            self.options.clone(),
            self.decorator.make_subblock_decorator(),
        );
        // Copy the annotation stack
        result.ann_stack = self.ann_stack.clone();
        Ok(result)
    }

    fn start_block(&mut self) -> Result<()> {
        html_trace!("start_block({})", self.width);
        self.flush_all()?;
        if self.lines.iter().any(|l| l.has_content()) {
            self.add_empty_line()?;
        }
        html_trace_quiet!("start_block; at_block_end <- false");
        self.at_block_end = false;
        Ok(())
    }

    fn new_line(&mut self) -> Result<()> {
        self.flush_all()
    }

    fn new_line_hard(&mut self) -> Result<()> {
        match &self.wrapping {
            None => self.add_empty_line(),
            Some(wrapping) => {
                if wrapping.wordlen == 0 && wrapping.line.len == 0 {
                    self.add_empty_line()
                } else {
                    self.flush_all()
                }
            }
        }
    }

    fn add_horizontal_border(&mut self) -> Result<()> {
        self.flush_wrapping()?;
        self.lines.push_back(RenderLine::Line(BorderHoriz::new(
            self.width,
            self.ann_stack.clone(),
        )));
        Ok(())
    }

    fn add_horizontal_border_width(&mut self, width: usize) -> Result<()> {
        self.flush_wrapping()?;
        self.lines.push_back(RenderLine::Line(BorderHoriz::new(
            width,
            self.ann_stack.clone(),
        )));
        Ok(())
    }

    fn push_ws(&mut self, ws: WhiteSpace) {
        self.ws_stack.push(ws);
    }

    fn pop_ws(&mut self) {
        self.ws_stack.pop();
    }

    fn push_preformat(&mut self) {
        self.pre_depth += 1;
    }

    fn pop_preformat(&mut self) {
        debug_assert!(self.pre_depth > 0);
        self.pre_depth -= 1;
    }

    fn end_block(&mut self) {
        self.at_block_end = true;
    }

    fn add_inline_text(&mut self, text: &str) -> Result<()> {
        html_trace!("add_inline_text({}, {})", self.width, text);
        if !self.ws_mode().preserve_whitespace()
            && self.at_block_end
            && text.chars().all(char::is_whitespace)
        {
            // Ignore whitespace between blocks.
            return Ok(());
        }
        if self.at_block_end {
            self.start_block()?;
        }
        let mut s = None;
        // Do any filtering of the text
        for filter in &self.text_filter_stack {
            let srctext = s.as_deref().unwrap_or(text);
            if let Some(filtered) = filter(srctext) {
                s = Some(filtered);
            }
        }
        let filtered_text = s.as_deref().unwrap_or(text);
        let ws_mode = self.ws_mode();
        let wrapping = get_wrapping_or_insert::<D>(&mut self.wrapping, &self.options, self.width);
        let mut pre_tag_start;
        let mut pre_tag_cont;

        let main_tag;
        let cont_tag;
        if self.pre_depth > 0 {
            pre_tag_start = self.ann_stack.clone();
            pre_tag_cont = self.ann_stack.clone();
            pre_tag_start.push(self.decorator.decorate_preformat_first());
            pre_tag_cont.push(self.decorator.decorate_preformat_cont());
            main_tag = &pre_tag_start;
            cont_tag = &pre_tag_cont;
        } else {
            main_tag = &self.ann_stack;
            cont_tag = &self.ann_stack;
        }
        wrapping.add_text(filtered_text, ws_mode, main_tag, cont_tag)?;
        Ok(())
    }

    fn width(&self) -> usize {
        self.width
    }

    fn append_subrender<'a, I>(&mut self, other: Self, prefixes: I) -> Result<()>
    where
        I: Iterator<Item = &'a str>,
    {
        use self::TaggedLineElement::Str;

        self.flush_wrapping()?;
        let tag = self.ann_stack.clone();
        self.lines.extend(
            other
                .into_lines()?
                .into_iter()
                .zip(prefixes)
                .map(|(line, prefix)| match line {
                    RenderLine::Text(mut tline) => {
                        if !prefix.is_empty() {
                            tline.insert_front(TaggedString {
                                s: prefix.to_string(),
                                tag: tag.clone(),
                            });
                        }
                        RenderLine::Text(tline)
                    }
                    RenderLine::Line(l) => {
                        let mut tline = TaggedLine::new();
                        tline.push(Str(TaggedString {
                            s: prefix.to_string(),
                            tag: tag.clone(),
                        }));
                        tline.push(Str(TaggedString {
                            s: l.to_string(),
                            tag: tag.clone(),
                        }));
                        RenderLine::Text(tline)
                    }
                }),
        );
        Ok(())
    }

    fn append_columns_with_borders<I>(&mut self, cols: I, collapse: bool) -> Result<()>
    where
        I: IntoIterator<Item = Self>,
        Self: Sized,
    {
        use self::TaggedLineElement::Str;
        html_trace!("append_columns_with_borders(collapse={})", collapse);
        html_trace!("self=<<<\n{}>>>", self.to_string());

        self.flush_wrapping()?;

        let mut tot_width = 0;

        let mut line_sets = cols
            .into_iter()
            .map(|sub_r| {
                let width = sub_r.width;
                tot_width += width;
                html_trace!("Adding column:\n{}", sub_r.to_string());
                Ok((
                    width,
                    sub_r
                        .into_lines()?
                        .into_iter()
                        .map(|mut line| {
                            match line {
                                RenderLine::Text(ref mut tline) => {
                                    tline.pad_to(width, &self.ann_stack);
                                }
                                RenderLine::Line(ref mut border) => {
                                    border.stretch_to(width);
                                }
                            }
                            line
                        })
                        .collect(),
                ))
            })
            .collect::<Result<Vec<(usize, Vec<RenderLine<_>>)>>>()?;

        tot_width += line_sets.len().saturating_sub(1);

        let mut next_border = BorderHoriz::new(tot_width, self.ann_stack.clone());

        // Join the vertical lines to all the borders
        if let Some(RenderLine::Line(prev_border)) = self.lines.back_mut() {
            let mut pos = 0;
            html_trace!("Merging with last line:\n{}", prev_border.to_string());
            for &(w, _) in &line_sets[..line_sets.len() - 1] {
                html_trace!("pos={}, w={}", pos, w);
                prev_border.join_below(pos + w);
                next_border.join_above(pos + w);
                pos += w + 1;
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
            html_trace!("Collapsing borders.");
            /* Collapse any top border */
            let mut pos = 0;
            for &mut (w, ref mut sublines) in &mut line_sets {
                let starts_border = matches!(sublines.first(), Some(RenderLine::Line(_)));
                if starts_border {
                    html_trace!("Starts border");
                    if let &mut RenderLine::Line(ref mut prev_border) =
                        self.lines.back_mut().expect("No previous line")
                    {
                        if let RenderLine::Line(line) = sublines.remove(0) {
                            html_trace!(
                                "prev border:\n{}\n, pos={}, line:\n{}",
                                prev_border.to_string(),
                                pos,
                                line.to_string()
                            );
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
                if let Some(RenderLine::Line(line)) = sublines.last() {
                    html_trace!("Ends border");
                    next_border.merge_from_above(line, pos);
                    column_padding[col_no] = Some(line.to_vertical_lines_above());
                    sublines.pop();
                }
                pos += w + 1;
            }
        }

        let cell_height = line_sets.iter().map(|(_, v)| v.len()).max().unwrap_or(0);
        let spaces: String = (0..tot_width).map(|_| ' ').collect();
        let last_cellno = line_sets.len() - 1;
        let mut line = TaggedLine::new();
        for i in 0..cell_height {
            for (cellno, &mut (width, ref mut ls)) in line_sets.iter_mut().enumerate() {
                match ls.get_mut(i) {
                    Some(RenderLine::Text(tline)) => line.consume(tline),
                    Some(RenderLine::Line(bord)) => line.push(Str(TaggedString {
                        s: bord.to_string(),
                        tag: self.ann_stack.clone(),
                    })),
                    None => line.push(Str(TaggedString {
                        s: column_padding[cellno]
                            .clone()
                            .unwrap_or_else(|| spaces[0..width].to_string()),
                        tag: self.ann_stack.clone(),
                    })),
                }
                if cellno != last_cellno {
                    line.push_char(
                        if self.options.draw_borders {
                            '│'
                        } else {
                            ' '
                        },
                        &self.ann_stack,
                    );
                }
            }
            self.lines.push_back(RenderLine::Text(line));
            line = TaggedLine::new();
        }
        if self.options.draw_borders {
            self.lines.push_back(RenderLine::Line(next_border));
        }
        Ok(())
    }

    fn append_vert_row<I>(&mut self, cols: I) -> Result<()>
    where
        I: IntoIterator<Item = Self>,
        Self: Sized,
    {
        html_trace!("append_vert_row()");
        html_trace!("self=\n{}", self.to_string());

        self.flush_wrapping()?;

        let width = self.width();

        let mut first = true;
        for col in cols {
            if first {
                first = false;
            } else if self.options.draw_borders {
                let border = BorderHoriz::new_type(
                    width,
                    BorderSegHoriz::StraightVert,
                    self.ann_stack.clone(),
                );
                self.add_horizontal_line(border)?;
            }
            self.append_subrender(col, std::iter::repeat(""))?;
        }
        if self.options.draw_borders {
            self.add_horizontal_border()?;
        }
        Ok(())
    }

    fn empty(&self) -> bool {
        self.lines.is_empty()
            && if let Some(wrapping) = &self.wrapping {
                wrapping.is_empty()
            } else {
                true
            }
    }

    fn start_link(&mut self, target: &str) -> Result<()> {
        let (s, annotation) = self.decorator.decorate_link_start(target);
        self.ann_stack.push(annotation);
        self.add_inline_text(&s)
    }
    fn end_link(&mut self) -> Result<()> {
        let s = self.decorator.decorate_link_end();
        self.add_inline_text(&s)?;
        self.ann_stack.pop();
        Ok(())
    }
    fn start_emphasis(&mut self) -> Result<()> {
        let (s, annotation) = self.decorator.decorate_em_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s)
    }
    fn end_emphasis(&mut self) -> Result<()> {
        let s = self.decorator.decorate_em_end();
        self.add_inline_text(&s)?;
        self.ann_stack.pop();
        Ok(())
    }
    fn start_strong(&mut self) -> Result<()> {
        let (s, annotation) = self.decorator.decorate_strong_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s)
    }
    fn end_strong(&mut self) -> Result<()> {
        let s = self.decorator.decorate_strong_end();
        self.add_inline_text(&s)?;
        self.ann_stack.pop();
        Ok(())
    }
    fn start_strikeout(&mut self) -> Result<()> {
        let (s, annotation) = self.decorator.decorate_strikeout_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s)?;
        self.text_filter_stack.push(filter_text_strikeout);
        Ok(())
    }
    fn end_strikeout(&mut self) -> Result<()> {
        self.text_filter_stack
            .pop()
            .expect("end_strikeout() called without a corresponding start_strokeout()");
        let s = self.decorator.decorate_strikeout_end();
        self.add_inline_text(&s)?;
        self.ann_stack.pop();
        Ok(())
    }
    fn start_code(&mut self) -> Result<()> {
        let (s, annotation) = self.decorator.decorate_code_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s)?;
        Ok(())
    }
    fn end_code(&mut self) -> Result<()> {
        let s = self.decorator.decorate_code_end();
        self.add_inline_text(&s)?;
        self.ann_stack.pop();
        Ok(())
    }
    fn add_image(&mut self, src: &str, title: &str) -> Result<()> {
        let (s, tag) = self.decorator.decorate_image(src, title);
        self.ann_stack.push(tag);
        self.add_inline_text(&s)?;
        self.ann_stack.pop();
        Ok(())
    }

    fn header_prefix(&mut self, level: usize) -> String {
        self.decorator.header_prefix(level)
    }

    fn quote_prefix(&mut self) -> String {
        self.decorator.quote_prefix()
    }

    fn unordered_item_prefix(&mut self) -> String {
        self.decorator.unordered_item_prefix()
    }

    fn ordered_item_prefix(&mut self, i: i64) -> String {
        self.decorator.ordered_item_prefix(i)
    }

    fn record_frag_start(&mut self, fragname: &str) {
        use self::TaggedLineElement::FragmentStart;

        get_wrapping_or_insert::<D>(&mut self.wrapping, &self.options, self.width)
            .add_element(FragmentStart(fragname.to_string()));
    }

    #[allow(unused)]
    fn push_colour(&mut self, colour: Colour) {
        if let Some(ann) = self.decorator.push_colour(colour) {
            self.ann_stack.push(ann);
        }
    }

    #[allow(unused)]
    fn pop_colour(&mut self) {
        if self.decorator.pop_colour() {
            self.ann_stack.pop();
        }
    }

    fn push_bgcolour(&mut self, colour: Colour) {
        if let Some(ann) = self.decorator.push_bgcolour(colour) {
            self.ann_stack.push(ann);
        }
    }

    fn pop_bgcolour(&mut self) {
        if self.decorator.pop_bgcolour() {
            self.ann_stack.pop();
        }
    }

    fn start_superscript(&mut self) -> Result<()> {
        let (s, annotation) = self.decorator.decorate_superscript_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s)?;
        Ok(())
    }
    fn end_superscript(&mut self) -> Result<()> {
        let s = self.decorator.decorate_superscript_end();
        self.add_inline_text(&s)?;
        self.ann_stack.pop();
        Ok(())
    }
}

/// A decorator for use with `SubRenderer` which outputs plain UTF-8 text
/// with no annotations.  Markup is rendered as text characters or footnotes.
#[derive(Clone, Debug)]
pub struct PlainDecorator {
    nlinks: Rc<Cell<usize>>,
}

impl PlainDecorator {
    /// Create a new `PlainDecorator`.
    #[allow(clippy::new_without_default)]
    pub fn new() -> PlainDecorator {
        PlainDecorator {
            nlinks: Rc::new(Cell::new(0)),
        }
    }
}

impl TextDecorator for PlainDecorator {
    type Annotation = ();

    fn decorate_link_start(&mut self, _url: &str) -> (String, Self::Annotation) {
        self.nlinks.set(self.nlinks.get() + 1);
        ("[".to_string(), ())
    }

    fn decorate_link_end(&mut self) -> String {
        format!("][{}]", self.nlinks.get())
    }

    fn decorate_em_start(&self) -> (String, Self::Annotation) {
        ("*".to_string(), ())
    }

    fn decorate_em_end(&self) -> String {
        "*".to_string()
    }

    fn decorate_strong_start(&self) -> (String, Self::Annotation) {
        ("**".to_string(), ())
    }

    fn decorate_strong_end(&self) -> String {
        "**".to_string()
    }

    fn decorate_strikeout_start(&self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_strikeout_end(&self) -> String {
        "".to_string()
    }

    fn decorate_code_start(&self) -> (String, Self::Annotation) {
        ("`".to_string(), ())
    }

    fn decorate_code_end(&self) -> String {
        "`".to_string()
    }

    fn decorate_preformat_first(&self) -> Self::Annotation {}
    fn decorate_preformat_cont(&self) -> Self::Annotation {}

    fn decorate_image(&mut self, _src: &str, title: &str) -> (String, Self::Annotation) {
        (format!("[{}]", title), ())
    }

    fn header_prefix(&self, level: usize) -> String {
        "#".repeat(level) + " "
    }

    fn quote_prefix(&self) -> String {
        "> ".to_string()
    }

    fn unordered_item_prefix(&self) -> String {
        "* ".to_string()
    }

    fn ordered_item_prefix(&self, i: i64) -> String {
        format!("{}. ", i)
    }

    fn finalise(&mut self, links: Vec<String>) -> Vec<TaggedLine<()>> {
        links
            .into_iter()
            .enumerate()
            .map(|(idx, s)| TaggedLine::from_string(format!("[{}]: {}", idx + 1, s), &()))
            .collect()
    }

    fn make_subblock_decorator(&self) -> Self {
        self.clone()
    }
}

/// A decorator for use with `SubRenderer` which outputs plain UTF-8 text
/// with no annotations or markup, emitting only the literal text.
#[derive(Clone, Debug)]
pub struct TrivialDecorator {}

impl TrivialDecorator {
    /// Create a new `TrivialDecorator`.
    #[allow(clippy::new_without_default)]
    pub fn new() -> TrivialDecorator {
        TrivialDecorator {}
    }
}

impl TextDecorator for TrivialDecorator {
    type Annotation = ();

    fn decorate_link_start(&mut self, _url: &str) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_link_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_em_start(&self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_em_end(&self) -> String {
        "".to_string()
    }

    fn decorate_strong_start(&self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_strong_end(&self) -> String {
        "".to_string()
    }

    fn decorate_strikeout_start(&self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_strikeout_end(&self) -> String {
        "".to_string()
    }

    fn decorate_code_start(&self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_code_end(&self) -> String {
        "".to_string()
    }

    fn decorate_preformat_first(&self) -> Self::Annotation {}
    fn decorate_preformat_cont(&self) -> Self::Annotation {}

    fn decorate_image(&mut self, _src: &str, title: &str) -> (String, Self::Annotation) {
        // FIXME: this should surely be the alt text, not the title text
        (title.to_string(), ())
    }

    fn header_prefix(&self, _level: usize) -> String {
        "".to_string()
    }

    fn quote_prefix(&self) -> String {
        "".to_string()
    }

    fn unordered_item_prefix(&self) -> String {
        "".to_string()
    }

    fn ordered_item_prefix(&self, _i: i64) -> String {
        "".to_string()
    }

    fn finalise(&mut self, _links: Vec<String>) -> Vec<TaggedLine<()>> {
        Vec::new()
    }

    fn make_subblock_decorator(&self) -> Self {
        TrivialDecorator::new()
    }
}

/// A decorator to generate rich text (styled) rather than
/// pure text output.
#[derive(Clone, Debug)]
pub struct RichDecorator {
    // Don't output decorations around '*bold*' text.
    skip_decorations: bool,
}

/// Annotation type for "rich" text.  Text is associated with a set of
/// these.
#[derive(PartialEq, Eq, Clone, Debug, Default)]
#[non_exhaustive]
pub enum RichAnnotation {
    /// Normal text.
    #[default]
    Default,
    /// A link with the target.
    Link(String),
    /// An image with its src (this tag is attached to the title text)
    Image(String),
    /// Emphasised text, which might be rendered in bold or another colour.
    Emphasis,
    /// Strong text, which might be rendered in bold or another colour.
    Strong,
    /// Stikeout text
    Strikeout,
    /// Code
    Code,
    /// Preformatted; true if a continuation line for an overly-long line.
    Preformat(bool),
    /// Colour information
    Colour(crate::Colour),
    /// Background Colour information
    BgColour(crate::Colour),
}

impl RichDecorator {
    /// Create a new `RichDecorator` with the default settings.
    #[allow(clippy::new_without_default)]
    pub fn new() -> RichDecorator {
        RichDecorator {
            skip_decorations: false,
        }
    }

    /// Create a new `RichDecorator` which doesn't add decorations
    /// when terminal formatting can be used.
    pub fn new_undecorated() -> RichDecorator {
        RichDecorator {
            skip_decorations: true,
        }
    }
}

impl TextDecorator for RichDecorator {
    type Annotation = RichAnnotation;

    fn decorate_link_start(&mut self, url: &str) -> (String, Self::Annotation) {
        ("".to_string(), RichAnnotation::Link(url.to_string()))
    }

    fn decorate_link_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_em_start(&self) -> (String, Self::Annotation) {
        ("".to_string(), RichAnnotation::Emphasis)
    }

    fn decorate_em_end(&self) -> String {
        "".to_string()
    }

    fn decorate_strong_start(&self) -> (String, Self::Annotation) {
        if self.skip_decorations {
            ("".to_string(), RichAnnotation::Strong)
        } else {
            ("*".to_string(), RichAnnotation::Strong)
        }
    }

    fn decorate_strong_end(&self) -> String {
        if self.skip_decorations {
            "".to_string()
        } else {
            "*".to_string()
        }
    }

    fn decorate_strikeout_start(&self) -> (String, Self::Annotation) {
        ("".to_string(), RichAnnotation::Strikeout)
    }

    fn decorate_strikeout_end(&self) -> String {
        "".to_string()
    }

    fn decorate_code_start(&self) -> (String, Self::Annotation) {
        if self.skip_decorations {
            ("".to_string(), RichAnnotation::Code)
        } else {
            ("`".to_string(), RichAnnotation::Code)
        }
    }

    fn decorate_code_end(&self) -> String {
        if self.skip_decorations {
            "".to_string()
        } else {
            "`".to_string()
        }
    }

    fn decorate_preformat_first(&self) -> Self::Annotation {
        RichAnnotation::Preformat(false)
    }

    fn decorate_preformat_cont(&self) -> Self::Annotation {
        RichAnnotation::Preformat(true)
    }

    fn decorate_image(&mut self, src: &str, title: &str) -> (String, Self::Annotation) {
        (title.to_string(), RichAnnotation::Image(src.to_string()))
    }

    fn header_prefix(&self, level: usize) -> String {
        "#".repeat(level) + " "
    }

    fn quote_prefix(&self) -> String {
        "> ".to_string()
    }

    fn unordered_item_prefix(&self) -> String {
        "* ".to_string()
    }

    fn ordered_item_prefix(&self, i: i64) -> String {
        format!("{}. ", i)
    }

    fn finalise(&mut self, _links: Vec<String>) -> Vec<TaggedLine<RichAnnotation>> {
        Vec::new()
    }

    fn make_subblock_decorator(&self) -> Self {
        self.clone()
    }

    fn push_colour(&mut self, colour: Colour) -> Option<Self::Annotation> {
        Some(RichAnnotation::Colour(colour))
    }

    fn pop_colour(&mut self) -> bool {
        true
    }

    fn push_bgcolour(&mut self, colour: Colour) -> Option<Self::Annotation> {
        Some(RichAnnotation::BgColour(colour))
    }

    fn pop_bgcolour(&mut self) -> bool {
        true
    }
}
