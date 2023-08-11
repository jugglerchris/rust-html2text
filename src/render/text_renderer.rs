//! Implementations of the `Renderer` trait.
//!
//! This module implements helpers and concrete types for rendering from HTML
//! into different text formats.

use super::Renderer;
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
pub struct TextRenderer<D: TextDecorator> {
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
    pub fn start_link(&mut self, target: &str) {
        self.links.push(target.to_string());
        self.subrender.last_mut().unwrap().start_link(target);
    }

    /// Push a new builder onto the stack
    pub fn push(&mut self, builder: SubRenderer<D>) {
        self.subrender.push(builder);
    }

    /// Pop off the top builder and return it.
    /// Panics if empty
    pub fn pop(&mut self) -> SubRenderer<D> {
        self.subrender.pop().unwrap()
    }

    /// Pop off the only builder and return it.
    /// panics if there aren't exactly 1 available.
    pub fn into_inner(mut self) -> (SubRenderer<D>, Vec<String>) {
        assert_eq!(self.subrender.len(), 1);
        (self.subrender.pop().unwrap(), self.links)
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

/// A line of tagged text (composed of a set of `TaggedString`s).
#[derive(Debug, Clone, PartialEq)]
pub struct TaggedLine<T> {
    v: Vec<TaggedLineElement<T>>,
}

impl<T: Debug + Eq + PartialEq + Clone + Default> TaggedLine<T> {
    /// Create an empty `TaggedLine`.
    pub fn new() -> TaggedLine<T> {
        TaggedLine { v: Vec::new() }
    }

    /// Create a new TaggedLine from a string and tag.
    pub fn from_string(s: String, tag: &T) -> TaggedLine<T> {
        TaggedLine {
            v: vec![TaggedLineElement::Str(TaggedString {
                s: s,
                tag: tag.clone(),
            })],
        }
    }

    /// Join the line into a String, ignoring the tags and markers.
    pub fn into_string(self) -> String {
        let mut s = String::new();
        for tle in self.v {
            if let TaggedLineElement::Str(ts) = tle {
                s.push_str(&ts.s);
            }
        }
        s
    }

    /// Return true if the line is non-empty
    pub fn is_empty(&self) -> bool {
        self.v.len() == 0
    }

    /// Add a new tagged string fragment to the line
    pub fn push_str(&mut self, ts: TaggedString<T>) {
        use self::TaggedLineElement::Str;

        if !self.v.is_empty() {
            if let Str(ref mut ts_prev) = self.v.last_mut().unwrap() {
                if ts_prev.tag == ts.tag {
                    ts_prev.s.push_str(&ts.s);
                    return;
                }
            }
        }
        self.v.push(Str(ts));
    }

    /// Add a new general TaggedLineElement to the line
    pub fn push(&mut self, tle: TaggedLineElement<T>) {
        use self::TaggedLineElement::Str;

        if let Str(ts) = tle {
            self.push_str(ts);
        } else {
            self.v.push(tle);
        }
    }

    /// Add a new fragment to the start of the line
    pub fn insert_front(&mut self, ts: TaggedString<T>) {
        use self::TaggedLineElement::Str;

        self.v.insert(0, Str(ts));
    }

    /// Add text with a particular tag to self
    pub fn push_char(&mut self, c: char, tag: &T) {
        use self::TaggedLineElement::Str;

        if !self.v.is_empty() {
            if let Str(ref mut ts_prev) = self.v.last_mut().unwrap() {
                if ts_prev.tag == *tag {
                    ts_prev.s.push(c);
                    return;
                }
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
    pub fn consume(&mut self, tl: &mut TaggedLine<T>) {
        for ts in tl.v.drain(..) {
            self.push(ts);
        }
    }

    /// Drain the contained items
    pub fn drain_all(&mut self) -> vec::Drain<TaggedLineElement<T>> {
        self.v.drain(..)
    }

    /// Iterator over the chars in this line.
    #[cfg_attr(feature = "clippy", allow(needless_lifetimes))]
    pub fn chars<'a>(&'a self) -> impl Iterator<Item = char> + 'a {
        use self::TaggedLineElement::Str;

        self.v.iter().flat_map(|tle| {
            if let Str(ts) = tle {
                ts.s.chars()
            } else {
                "".chars()
            }
        })
    }

    #[cfg(feature = "html_trace")]
    /// Return a string contents for debugging.
    fn to_string(&self) -> String {
        self.chars().collect()
    }

    /// Iterator over TaggedLineElements
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &TaggedLineElement<T>> + 'a {
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
    pub fn into_tagged_strings(self) -> impl Iterator<Item = TaggedString<T>> {
        self.v.into_iter().filter_map(|tle| match tle {
            TaggedLineElement::Str(ts) => Some(ts),
            _ => None,
        })
    }

    /// Return the width of the line in cells
    pub fn width(&self) -> usize {
        self.tagged_strings().map(TaggedString::width).sum()
    }

    /// Pad this line to width with spaces (or if already at least this wide, do
    /// nothing).
    pub fn pad_to(&mut self, width: usize) {
        use self::TaggedLineElement::Str;

        let my_width = self.width();
        if width > my_width {
            self.v.push(Str(TaggedString {
                s: format!("{: <width$}", "", width = width - my_width),
                tag: T::default(),
            }));
        }
    }
}

/// A type to build up wrapped text, allowing extra metadata for
/// spans.
#[derive(Debug, Clone)]
struct WrappedBlock<T> {
    width: usize,
    text: Vec<TaggedLine<T>>,
    textlen: usize,
    line: TaggedLine<T>,
    linelen: usize,
    spacetag: Option<T>, // Tag for the whitespace before the current word
    word: TaggedLine<T>, // The current word (with no whitespace).
    wordlen: usize,
    pre_wrapped: bool, // If true, we've been forced to wrap a <pre> line.
}

impl<T: Clone + Eq + Debug + Default> WrappedBlock<T> {
    pub fn new(width: usize) -> WrappedBlock<T> {
        WrappedBlock {
            width,
            text: Vec::new(),
            textlen: 0,
            line: TaggedLine::new(),
            linelen: 0,
            spacetag: None,
            word: TaggedLine::new(),
            wordlen: 0,
            pre_wrapped: false,
        }
    }

    fn flush_word(&mut self) {
        use self::TaggedLineElement::Str;

        /* Finish the word. */
        html_trace_quiet!("flush_word: word={:?}, linelen={}", self.word, self.linelen);
        if !self.word.is_empty() {
            self.pre_wrapped = false;
            let space_in_line = self.width - self.linelen;
            let space_needed = self.wordlen + if self.linelen > 0 { 1 } else { 0 }; // space
            if space_needed <= space_in_line {
                html_trace!("Got enough space");
                if self.linelen > 0 {
                    self.line.push(Str(TaggedString {
                        s: " ".into(),
                        tag: self.spacetag.take().unwrap_or_else(|| Default::default()),
                    }));
                    self.linelen += 1;
                    html_trace!("linelen incremented to {}", self.linelen);
                }
                self.line.consume(&mut self.word);
                self.linelen += self.wordlen;
                html_trace!("linelen increased by wordlen to {}", self.linelen);
            } else {
                html_trace!("Not enough space");
                /* Start a new line */
                self.flush_line();
                if self.wordlen <= self.width {
                    html_trace!("wordlen <= width");
                    let mut new_word = TaggedLine::new();
                    mem::swap(&mut new_word, &mut self.word);
                    mem::swap(&mut self.line, &mut new_word);
                    self.linelen = self.wordlen;
                    html_trace!("linelen set to wordlen {}", self.linelen);
                } else {
                    html_trace!("Splitting the word");
                    /* We need to split the word. */
                    let mut wordbits = self.word.drain_all();
                    /* Note: there's always at least one piece */
                    let mut opt_elt = wordbits.next();
                    let mut lineleft = self.width;
                    while let Some(elt) = opt_elt.take() {
                        html_trace!("Take element {:?}", elt);
                        if let Str(piece) = elt {
                            let w = piece.width();
                            if w <= lineleft {
                                self.line.push(Str(piece));
                                lineleft -= w;
                                self.linelen += w;
                                html_trace!("linelen had w={} added to {}", w, self.linelen);
                                opt_elt = wordbits.next();
                            } else {
                                /* Split into two */
                                let mut split_idx = 0;
                                for (idx, c) in piece.s.char_indices() {
                                    let c_w = UnicodeWidthChar::width(c).unwrap();
                                    if c_w <= lineleft {
                                        lineleft -= c_w;
                                    } else {
                                        split_idx = idx;
                                        break;
                                    }
                                }
                                self.line.push(Str(TaggedString {
                                    s: piece.s[..split_idx].into(),
                                    tag: piece.tag.clone(),
                                }));
                                {
                                    let mut tmp_line = TaggedLine::new();
                                    mem::swap(&mut tmp_line, &mut self.line);
                                    self.text.push(tmp_line);
                                }
                                lineleft = self.width;
                                self.linelen = 0;
                                html_trace!("linelen set to zero here");
                                opt_elt = Some(Str(TaggedString {
                                    s: piece.s[split_idx..].into(),
                                    tag: piece.tag,
                                }));
                            }
                        } else {
                            self.line.push(elt);
                            opt_elt = wordbits.next();
                        }
                    }
                }
            }
        }
        self.wordlen = 0;
    }

    fn flush_line(&mut self) {
        if !self.line.is_empty() {
            self.force_flush_line();
        }
    }

    fn force_flush_line(&mut self) {
        let mut tmp_line = TaggedLine::new();
        mem::swap(&mut tmp_line, &mut self.line);
        self.text.push(tmp_line);
        self.linelen = 0;
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

    pub fn add_preformatted_text(&mut self, text: &str, tag_main: &T, tag_wrapped: &T) {
        html_trace!(
            "WrappedBlock::add_preformatted_text({}), {:?}/{:?}",
            text,
            tag_main,
            tag_wrapped
        );
        // Make sure that any previous word has been sent to the line, as we
        // bypass the word buffer.
        self.flush_word();

        for c in text.chars() {
            if let Some(charwidth) = UnicodeWidthChar::width(c) {
                if self.linelen + charwidth > self.width {
                    self.flush_line();
                    self.pre_wrapped = true;
                }
                self.line.push_char(
                    c,
                    if self.pre_wrapped {
                        tag_wrapped
                    } else {
                        tag_main
                    },
                );
                self.linelen += charwidth;
            } else {
                match c {
                    '\n' => {
                        self.force_flush_line();
                        self.pre_wrapped = false;
                    }
                    '\t' => {
                        let tab_stop = 8;
                        let mut at_least_one_space = false;
                        while self.linelen % tab_stop != 0 || !at_least_one_space {
                            if self.linelen >= self.width {
                                self.flush_line();
                            } else {
                                self.line.push_char(
                                    ' ',
                                    if self.pre_wrapped {
                                        tag_wrapped
                                    } else {
                                        tag_main
                                    },
                                );
                                self.linelen += 1;
                                at_least_one_space = true;
                            }
                        }
                    }
                    _ => {}
                }
            }
            html_trace_quiet!("  Added char {:?}", c);
        }
    }

    pub fn add_element(&mut self, elt: TaggedLineElement<T>) {
        self.word.push(elt);
    }

    pub fn text_len(&self) -> usize {
        self.textlen + self.linelen + self.wordlen
    }

    pub fn is_empty(&self) -> bool {
        self.text_len() == 0
    }
}

/// Allow decorating/styling text.
///
/// Decorating refers to adding extra text around the rendered version
/// of some elements, such as surrounding emphasised text with `*` like
/// in markdown: `Some *bold* text`.  The decorations are formatted and
/// wrapped along with the rest of the rendered text.  This is
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
    fn decorate_em_start(&mut self) -> (String, Self::Annotation);

    /// Return a suffix for after an em.
    fn decorate_em_end(&mut self) -> String;

    /// Return an annotation and rendering prefix for strong
    fn decorate_strong_start(&mut self) -> (String, Self::Annotation);

    /// Return a suffix for after an strong.
    fn decorate_strong_end(&mut self) -> String;

    /// Return an annotation and rendering prefix for strikeout
    fn decorate_strikeout_start(&mut self) -> (String, Self::Annotation);

    /// Return a suffix for after an strikeout.
    fn decorate_strikeout_end(&mut self) -> String;

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
    fn decorate_image(&mut self, src: &str, title: &str) -> (String, Self::Annotation);

    /// Return prefix string of header in specific level.
    fn header_prefix(&mut self, level: usize) -> String;

    /// Return prefix string of quoted block.
    fn quote_prefix(&mut self) -> String;

    /// Return prefix string of unordered list item.
    fn unordered_item_prefix(&mut self) -> String;

    /// Return prefix string of ith ordered list item.
    fn ordered_item_prefix(&mut self, i: i64) -> String;

    /// Return a new decorator of the same type which can be used
    /// for sub blocks.
    fn make_subblock_decorator(&self) -> Self;

    /// Finish with a document, and return extra lines (eg footnotes)
    /// to add to the rendered text.
    fn finalise(&mut self, links: Vec<String>) -> Vec<TaggedLine<Self::Annotation>>;
}

/// A space on a horizontal row.
#[derive(Copy, Clone, Debug)]
pub enum BorderSegHoriz {
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

    /// Create a new blank border line.
    pub fn new_type(width: usize, linetype: BorderSegHoriz) -> BorderHoriz {
        BorderHoriz {
            segments: vec![linetype; width],
        }
    }

    /// Stretch the line to at least the specified width
    pub fn stretch_to(&mut self, width: usize) {
        use self::BorderSegHoriz::*;
        while width > self.segments.len() {
            self.segments.push(Straight);
        }
    }

    /// Make a join to a line above at the xth cell
    pub fn join_above(&mut self, x: usize) {
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
    pub fn join_below(&mut self, x: usize) {
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
    pub fn merge_from_below(&mut self, other: &BorderHoriz, pos: usize) {
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
    pub fn merge_from_above(&mut self, other: &BorderHoriz, pos: usize) {
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
    pub fn to_vertical_lines_above(&self) -> String {
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
    pub fn into_string(self) -> String {
        self.segments
            .into_iter()
            .map(|seg| match seg {
                BorderSegHoriz::Straight => '─',
                BorderSegHoriz::StraightVert => '/',
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
#[derive(Clone, Debug)]
pub enum RenderLine<T: PartialEq + Eq + Clone + Debug + Default> {
    /// Some rendered text
    Text(TaggedLine<T>),
    /// A table border line
    Line(BorderHoriz),
}

impl<T: PartialEq + Eq + Clone + Debug + Default> RenderLine<T> {
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
        use self::TaggedLineElement::Str;

        match self {
            RenderLine::Text(tagged) => tagged,
            RenderLine::Line(border) => {
                let mut tagged = TaggedLine::new();
                tagged.push(Str(TaggedString {
                    s: border.into_string(),
                    tag: T::default(),
                }));
                tagged
            }
        }
    }

    #[cfg(feature = "html_trace")]
    /// For testing, return a simple string of the contents.
    fn to_string(&self) -> String {
        match self {
            RenderLine::Text(tagged) => tagged.to_string(),
            RenderLine::Line(border) => border.to_string(),
        }
    }
}

/// A renderer which just outputs plain text with
/// annotations depending on a decorator.
#[derive(Clone)]
pub struct SubRenderer<D: TextDecorator> {
    width: usize,
    lines: LinkedList<RenderLine<Vec<D::Annotation>>>,
    /// True at the end of a block, meaning we should add
    /// a blank line if any other text is added.
    at_block_end: bool,
    wrapping: Option<WrappedBlock<Vec<D::Annotation>>>,
    decorator: D,
    ann_stack: Vec<D::Annotation>,
    text_filter_stack: Vec<fn(&str) -> Option<String>>,
    /// The depth of <pre> block stacking.
    pre_depth: usize,
}

impl<D: TextDecorator + Debug> std::fmt::Debug for SubRenderer<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("SubRenderer")
            .field("width", &self.width)
            .field("lines", &self.lines)
            .field("decorator", &self.decorator)
            .field("ann_stack", &self.ann_stack)
            .field("pre_depth", &self.pre_depth)
            .finish()
    }
}

impl<D: TextDecorator> SubRenderer<D> {
    /// Render links as lines
    pub fn finalise(&mut self, links: Vec<String>) -> Vec<TaggedLine<D::Annotation>> {
        self.decorator.finalise(links)
    }

    /// Construct a new empty SubRenderer.
    pub fn new(width: usize, decorator: D) -> SubRenderer<D> {
        html_trace!("new({})", width);
        SubRenderer {
            width,
            lines: LinkedList::new(),
            at_block_end: false,
            wrapping: None,
            decorator,
            ann_stack: Vec::new(),
            pre_depth: 0,
            text_filter_stack: Vec::new(),
        }
    }

    fn ensure_wrapping_exists(&mut self) {
        if self.wrapping.is_none() {
            self.wrapping = Some(WrappedBlock::new(self.width));
        }
    }

    /// Get the current line wrapping context (and create if
    /// needed).
    fn current_text(&mut self) -> &mut WrappedBlock<Vec<D::Annotation>> {
        self.ensure_wrapping_exists();
        self.wrapping.as_mut().unwrap()
    }

    /// Add a prerendered (multiline) string with the current annotations.
    pub fn add_subblock(&mut self, s: &str) {
        use self::TaggedLineElement::Str;

        html_trace!("add_subblock({}, {})", self.width, s);
        let tag = self.ann_stack.clone();
        self.lines.extend(s.lines().map(|l| {
            let mut line = TaggedLine::new();
            line.push(Str(TaggedString {
                s: l.into(),
                tag: tag.clone(),
            }));
            RenderLine::Text(line)
        }));
    }

    /// Flushes the current wrapped block into the lines.
    fn flush_wrapping(&mut self) {
        if let Some(w) = self.wrapping.take() {
            self.lines
                .extend(w.into_lines().into_iter().map(RenderLine::Text))
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
        #[cfg(feature = "html_trace")]
        let width: usize = self.width;
        for line in self.into_lines() {
            result.push_str(&line.into_string());
            result.push('\n');
        }
        html_trace!("into_string({}, {:?})", width, result);
        result
    }

    #[cfg(feature = "html_trace")]
    /// Returns a string of the current builder contents (for testing).
    fn to_string(&self) -> String {
        let mut result = String::new();
        for line in &self.lines {
            result += &line.to_string();
            result.push_str("\n");
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
                if pos + width > self.width {
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
    pub fn into_lines(mut self) -> LinkedList<RenderLine<Vec<D::Annotation>>> {
        self.flush_wrapping();
        self.lines
    }

    fn add_horizontal_line(&mut self, line: BorderHoriz) {
        self.flush_wrapping();
        self.lines.push_back(RenderLine::Line(line));
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
    fn add_empty_line(&mut self) {
        html_trace!("add_empty_line()");
        self.flush_all();
        self.lines.push_back(RenderLine::Text(TaggedLine::new()));
        html_trace_quiet!("add_empty_line: at_block_end <- false");
        self.at_block_end = false;
        html_trace_quiet!("add_empty_line: new lines: {:?}", self.lines);
    }

    fn new_sub_renderer(&self, width: usize) -> Self {
        SubRenderer::new(width, self.decorator.make_subblock_decorator())
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

    fn new_line_hard(&mut self) {
        match self.wrapping {
            None => self.add_empty_line(),
            Some(WrappedBlock {
                linelen: 0,
                wordlen: 0,
                ..
            }) => self.add_empty_line(),
            Some(_) => self.flush_all(),
        }
    }

    fn add_horizontal_border(&mut self) {
        self.flush_wrapping();
        self.lines
            .push_back(RenderLine::Line(BorderHoriz::new(self.width)));
    }

    fn add_horizontal_border_width(&mut self, width: usize) {
        self.flush_wrapping();
        self.lines
            .push_back(RenderLine::Line(BorderHoriz::new(width)));
    }

    fn start_pre(&mut self) {
        self.pre_depth += 1;
    }

    fn end_pre(&mut self) {
        if self.pre_depth > 0 {
            self.pre_depth -= 1;
        } else {
            panic!("Attempt to end a preformatted block which wasn't opened.");
        }
    }

    fn end_block(&mut self) {
        self.at_block_end = true;
    }

    fn add_inline_text(&mut self, text: &str) {
        html_trace!("add_inline_text({}, {})", self.width, text);
        if self.pre_depth == 0 && self.at_block_end && text.chars().all(char::is_whitespace) {
            // Ignore whitespace between blocks.
            return;
        }
        if self.at_block_end {
            self.start_block();
        }
        // ensure wrapping is set
        let _ = self.current_text();
        let mut s = None;
        // Do any filtering of the text
        for filter in &self.text_filter_stack {
            // When we stop supporting Rust < 1.40, this can become:
            //let srctext = s.as_deref().unwrap_or(text);
            let srctext = s.as_ref().map(Deref::deref).unwrap_or(text);
            if let Some(filtered) = filter(srctext) {
                s = Some(filtered);
            }
        }
        // When we stop supporting Rust < 1.40, this can become:
        //let filtered_text = s.as_deref().unwrap_or(text);
        let filtered_text = s.as_ref().map(Deref::deref).unwrap_or(text);
        if self.pre_depth == 0 {
            self.wrapping
                .as_mut()
                .unwrap()
                .add_text(filtered_text, &self.ann_stack);
        } else {
            let mut tag_first = self.ann_stack.clone();
            let mut tag_cont = self.ann_stack.clone();
            tag_first.push(self.decorator.decorate_preformat_first());
            tag_cont.push(self.decorator.decorate_preformat_cont());
            self.wrapping.as_mut().unwrap().add_preformatted_text(
                filtered_text,
                &tag_first,
                &tag_cont,
            );
        }
    }

    fn width(&self) -> usize {
        self.width
    }

    fn add_block_line(&mut self, line: &str) {
        self.add_subblock(line);
    }

    fn append_subrender<'a, I>(&mut self, other: Self, prefixes: I)
    where
        I: Iterator<Item = &'a str>,
    {
        use self::TaggedLineElement::Str;

        self.flush_wrapping();
        let tag = self.ann_stack.clone();
        self.lines.extend(
            other
                .into_lines()
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
                            s: l.into_string(),
                            tag: tag.clone(),
                        }));
                        RenderLine::Text(tline)
                    }
                }),
        );
    }

    fn append_columns_with_borders<I>(&mut self, cols: I, collapse: bool)
    where
        I: IntoIterator<Item = Self>,
        Self: Sized,
    {
        use self::TaggedLineElement::Str;
        html_trace!("append_columns_with_borders(collapse={})", collapse);
        html_trace!("self=\n{}", self.to_string());

        self.flush_wrapping();

        let mut tot_width = 0;

        let mut line_sets = cols
            .into_iter()
            .map(|sub_r| {
                let width = sub_r.width;
                tot_width += width;
                html_trace!("Adding column:\n{}", sub_r.to_string());
                (
                    width,
                    sub_r
                        .into_lines()
                        .into_iter()
                        .map(|mut line| {
                            match line {
                                RenderLine::Text(ref mut tline) => {
                                    tline.pad_to(width);
                                }
                                RenderLine::Line(ref mut border) => {
                                    border.stretch_to(width);
                                }
                            }
                            line
                        })
                        .collect(),
                )
            })
            .collect::<Vec<(usize, Vec<RenderLine<_>>)>>();

        tot_width += line_sets.len().saturating_sub(1);

        let mut next_border = BorderHoriz::new(tot_width);

        // Join the vertical lines to all the borders
        {
            let mut pos = 0;
            if let &mut RenderLine::Line(ref mut prev_border) = self.lines.back_mut().unwrap() {
                html_trace!("Merging with last line:\n{}", prev_border.to_string());
                for &(w, _) in &line_sets[..line_sets.len() - 1] {
                    html_trace!("pos={}, w={}", pos, w);
                    prev_border.join_below(pos + w);
                    next_border.join_above(pos + w);
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
            html_trace!("Collapsing borders.");
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
                    html_trace!("Ends border");
                    if let RenderLine::Line(line) = sublines.pop().unwrap() {
                        next_border.merge_from_above(&line, pos);
                        column_padding[col_no] = Some(line.to_vertical_lines_above())
                    }
                }
                pos += w + 1;
            }
        }

        let cell_height = line_sets
            .iter()
            .map(|&(_, ref v)| v.len())
            .max()
            .unwrap_or(0);
        let spaces: String = (0..tot_width).map(|_| ' ').collect();
        let last_cellno = line_sets.len() - 1;
        for i in 0..cell_height {
            let mut line = TaggedLine::new();
            for (cellno, &mut (width, ref mut ls)) in line_sets.iter_mut().enumerate() {
                if let Some(piece) = ls.get_mut(i) {
                    match piece {
                        &mut RenderLine::Text(ref mut tline) => {
                            line.consume(tline);
                        }
                        &mut RenderLine::Line(ref bord) => {
                            line.push(Str(TaggedString {
                                s: bord.to_string(),
                                tag: self.ann_stack.clone(),
                            }));
                        }
                    };
                } else {
                    line.push(Str(TaggedString {
                        s: column_padding[cellno]
                            .as_ref()
                            .map(|s| s.clone())
                            .unwrap_or_else(|| spaces[0..width].to_string()),

                        tag: self.ann_stack.clone(),
                    }));
                }
                if cellno != last_cellno {
                    line.push_char('│', &self.ann_stack);
                }
            }
            self.lines.push_back(RenderLine::Text(line));
        }
        self.lines.push_back(RenderLine::Line(next_border));
    }

    fn append_vert_row<I>(&mut self, cols: I)
    where
        I: IntoIterator<Item = Self>,
        Self: Sized,
    {
        html_trace!("append_vert_row()");
        html_trace!("self=\n{}", self.to_string());

        self.flush_wrapping();

        let width = self.width();

        let mut first = true;
        for col in cols {
            if first {
                first = false;
            } else {
                let border = BorderHoriz::new_type(width, BorderSegHoriz::StraightVert);
                self.add_horizontal_line(border);
            }
            self.append_subrender(col, std::iter::repeat(""));
        }
        self.add_horizontal_border();
    }

    fn empty(&self) -> bool {
        self.lines.is_empty()
            && if let Some(wrapping) = &self.wrapping {
                wrapping.is_empty()
            } else {
                true
            }
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

    fn start_link(&mut self, target: &str) {
        let (s, annotation) = self.decorator.decorate_link_start(target);
        self.ann_stack.push(annotation);
        self.add_inline_text(&s);
    }
    fn end_link(&mut self) {
        let s = self.decorator.decorate_link_end();
        self.add_inline_text(&s);
        self.ann_stack.pop();
    }
    fn start_emphasis(&mut self) {
        let (s, annotation) = self.decorator.decorate_em_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s);
    }
    fn end_emphasis(&mut self) {
        let s = self.decorator.decorate_em_end();
        self.add_inline_text(&s);
        self.ann_stack.pop();
    }
    fn start_strong(&mut self) {
        let (s, annotation) = self.decorator.decorate_strong_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s);
    }
    fn end_strong(&mut self) {
        let s = self.decorator.decorate_strong_end();
        self.add_inline_text(&s);
        self.ann_stack.pop();
    }
    fn start_strikeout(&mut self) {
        let (s, annotation) = self.decorator.decorate_strikeout_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s);
        self.text_filter_stack.push(filter_text_strikeout);
    }
    fn end_strikeout(&mut self) {
        self.text_filter_stack.pop().unwrap();
        let s = self.decorator.decorate_strikeout_end();
        self.add_inline_text(&s);
        self.ann_stack.pop();
    }
    fn start_code(&mut self) {
        let (s, annotation) = self.decorator.decorate_code_start();
        self.ann_stack.push(annotation);
        self.add_inline_text(&s);
    }
    fn end_code(&mut self) {
        let s = self.decorator.decorate_code_end();
        self.add_inline_text(&s);
        self.ann_stack.pop();
    }
    fn add_image(&mut self, src: &str, title: &str) {
        let (s, tag) = self.decorator.decorate_image(src, title);
        self.ann_stack.push(tag);
        self.add_inline_text(&s);
        self.ann_stack.pop();
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

        self.ensure_wrapping_exists();
        self.wrapping
            .as_mut()
            .unwrap()
            .add_element(FragmentStart(fragname.to_string()));
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
    #[cfg_attr(feature = "clippy", allow(new_without_default_derive))]
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

    fn decorate_em_start(&mut self) -> (String, Self::Annotation) {
        ("*".to_string(), ())
    }

    fn decorate_em_end(&mut self) -> String {
        "*".to_string()
    }

    fn decorate_strong_start(&mut self) -> (String, Self::Annotation) {
        ("**".to_string(), ())
    }

    fn decorate_strong_end(&mut self) -> String {
        "**".to_string()
    }

    fn decorate_strikeout_start(&mut self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_strikeout_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_code_start(&mut self) -> (String, Self::Annotation) {
        ("`".to_string(), ())
    }

    fn decorate_code_end(&mut self) -> String {
        "`".to_string()
    }

    fn decorate_preformat_first(&mut self) -> Self::Annotation {
        ()
    }
    fn decorate_preformat_cont(&mut self) -> Self::Annotation {
        ()
    }

    fn decorate_image(&mut self, _src: &str, title: &str) -> (String, Self::Annotation) {
        (format!("[{}]", title), ())
    }

    fn header_prefix(&mut self, level: usize) -> String {
        "#".repeat(level) + " "
    }

    fn quote_prefix(&mut self) -> String {
        "> ".to_string()
    }

    fn unordered_item_prefix(&mut self) -> String {
        "* ".to_string()
    }

    fn ordered_item_prefix(&mut self, i: i64) -> String {
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
    #[cfg_attr(feature = "clippy", allow(new_without_default_derive))]
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

    fn decorate_em_start(&mut self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_em_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_strong_start(&mut self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_strong_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_strikeout_start(&mut self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_strikeout_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_code_start(&mut self) -> (String, Self::Annotation) {
        ("".to_string(), ())
    }

    fn decorate_code_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_preformat_first(&mut self) -> Self::Annotation {
        ()
    }
    fn decorate_preformat_cont(&mut self) -> Self::Annotation {
        ()
    }

    fn decorate_image(&mut self, _src: &str, title: &str) -> (String, Self::Annotation) {
        // FIXME: this should surely be the alt text, not the title text
        (title.to_string(), ())
    }

    fn header_prefix(&mut self, _level: usize) -> String {
        "".to_string()
    }

    fn quote_prefix(&mut self) -> String {
        "".to_string()
    }

    fn unordered_item_prefix(&mut self) -> String {
        "".to_string()
    }

    fn ordered_item_prefix(&mut self, _i: i64) -> String {
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
pub struct RichDecorator {}

/// Annotation type for "rich" text.  Text is associated with a set of
/// these.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum RichAnnotation {
    /// Normal text.
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
}

impl Default for RichAnnotation {
    fn default() -> Self {
        RichAnnotation::Default
    }
}

impl RichDecorator {
    /// Create a new `RichDecorator`.
    #[cfg_attr(feature = "clippy", allow(new_without_default_derive))]
    pub fn new() -> RichDecorator {
        RichDecorator {}
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

    fn decorate_em_start(&mut self) -> (String, Self::Annotation) {
        ("".to_string(), RichAnnotation::Emphasis)
    }

    fn decorate_em_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_strong_start(&mut self) -> (String, Self::Annotation) {
        ("*".to_string(), RichAnnotation::Strong)
    }

    fn decorate_strong_end(&mut self) -> String {
        "*".to_string()
    }

    fn decorate_strikeout_start(&mut self) -> (String, Self::Annotation) {
        ("".to_string(), RichAnnotation::Strikeout)
    }

    fn decorate_strikeout_end(&mut self) -> String {
        "".to_string()
    }

    fn decorate_code_start(&mut self) -> (String, Self::Annotation) {
        ("`".to_string(), RichAnnotation::Code)
    }

    fn decorate_code_end(&mut self) -> String {
        "`".to_string()
    }

    fn decorate_preformat_first(&mut self) -> Self::Annotation {
        RichAnnotation::Preformat(false)
    }

    fn decorate_preformat_cont(&mut self) -> Self::Annotation {
        RichAnnotation::Preformat(true)
    }

    fn decorate_image(&mut self, src: &str, title: &str) -> (String, Self::Annotation) {
        (title.to_string(), RichAnnotation::Image(src.to_string()))
    }

    fn header_prefix(&mut self, level: usize) -> String {
        "#".repeat(level) + " "
    }

    fn quote_prefix(&mut self) -> String {
        "> ".to_string()
    }

    fn unordered_item_prefix(&mut self) -> String {
        "* ".to_string()
    }

    fn ordered_item_prefix(&mut self, i: i64) -> String {
        format!("{}. ", i)
    }

    fn finalise(&mut self, _links: Vec<String>) -> Vec<TaggedLine<RichAnnotation>> {
        Vec::new()
    }

    fn make_subblock_decorator(&self) -> Self {
        RichDecorator::new()
    }
}
