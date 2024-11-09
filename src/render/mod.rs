//! Module containing the `Renderer` interface for constructing a
//! particular text output.

use crate::Colour;
use crate::WhiteSpace;

pub(crate) mod text_renderer;

pub use text_renderer::{
    PlainDecorator, RichAnnotation, RichDecorator, TaggedLine, TaggedLineElement, TextDecorator,
    TrivialDecorator,
};

pub(crate) type Result<T> = std::result::Result<T, TooNarrow>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TooNarrow;

impl From<TooNarrow> for crate::Error {
    fn from(_: TooNarrow) -> crate::Error {
        crate::Error::TooNarrow
    }
}

/// A type which is a backend for HTML to text rendering.
pub(crate) trait Renderer {
    /// Add an empty line to the output (ie between blocks).
    fn add_empty_line(&mut self) -> Result<()>;

    /// Create a sub-renderer for nested blocks.
    fn new_sub_renderer(&self, width: usize) -> Result<Self>
    where
        Self: Sized;

    /// Start a new block.
    fn start_block(&mut self) -> Result<()>;

    /// Mark the end of a block.
    fn end_block(&mut self);

    /// Start a new line, if necessary (but don't add a new line).
    fn new_line(&mut self) -> Result<()>;

    /// Start a new line.
    fn new_line_hard(&mut self) -> Result<()>;

    /// Add a horizontal table border.
    fn add_horizontal_border(&mut self) -> Result<()>;

    /// Add a horizontal border which is not the full width
    fn add_horizontal_border_width(
        &mut self,
        #[allow(unused_variables)] width: usize,
    ) -> Result<()> {
        self.add_horizontal_border()
    }

    /// Begin a preformatted block.  This indicates we are inside a <pre> element.
    /// The whitespace/wrapping behaviour is treated separately with `push_ws`.
    fn push_preformat(&mut self);

    /// End a preformatted block.
    fn pop_preformat(&mut self);

    /// Update the white-space CSS setting.
    fn push_ws(&mut self, ws: WhiteSpace);

    /// End the current white-space setting.
    fn pop_ws(&mut self);

    /// Add some inline text (which should be wrapped at the
    /// appropriate width) to the current block.
    fn add_inline_text(&mut self, text: &str) -> Result<()>;

    /// Return the current width in character cells
    fn width(&self) -> usize;

    /// Add a new block from a sub renderer, and prefix every line by the
    /// corresponding text from each iteration of prefixes.
    fn append_subrender<'a, I>(&mut self, other: Self, prefixes: I) -> Result<()>
    where
        I: Iterator<Item = &'a str>;

    /// Append a set of sub renderers joined left-to-right with a vertical line,
    /// and add a horizontal line below.
    /// If collapse is true, then merge top/bottom borders of the subrenderer
    /// with the surrounding one.
    fn append_columns_with_borders<I>(&mut self, cols: I, collapse: bool) -> Result<()>
    where
        I: IntoIterator<Item = Self>,
        Self: Sized;

    /// Append a set of sub renderers joined vertically with lines, for tables
    /// which would otherwise be too wide for the screen.
    fn append_vert_row<I>(&mut self, cols: I) -> Result<()>
    where
        I: IntoIterator<Item = Self>,
        Self: Sized;

    /// Returns true if this renderer has no content.
    fn empty(&self) -> bool;

    /// Start a hyperlink
    /// TODO: return sub-builder or similar to make misuse
    /// of start/link harder?
    fn start_link(&mut self, target: &str) -> Result<()>;

    /// Finish a hyperlink started earlier.
    fn end_link(&mut self) -> Result<()>;

    /// Start an emphasised region
    fn start_emphasis(&mut self) -> Result<()>;

    /// Finish emphasised text started earlier.
    fn end_emphasis(&mut self) -> Result<()>;

    /// Start a strong region
    fn start_strong(&mut self) -> Result<()>;

    /// Finish strong text started earlier.
    fn end_strong(&mut self) -> Result<()>;

    /// Start a strikeout region
    fn start_strikeout(&mut self) -> Result<()>;

    /// Finish strikeout text started earlier.
    fn end_strikeout(&mut self) -> Result<()>;

    /// Start a code region
    fn start_code(&mut self) -> Result<()>;

    /// End a code region
    fn end_code(&mut self) -> Result<()>;

    /// Add an image
    fn add_image(&mut self, src: &str, title: &str) -> Result<()>;

    /// Get prefix string of header in specific level.
    fn header_prefix(&mut self, level: usize) -> String;

    /// Get prefix string of quoted block.
    fn quote_prefix(&mut self) -> String;

    /// Get prefix string of unordered list item.
    fn unordered_item_prefix(&mut self) -> String;

    /// Get prefix string of ith ordered list item.
    fn ordered_item_prefix(&mut self, i: i64) -> String;

    /// Record the start of a named HTML fragment
    fn record_frag_start(&mut self, fragname: &str);

    #[allow(unused)]
    /// Push a new foreground colour
    fn push_colour(&mut self, colour: Colour);

    #[allow(unused)]
    /// Pop the last foreground colour
    fn pop_colour(&mut self);

    #[allow(unused)]
    /// Push a new background colour
    fn push_bgcolour(&mut self, colour: Colour);

    #[allow(unused)]
    /// Pop the last background colour
    fn pop_bgcolour(&mut self);

    /// Start a section of superscript text.
    fn start_superscript(&mut self) -> Result<()>;

    /// End a section of superscript text.
    fn end_superscript(&mut self) -> Result<()>;
}
