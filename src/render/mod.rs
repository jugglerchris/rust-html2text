//! Module containing the `Renderer` interface for constructing a
//! particular text output.

pub mod text_renderer;

/// A type which is a backend for HTML to text rendering.
pub trait Renderer {
    /// A type of a sub-renderer which will be used for rendering
    /// nested blocks such as table cells and list items.
    type Sub: Renderer;

    /// Add an empty line to the output (ie between blocks).
    fn add_empty_line(&mut self);

    /// Create a sub-renderer for nested blocks.
    fn new_sub_renderer(&self, width: usize) -> Self::Sub;

    /// Start a new block.
    fn start_block(&mut self);

    /// Mark the end of a block.
    fn end_block(&mut self);

    /// Start a new line, if necessary (but don't add a new line).
    fn new_line(&mut self);

    /// Add a horizontal table border.
    fn add_horizontal_border(&mut self);

    /// Add a new block of preformatted text.
    fn add_preformatted_block(&mut self, text: &str);

    /// Add some inline text (which should be wrapped at the
    /// appropriate width) to the current block.
    fn add_inline_text(&mut self, text: &str);

    /// Return the current width in character cells
    fn width(&self) -> usize;

    /// Add a line to the current block without starting a new one.
    fn add_block_line(&mut self, line: &str);

    /// Add a new block from a Sub, and prefix every line by the
    /// corresponding text from each iteration of prefixes.
    fn append_subrender<'a, I>(&mut self, other: Self::Sub, prefixes: I)
                           where I:Iterator<Item=&'a str>;

    /// Append a set of Sub joined left-to-right with a vertical line,
    /// and add a horizontal line below.
    /// If collapse is true, then merge top/bottom borders of the subrenderer
    /// with the surrounding one.
    fn append_columns_with_borders<I>(&mut self, cols: I, collapse: bool)
                           where I:IntoIterator<Item=Self::Sub>;

    /// Returns true if this renderer has no content.
    fn empty(&self) -> bool;

    /// Return the length of the contained text.
    fn text_len(&self) -> usize;

    /// Start a hyperlink
    /// TODO: return sub-builder or similar to make misuse
    /// of start/link harder?
    fn start_link(&mut self, target: &str);

    /// Finish a hyperlink started earlier.
    fn end_link(&mut self);

    /// Start an emphasised region
    fn start_emphasis(&mut self);

    /// Finish emphasised text started earlier.
    fn end_emphasis(&mut self);

    /// Add an image
    fn add_image(&mut self, title: &str);
}