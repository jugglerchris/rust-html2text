pub mod text_renderer;

/// A type which is a backend for HTML to text rendering.
pub trait Renderer {
    type Sub: Renderer;

    /// Add an empty line to the output (ie between blocks).
    fn add_empty_line(&mut self);

    /// Create a sub-renderer for nested blocks.
    fn new_sub_renderer(&self, width: usize) -> Self::Sub;

    /// Start a new block.
    fn start_block(&mut self);

    /// Mark the end of a block.
    fn end_block(&mut self);

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

    /// Append a set of Sub joined left-to-right with a separator.
    fn append_columns<I>(&mut self, cols: I, separator: &str)
                           where I:IntoIterator<Item=Self::Sub>;

    /// Returns true if this renderer has no content.
    fn empty(&self) -> bool;

    /// Return the length of the contained text.
    fn text_len(&self) -> usize;
}