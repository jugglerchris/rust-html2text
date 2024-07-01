//! Convert HTML to text formats.
//!
//! This crate renders HTML into a text format, wrapped to a specified width.
//! This can either be plain text or with extra annotations to (for example)
//! show in a terminal which supports colours.
//!
//! # Examples
//!
//! ```rust
//! # use html2text::from_read;
//! let html = b"
//!        <ul>
//!          <li>Item one</li>
//!          <li>Item two</li>
//!          <li>Item three</li>
//!        </ul>";
//! assert_eq!(from_read(&html[..], 20).unwrap(),
//!            "\
//! * Item one
//! * Item two
//! * Item three
//! ");
//! ```
//! A couple of simple demonstration programs are included as examples:
//!
//! ### html2text
//!
//! The simplest example uses `from_read` to convert HTML on stdin into plain
//! text:
//!
//! ```sh
//! $ cargo run --example html2text < foo.html
//! [...]
//! ```
//!
//! ### html2term
//!
//! A very simple example of using the rich interface (`from_read_rich`) for a
//! slightly interactive console HTML viewer is provided as `html2term`.
//!
//! ```sh
//! $ cargo run --example html2term foo.html
//! [...]
//! ```
//!
//! Note that this example takes the HTML file as a parameter so that it can
//! read keys from stdin.
//!

#![deny(missing_docs)]

// Check code in README.md
#[cfg(doctest)]
#[doc = include_str!("../README.md")]
struct ReadMe;

#[macro_use]
extern crate html5ever;

#[macro_use]
mod macros;

pub mod css;
pub mod render;

use render::text_renderer::{
    RenderLine, RenderOptions, RichAnnotation, SubRenderer, TaggedLine, TextRenderer,
};
use render::{Renderer, RichDecorator, TextDecorator};

use html5ever::driver::ParseOpts;
use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use html5ever::tree_builder::TreeBuilderOpts;
mod markup5ever_rcdom;
pub use markup5ever_rcdom::RcDom;
use markup5ever_rcdom::{
    Handle,
    NodeData::{Comment, Document, Element},
};
use std::cell::Cell;
use std::cmp::{max, min};
use std::collections::{BTreeSet, HashMap};
use unicode_width::UnicodeWidthStr;

use std::io;
use std::io::Write;
use std::iter::{once, repeat};

/// An RGB colour value
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Colour {
    /// Red value
    pub r: u8,
    /// Green value
    pub g: u8,
    /// Blue value
    pub b: u8,
}

/// Errors from reading or rendering HTML
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    /// The output width was too narrow to render to.
    #[error("Output width not wide enough.")]
    TooNarrow,
    #[cfg(feature = "css")]
    /// CSS parse error
    #[error("Invalid CSS")]
    CssParseError,
    /// An general error was encountered.
    #[error("Unknown failure")]
    Fail,
    /// An I/O error
    #[error("I/O error")]
    IoError(#[from] std::io::Error),
}

impl PartialEq for Error {
    fn eq(&self, other: &Error) -> bool {
        use Error::*;
        match (self, other) {
            (TooNarrow, TooNarrow) => true,
            #[cfg(feature = "css")]
            (CssParseError, CssParseError) => true,
            (Fail, Fail) => true,
            _ => false,
        }
    }
}

impl Eq for Error {}

type Result<T> = std::result::Result<T, Error>;

/// A dummy writer which does nothing
struct Discard {}
impl Write for Discard {
    fn write(&mut self, bytes: &[u8]) -> std::result::Result<usize, io::Error> {
        Ok(bytes.len())
    }
    fn flush(&mut self) -> std::result::Result<(), io::Error> {
        Ok(())
    }
}

const MIN_WIDTH: usize = 3;

/// Size information/estimate
#[derive(Debug, Copy, Clone, Default)]
struct SizeEstimate {
    size: usize,      // Rough overall size
    min_width: usize, // The narrowest possible

    // The use is specific to the node type.
    prefix_size: usize,
}

impl SizeEstimate {
    /// Combine two estimates into one (add size and take the largest
    /// min width)
    fn add(self, other: SizeEstimate) -> SizeEstimate {
        let min_width = max(self.min_width, other.min_width);
        SizeEstimate {
            size: self.size + other.size,
            min_width,
            prefix_size: 0,
        }
    }
    /// Combine two estimates into one which need to be side by side.
    /// The min widths are added.
    fn add_hor(self, other: SizeEstimate) -> SizeEstimate {
        SizeEstimate {
            size: self.size + other.size,
            min_width: self.min_width + other.min_width,
            prefix_size: 0,
        }
    }

    /// Combine two estimates into one (take max of each)
    fn max(self, other: SizeEstimate) -> SizeEstimate {
        SizeEstimate {
            size: max(self.size, other.size),
            min_width: max(self.min_width, other.min_width),
            prefix_size: 0,
        }
    }
}

#[derive(Clone, Debug)]
/// Render tree table cell
struct RenderTableCell {
    colspan: usize,
    content: Vec<RenderNode>,
    size_estimate: Cell<Option<SizeEstimate>>,
    col_width: Option<usize>, // Actual width to use
}

impl RenderTableCell {
    /// Calculate or return the estimate size of the cell
    fn get_size_estimate(&self) -> SizeEstimate {
        if self.size_estimate.get().is_none() {
            let size = self
                .content
                .iter()
                .map(|node| node.get_size_estimate())
                .fold(Default::default(), SizeEstimate::add);
            self.size_estimate.set(Some(size));
        }
        self.size_estimate.get().unwrap()
    }
}

#[derive(Clone, Debug)]
/// Render tree table row
struct RenderTableRow {
    cells: Vec<RenderTableCell>,
    col_sizes: Option<Vec<usize>>,
}

impl RenderTableRow {
    /// Return a mutable iterator over the cells.
    fn cells(&self) -> std::slice::Iter<RenderTableCell> {
        self.cells.iter()
    }
    /// Return a mutable iterator over the cells.
    fn cells_mut(&mut self) -> std::slice::IterMut<RenderTableCell> {
        self.cells.iter_mut()
    }
    /// Count the number of cells in the row.
    /// Takes into account colspan.
    fn num_cells(&self) -> usize {
        self.cells.iter().map(|cell| cell.colspan.max(1)).sum()
    }

    /// Return the contained cells as RenderNodes, annotated with their
    /// widths if available.  Skips cells with no width allocated.
    fn into_cells(self, vertical: bool) -> Vec<RenderNode> {
        let mut result = Vec::new();
        let mut colno = 0;
        let col_sizes = self.col_sizes.unwrap();
        for mut cell in self.cells {
            let colspan = cell.colspan;
            let col_width = if vertical {
                col_sizes[colno]
            } else {
                col_sizes[colno..colno + cell.colspan].iter().sum::<usize>()
            };
            // Skip any zero-width columns
            if col_width > 0 {
                cell.col_width = Some(col_width + cell.colspan - 1);
                result.push(RenderNode::new(RenderNodeInfo::TableCell(cell)));
            }
            colno += colspan;
        }
        result
    }
}

#[derive(Clone, Debug)]
/// A representation of a table render tree with metadata.
struct RenderTable {
    rows: Vec<RenderTableRow>,
    num_columns: usize,
    size_estimate: Cell<Option<SizeEstimate>>,
}

impl RenderTable {
    /// Create a new RenderTable with the given rows
    fn new(mut rows: Vec<RenderTableRow>) -> RenderTable {
        // We later on want to allocate a vector sized by the column count,
        // but occasionally we see something like colspan="1000000000".  We
        // handle this by remapping the column ids to the smallest values
        // possible.
        //
        // Tables with no explicit colspan will be unchanged, but if there
        // are multiple columns each covered by a single <td> on every row,
        // they will be collapsed into a single column.  For example:
        //
        //    <td><td colspan=1000><td>
        //    <td colspan=1000><td><td>
        //
        //  becomes the equivalent:
        //    <td><td colspan=2><td>
        //    <td colspan=2><td><td>

        // This will include 0 and the index after the last colspan.
        let mut col_positions = BTreeSet::new();
        col_positions.insert(0);
        for row in &rows {
            let mut col = 0;
            for cell in row.cells() {
                col += cell.colspan;
                col_positions.insert(col);
            }
        }

        let colmap: HashMap<_, _> = col_positions
            .into_iter()
            .enumerate()
            .map(|(i, pos)| (pos, i))
            .collect();

        for row in &mut rows {
            let mut pos = 0;
            let mut mapped_pos = 0;
            for cell in row.cells_mut() {
                let nextpos = pos + cell.colspan.max(1);
                let next_mapped_pos = *colmap.get(&nextpos).unwrap();
                cell.colspan = next_mapped_pos - mapped_pos;
                pos = nextpos;
                mapped_pos = next_mapped_pos;
            }
        }

        let num_columns = rows.iter().map(|r| r.num_cells()).max().unwrap_or(0);
        RenderTable {
            rows,
            num_columns,
            size_estimate: Cell::new(None),
        }
    }

    /// Return an iterator over the rows.
    fn rows(&self) -> std::slice::Iter<RenderTableRow> {
        self.rows.iter()
    }

    /// Consume this and return a `Vec<RenderNode>` containing the children;
    /// the children know the column sizes required.
    fn into_rows(self, col_sizes: Vec<usize>, vert: bool) -> Vec<RenderNode> {
        self.rows
            .into_iter()
            .map(|mut tr| {
                tr.col_sizes = Some(col_sizes.clone());
                RenderNode::new(RenderNodeInfo::TableRow(tr, vert))
            })
            .collect()
    }

    fn calc_size_estimate(&self, _context: &HtmlContext) -> SizeEstimate {
        if self.num_columns == 0 {
            let result = SizeEstimate {
                size: 0,
                min_width: 0,
                prefix_size: 0,
            };
            self.size_estimate.set(Some(result));
            return result;
        }
        let mut sizes: Vec<SizeEstimate> = vec![Default::default(); self.num_columns];

        // For now, a simple estimate based on adding up sub-parts.
        for row in self.rows() {
            let mut colno = 0usize;
            for cell in row.cells() {
                let cellsize = cell.get_size_estimate();
                for colnum in 0..cell.colspan {
                    sizes[colno + colnum].size += cellsize.size / cell.colspan;
                    sizes[colno + colnum].min_width = max(
                        sizes[colno + colnum].min_width,
                        cellsize.min_width / cell.colspan,
                    );
                }
                colno += cell.colspan;
            }
        }
        let size = sizes.iter().map(|s| s.size).sum(); // Include borders?
        let min_width = sizes.iter().map(|s| s.min_width).sum::<usize>() + self.num_columns - 1;
        let result = SizeEstimate {
            size,
            min_width,
            prefix_size: 0,
        };
        self.size_estimate.set(Some(result));
        result
    }
}

/// The node-specific information distilled from the DOM.
#[derive(Clone, Debug)]
#[non_exhaustive]
enum RenderNodeInfo {
    /// Some text.
    Text(String),
    /// A group of nodes collected together.
    Container(Vec<RenderNode>),
    /// A link with contained nodes
    Link(String, Vec<RenderNode>),
    /// An emphasised region
    Em(Vec<RenderNode>),
    /// A strong region
    Strong(Vec<RenderNode>),
    /// A struck out region
    Strikeout(Vec<RenderNode>),
    /// A code region
    Code(Vec<RenderNode>),
    /// An image (src, title)
    Img(String, String),
    /// A block element with children
    Block(Vec<RenderNode>),
    /// A header (h1, h2, ...) with children
    Header(usize, Vec<RenderNode>),
    /// A Div element with children
    Div(Vec<RenderNode>),
    /// A preformatted region.
    Pre(Vec<RenderNode>),
    /// A blockquote
    BlockQuote(Vec<RenderNode>),
    /// An unordered list
    Ul(Vec<RenderNode>),
    /// An ordered list
    Ol(i64, Vec<RenderNode>),
    /// A description list (containing Dt or Dd)
    Dl(Vec<RenderNode>),
    /// A term (from a `<dl>`)
    Dt(Vec<RenderNode>),
    /// A definition (from a `<dl>`)
    Dd(Vec<RenderNode>),
    /// A line break
    Break,
    /// A table
    Table(RenderTable),
    /// A set of table rows (from either `<thead>` or `<tbody>`
    TableBody(Vec<RenderTableRow>),
    /// Table row (must only appear within a table body)
    /// If the boolean is true, then the cells are drawn vertically
    /// instead of horizontally (because of space).
    TableRow(RenderTableRow, bool),
    /// Table cell (must only appear within a table row)
    TableCell(RenderTableCell),
    /// Start of a named HTML fragment
    FragStart(String),
    /// A region with a foreground colour
    #[allow(unused)]
    Coloured(Colour, Vec<RenderNode>),
    /// A list item
    ListItem(Vec<RenderNode>),
    /// Superscript text
    Sup(Vec<RenderNode>),
}

/// Common fields from a node.
#[derive(Clone, Debug)]
struct RenderNode {
    size_estimate: Cell<Option<SizeEstimate>>,
    info: RenderNodeInfo,
    style: css::ComputedStyle,
}

impl RenderNode {
    /// Create a node from the RenderNodeInfo.
    fn new(info: RenderNodeInfo) -> RenderNode {
        RenderNode {
            size_estimate: Cell::new(None),
            info,
            style: Default::default(),
        }
    }

    /// Create a node from the RenderNodeInfo.
    fn new_styled(info: RenderNodeInfo, style: css::ComputedStyle) -> RenderNode {
        RenderNode {
            size_estimate: Cell::new(None),
            info,
            style,
        }
    }

    /// Get a size estimate
    fn get_size_estimate(&self) -> SizeEstimate {
        self.size_estimate.get().unwrap()
    }

    /// Calculate the size of this node.
    fn calc_size_estimate<D: TextDecorator>(
        &self,
        context: &HtmlContext,
        decorator: &'_ D,
    ) -> SizeEstimate {
        // If it's already calculated, then just return the answer.
        if let Some(s) = self.size_estimate.get() {
            return s;
        };

        use RenderNodeInfo::*;

        let recurse = |node: &RenderNode| node.calc_size_estimate(context, decorator);

        // Otherwise, make an estimate.
        let estimate = match self.info {
            Text(ref t) | Img(_, ref t) => {
                use unicode_width::UnicodeWidthChar;
                let mut len = 0;
                let mut in_whitespace = false;
                for c in t.trim().chars() {
                    let is_ws = c.is_whitespace();
                    if !is_ws {
                        len += UnicodeWidthChar::width(c).unwrap_or(0);
                        // Count the preceding whitespace as one.
                        if in_whitespace {
                            len += 1;
                        }
                    }
                    in_whitespace = is_ws;
                }
                // Add one for preceding whitespace.
                if let Some(true) = t.chars().next().map(|c| c.is_whitespace()) {
                    len += 1;
                }
                if let Img(_, _) = self.info {
                    len += 2;
                }
                SizeEstimate {
                    size: len,
                    min_width: len.min(context.min_wrap_width),
                    prefix_size: 0,
                }
            }

            Container(ref v) | Em(ref v) | Strong(ref v) | Strikeout(ref v) | Code(ref v)
            | Block(ref v) | Div(ref v) | Pre(ref v) | Dl(ref v) | Dt(ref v) | ListItem(ref v)
            | Sup(ref v) => v
                .iter()
                .map(recurse)
                .fold(Default::default(), SizeEstimate::add),
            Link(ref _target, ref v) => v
                .iter()
                .map(recurse)
                .fold(Default::default(), SizeEstimate::add)
                .add(SizeEstimate {
                    size: 5,
                    min_width: 5,
                    prefix_size: 0,
                }),
            Dd(ref v) | BlockQuote(ref v) | Ul(ref v) => {
                let prefix = match self.info {
                    Dd(_) => "  ".into(),
                    BlockQuote(_) => decorator.quote_prefix(),
                    Ul(_) => decorator.unordered_item_prefix(),
                    _ => unreachable!(),
                };
                let prefix_width = UnicodeWidthStr::width(prefix.as_str());
                let mut size = v
                    .iter()
                    .map(recurse)
                    .fold(Default::default(), SizeEstimate::add)
                    .add_hor(SizeEstimate {
                        size: prefix_width,
                        min_width: prefix_width,
                        prefix_size: 0,
                    });
                size.prefix_size = prefix_width;
                size
            }
            Ol(i, ref v) => {
                let prefix_size = calc_ol_prefix_size(i, v.len(), decorator);
                let mut result = v
                    .iter()
                    .map(recurse)
                    .fold(Default::default(), SizeEstimate::add)
                    .add_hor(SizeEstimate {
                        size: prefix_size,
                        min_width: prefix_size,
                        prefix_size: 0,
                    });
                result.prefix_size = prefix_size;
                result
            }
            Header(level, ref v) => {
                let prefix_size = decorator.header_prefix(level).len();
                let mut size = v
                    .iter()
                    .map(recurse)
                    .fold(Default::default(), SizeEstimate::add)
                    .add_hor(SizeEstimate {
                        size: prefix_size,
                        min_width: prefix_size,
                        prefix_size: 0,
                    });
                size.prefix_size = prefix_size;
                size
            }
            Break => SizeEstimate {
                size: 1,
                min_width: 1,
                prefix_size: 0,
            },
            Table(ref t) => t.calc_size_estimate(context),
            TableRow(..) | TableBody(_) | TableCell(_) => unimplemented!(),
            FragStart(_) => Default::default(),
            Coloured(_, ref v) => v
                .iter()
                .map(recurse)
                .fold(Default::default(), SizeEstimate::add),
        };
        self.size_estimate.set(Some(estimate));
        estimate
    }

    /// Return true if this node is definitely empty.  This is used to quickly
    /// remove e.g. links with no anchor text in most cases, but can't recurse
    /// and look more deeply.
    fn is_shallow_empty(&self) -> bool {
        use RenderNodeInfo::*;

        // Otherwise, make an estimate.
        match self.info {
            Text(ref t) | Img(_, ref t) => {
                let len = t.trim().len();
                len == 0
            }

            Container(ref v)
            | Link(_, ref v)
            | Em(ref v)
            | Strong(ref v)
            | Strikeout(ref v)
            | Code(ref v)
            | Block(ref v)
            | ListItem(ref v)
            | Div(ref v)
            | Pre(ref v)
            | BlockQuote(ref v)
            | Dl(ref v)
            | Dt(ref v)
            | Dd(ref v)
            | Ul(ref v)
            | Ol(_, ref v)
            | Sup(ref v) => v.is_empty(),
            Header(_level, ref v) => v.is_empty(),
            Break => true,
            Table(ref _t) => false,
            TableRow(..) | TableBody(_) | TableCell(_) => false,
            FragStart(_) => true,
            Coloured(_, ref v) => v.is_empty(),
        }
    }
}

fn precalc_size_estimate<'a, 'b: 'a, D: TextDecorator>(
    node: &'a RenderNode,
    context: &mut HtmlContext,
    decorator: &'b D,
) -> Result<TreeMapResult<'a, HtmlContext, &'a RenderNode, ()>> {
    use RenderNodeInfo::*;
    if node.size_estimate.get().is_some() {
        return Ok(TreeMapResult::Nothing);
    }
    Ok(match node.info {
        Text(_) | Img(_, _) | Break | FragStart(_) => {
            let _ = node.calc_size_estimate(context, decorator);
            TreeMapResult::Nothing
        }

        Container(ref v)
        | Link(_, ref v)
        | Em(ref v)
        | Strong(ref v)
        | Strikeout(ref v)
        | Code(ref v)
        | Block(ref v)
        | ListItem(ref v)
        | Div(ref v)
        | Pre(ref v)
        | BlockQuote(ref v)
        | Ul(ref v)
        | Ol(_, ref v)
        | Dl(ref v)
        | Dt(ref v)
        | Dd(ref v)
        | Sup(ref v)
        | Header(_, ref v) => TreeMapResult::PendingChildren {
            children: v.iter().collect(),
            cons: Box::new(move |context, _cs| {
                node.calc_size_estimate(context, decorator);
                Ok(None)
            }),
            prefn: None,
            postfn: None,
        },
        Table(ref t) => {
            /* Return all the indirect children which are RenderNodes. */
            let mut children = Vec::new();
            for row in &t.rows {
                for cell in &row.cells {
                    children.extend(cell.content.iter());
                }
            }
            TreeMapResult::PendingChildren {
                children,
                cons: Box::new(move |context, _cs| {
                    node.calc_size_estimate(context, decorator);
                    Ok(None)
                }),
                prefn: None,
                postfn: None,
            }
        }
        TableRow(..) | TableBody(_) | TableCell(_) => unimplemented!(),
        Coloured(_, ref v) => TreeMapResult::PendingChildren {
            children: v.iter().collect(),
            cons: Box::new(move |context, _cs| {
                node.calc_size_estimate(context, decorator);
                Ok(None)
            }),
            prefn: None,
            postfn: None,
        },
    })
}

/// Make a Vec of RenderNodes from the children of a node.
fn children_to_render_nodes<T: Write>(
    handle: Handle,
    err_out: &mut T,
    context: &mut HtmlContext,
) -> Result<Vec<RenderNode>> {
    /* process children, but don't add anything */
    handle
        .children
        .borrow()
        .iter()
        .flat_map(|ch| dom_to_render_tree_with_context(ch.clone(), err_out, context).transpose())
        .collect()
}

/// Make a Vec of DtElements from the `<dt>` and `<dd>` children of a node.
fn desc_list_children_to_render_nodes<T: Write>(
    handle: Handle,
    err_out: &mut T,
    context: &mut HtmlContext,
) -> Result<Vec<RenderNode>> {
    let mut children = Vec::new();

    for child in handle.children.borrow().iter() {
        match child.data {
            Element { ref name, .. } => match name.expanded() {
                expanded_name!(html "dt") => {
                    let dt_children = children_to_render_nodes(child.clone(), err_out, context)?;
                    children.push(RenderNode::new(RenderNodeInfo::Dt(dt_children)));
                }
                expanded_name!(html "dd") => {
                    let dd_children = children_to_render_nodes(child.clone(), err_out, context)?;
                    children.push(RenderNode::new(RenderNodeInfo::Dd(dd_children)));
                }
                _ => {}
            },
            Comment { .. } => {}
            _ => {
                html_trace!("Unhandled in list: {:?}\n", child);
            }
        }
    }
    Ok(children)
}

/// Convert a table into a RenderNode
fn table_to_render_tree<'a, T: Write>(
    handle: Handle,
    _err_out: &mut T,
) -> TreeMapResult<'a, HtmlContext, Handle, RenderNode> {
    pending(handle, |_, rowset| {
        let mut rows = vec![];
        for bodynode in rowset {
            if let RenderNodeInfo::TableBody(body) = bodynode.info {
                rows.extend(body);
            } else {
                html_trace!("Found in table: {:?}", bodynode.info);
            }
        }
        if rows.is_empty() {
            Ok(None)
        } else {
            Ok(Some(RenderNode::new(RenderNodeInfo::Table(
                RenderTable::new(rows),
            ))))
        }
    })
}

/// Add rows from a thead or tbody.
fn tbody_to_render_tree<'a, T: Write>(
    handle: Handle,
    _err_out: &mut T,
) -> TreeMapResult<'a, HtmlContext, Handle, RenderNode> {
    pending_noempty(handle, |_, rowchildren| {
        let mut rows = rowchildren
            .into_iter()
            .flat_map(|rownode| {
                if let RenderNodeInfo::TableRow(row, _) = rownode.info {
                    Some(row)
                } else {
                    html_trace!("  [[tbody child: {:?}]]", rownode);
                    None
                }
            })
            .collect::<Vec<_>>();

        // Handle colspan=0 by replacing it.
        // Get a list of (has_zero_colspan, sum_colspan)
        let num_columns = rows
            .iter()
            .map(|row| {
                row.cells()
                    // Treat the column as having colspan 1 for initial counting.
                    .map(|cell| (cell.colspan == 0, cell.colspan.max(1)))
                    .fold((false, 0), |a, b| (a.0 || b.0, a.1 + b.1))
            })
            .collect::<Vec<_>>();

        let max_columns = num_columns.iter().map(|(_, span)| span).max().unwrap_or(&1);

        for (i, &(has_zero, num_cols)) in num_columns.iter().enumerate() {
            // Note this won't be sensible if more than one column has colspan=0,
            // but that's not very well defined anyway.
            if has_zero {
                for cell in rows[i].cells_mut() {
                    if cell.colspan == 0 {
                        // +1 because we said it had 1 to start with
                        cell.colspan = max_columns - num_cols + 1;
                    }
                }
            }
        }

        Ok(Some(RenderNode::new(RenderNodeInfo::TableBody(rows))))
    })
}

/// Convert a table row to a RenderTableRow
fn tr_to_render_tree<'a, T: Write>(
    handle: Handle,
    _err_out: &mut T,
) -> TreeMapResult<'a, HtmlContext, Handle, RenderNode> {
    pending(handle, |_, cellnodes| {
        let cells = cellnodes
            .into_iter()
            .flat_map(|cellnode| {
                if let RenderNodeInfo::TableCell(cell) = cellnode.info {
                    Some(cell)
                } else {
                    html_trace!("  [[tr child: {:?}]]", cellnode);
                    None
                }
            })
            .collect();
        Ok(Some(RenderNode::new(RenderNodeInfo::TableRow(
            RenderTableRow {
                cells,
                col_sizes: None,
            },
            false,
        ))))
    })
}

/// Convert a single table cell to a render node.
fn td_to_render_tree<'a, T: Write>(
    handle: Handle,
    _err_out: &mut T,
) -> TreeMapResult<'a, HtmlContext, Handle, RenderNode> {
    let mut colspan = 1;
    if let Element { ref attrs, .. } = handle.data {
        for attr in attrs.borrow().iter() {
            if &attr.name.local == "colspan" {
                let v: &str = &attr.value;
                colspan = v.parse().unwrap_or(1);
            }
        }
    }
    pending(handle, move |_, children| {
        Ok(Some(RenderNode::new(RenderNodeInfo::TableCell(
            RenderTableCell {
                colspan,
                content: children,
                size_estimate: Cell::new(None),
                col_width: None,
            },
        ))))
    })
}

/// A reducer which combines results from mapping children into
/// the result for the current node.  Takes a context and a
/// vector of results and returns a new result (or nothing).
type ResultReducer<'a, C, R> = dyn FnOnce(&mut C, Vec<R>) -> Result<Option<R>> + 'a;

/// A closure to call before processing a child node.
type ChildPreFn<C, N> = dyn Fn(&mut C, &N) -> Result<()>;

/// A closure to call after processing a child node,
/// before adding the result to the processed results
/// vector.
type ChildPostFn<C, R> = dyn Fn(&mut C, &R) -> Result<()>;

/// The result of trying to render one node.
enum TreeMapResult<'a, C, N, R> {
    /// A completed result.
    Finished(R),
    /// Deferred completion - can be turned into a result
    /// once the vector of children are processed.
    PendingChildren {
        children: Vec<N>,
        cons: Box<ResultReducer<'a, C, R>>,
        prefn: Option<Box<ChildPreFn<C, N>>>,
        postfn: Option<Box<ChildPostFn<C, R>>>,
    },
    /// Nothing (e.g. a comment or other ignored element).
    Nothing,
}

impl<'a, C: 'a, N> TreeMapResult<'a, C, N, RenderNode> {
    #[cfg(feature = "css")]
    fn wrap_render_nodes<F>(self, f: F) -> Self
    where
        F: Fn(Vec<RenderNode>) -> RenderNodeInfo + 'a,
    {
        use TreeMapResult::*;
        match self {
            Finished(node) => Finished(RenderNode::new(f(vec![node]))),
            PendingChildren {
                children,
                cons,
                prefn,
                postfn,
            } => {
                PendingChildren {
                    children,
                    prefn,
                    postfn,
                    cons: Box::new(move |ctx, ch| {
                        Ok(cons(ctx, ch)?.map(|n| {
                            // Special case: lists need to recognise ListItem nodes as
                            // direct children.
                            match n.info {
                                RenderNodeInfo::ListItem(children) => {
                                    let wrapped = RenderNode::new(f(children));
                                    RenderNode::new(RenderNodeInfo::ListItem(vec![wrapped]))
                                }
                                RenderNodeInfo::TableCell(mut cellinfo) => {
                                    let children = cellinfo.content;
                                    let wrapped = RenderNode::new(f(children));
                                    cellinfo.content = vec![wrapped];
                                    RenderNode::new(RenderNodeInfo::TableCell(cellinfo))
                                }
                                RenderNodeInfo::TableRow(mut row, vert) => {
                                    let cells = row.cells;
                                    let cells = cells
                                        .into_iter()
                                        .map(|mut child| {
                                            let children = child.content;
                                            let wrapped = RenderNode::new(f(children));
                                            child.content = vec![wrapped];
                                            child
                                        })
                                        .collect();
                                    row.cells = cells;
                                    RenderNode::new(RenderNodeInfo::TableRow(row, vert))
                                }
                                ni => RenderNode::new(f(vec![RenderNode::new_styled(ni, n.style)])),
                            }
                        }))
                    }),
                }
            }
            Nothing => Nothing,
        }
    }
}

fn tree_map_reduce<'a, C, N, R, M>(
    context: &mut C,
    top: N,
    mut process_node: M,
) -> Result<Option<R>>
where
    M: for<'c> FnMut(&'c mut C, N) -> Result<TreeMapResult<'a, C, N, R>>,
{
    /// A node partially decoded, waiting for its children to
    /// be processed.
    struct PendingNode<'a, C, R, N> {
        /// How to make the node once finished
        construct: Box<ResultReducer<'a, C, R>>,
        /// Called before processing each child
        prefn: Option<Box<ChildPreFn<C, N>>>,
        /// Called after processing each child
        postfn: Option<Box<ChildPostFn<C, R>>>,
        /// Children already processed
        children: Vec<R>,
        /// Iterator of child nodes not yet processed
        to_process: std::vec::IntoIter<N>,
    }

    let mut last = PendingNode {
        // We only expect one child, which we'll just return.
        construct: Box::new(|_, mut cs| Ok(cs.pop())),
        prefn: None,
        postfn: None,
        children: Vec::new(),
        to_process: vec![top].into_iter(),
    };
    let mut pending_stack = Vec::new();
    loop {
        // Get the next child node to process
        if let Some(h) = last.to_process.next() {
            last.prefn
                .as_ref()
                .map(|ref f| f(context, &h))
                .transpose()?;
            match process_node(context, h)? {
                TreeMapResult::Finished(result) => {
                    last.postfn.as_ref().map(|ref f| f(context, &result));
                    last.children.push(result);
                }
                TreeMapResult::PendingChildren {
                    children,
                    cons,
                    prefn,
                    postfn,
                } => {
                    pending_stack.push(last);
                    last = PendingNode {
                        construct: cons,
                        prefn,
                        postfn,
                        children: Vec::new(),
                        to_process: children.into_iter(),
                    };
                }
                TreeMapResult::Nothing => {}
            };
        } else {
            // No more children, so finally construct the parent.
            let reduced = (last.construct)(context, last.children)?;
            let parent = pending_stack.pop();
            if let Some(node) = reduced {
                if let Some(mut parent) = parent {
                    parent.postfn.as_ref().map(|ref f| f(context, &node));
                    parent.children.push(node);
                    last = parent;
                } else {
                    // Finished the whole stack!
                    break Ok(Some(node));
                }
            } else {
                /* Finished the stack, and have nothing */
                match parent {
                    Some(parent) => last = parent,
                    None => break Ok(None),
                }
            }
        }
    }
}

#[derive(Default, Debug)]
struct HtmlContext {
    #[cfg(feature = "css")]
    style_data: css::StyleData,
    #[cfg(feature = "css")]
    use_doc_css: bool,

    max_wrap_width: Option<usize>,
    pad_block_width: bool,
    allow_width_overflow: bool,
    min_wrap_width: usize,
    raw: bool,
    draw_borders: bool,
    wrap_links: bool,
}

fn dom_to_render_tree_with_context<T: Write>(
    handle: Handle,
    err_out: &mut T,
    context: &mut HtmlContext,
) -> Result<Option<RenderNode>> {
    html_trace!("### dom_to_render_tree: HTML: {:?}", handle);
    #[cfg(feature = "css")]
    if context.use_doc_css {
        let mut doc_style_data = css::dom_to_stylesheet(handle.clone(), err_out)?;
        doc_style_data.merge(std::mem::take(&mut context.style_data));
        context.style_data = doc_style_data;
    }

    let result = tree_map_reduce(context, handle, |context, handle| {
        process_dom_node(handle, err_out, context)
    });

    html_trace!("### dom_to_render_tree: out= {:#?}", result);
    result
}

fn pending<'a, F>(handle: Handle, f: F) -> TreeMapResult<'a, HtmlContext, Handle, RenderNode>
where
    for<'r> F:
        Fn(&'r mut HtmlContext, std::vec::Vec<RenderNode>) -> Result<Option<RenderNode>> + 'static,
{
    TreeMapResult::PendingChildren {
        children: handle.children.borrow().clone(),
        cons: Box::new(f),
        prefn: None,
        postfn: None,
    }
}

fn pending_noempty<'a, F>(
    handle: Handle,
    f: F,
) -> TreeMapResult<'a, HtmlContext, Handle, RenderNode>
where
    for<'r> F:
        Fn(&'r mut HtmlContext, std::vec::Vec<RenderNode>) -> Result<Option<RenderNode>> + 'static,
{
    TreeMapResult::PendingChildren {
        children: handle.children.borrow().clone(),
        cons: Box::new(move |ctx, children| {
            if children.is_empty() {
                Ok(None)
            } else {
                f(ctx, children)
            }
        }),
        prefn: None,
        postfn: None,
    }
}

/// Prepend a FragmentStart (or analogous) marker to an existing
/// RenderNode.
fn prepend_marker(prefix: RenderNode, mut orig: RenderNode) -> RenderNode {
    use RenderNodeInfo::*;
    html_trace!("prepend_marker({:?}, {:?})", prefix, orig);

    match orig.info {
        // For block elements such as Block and Div, we need to insert
        // the node at the front of their children array, otherwise
        // the renderer is liable to drop the fragment start marker
        // _before_ the new line indicating the end of the previous
        // paragraph.
        //
        // For Container, we do the same thing just to make the data
        // less pointlessly nested.
        Block(ref mut children)
        | ListItem(ref mut children)
        | Div(ref mut children)
        | Pre(ref mut children)
        | BlockQuote(ref mut children)
        | Container(ref mut children)
        | TableCell(RenderTableCell {
            content: ref mut children,
            ..
        }) => {
            children.insert(0, prefix);
            // Now return orig, but we do that outside the match so
            // that we've given back the borrowed ref 'children'.
        }

        // For table rows and tables, push down if there's any content.
        TableRow(ref mut rrow, _) => {
            // If the row is empty, then there isn't really anything
            // to attach the fragment start to.
            if let Some(cell) = rrow.cells.first_mut() {
                cell.content.insert(0, prefix);
            }
        }

        TableBody(ref mut rows) | Table(RenderTable { ref mut rows, .. }) => {
            // If the row is empty, then there isn't really anything
            // to attach the fragment start to.
            if let Some(rrow) = rows.first_mut() {
                if let Some(cell) = rrow.cells.first_mut() {
                    cell.content.insert(0, prefix);
                }
            }
        }

        // For anything else, just make a new Container with the
        // prefix node and the original one.
        _ => {
            let result = RenderNode::new(Container(vec![prefix, orig]));
            html_trace!("prepend_marker() -> {:?}", result);
            return result;
        }
    }
    html_trace!("prepend_marker() -> {:?}", &orig);
    orig
}

fn process_dom_node<'a, T: Write>(
    handle: Handle,
    err_out: &mut T,
    #[allow(unused)] // Used with css feature
    context: &mut HtmlContext,
) -> Result<TreeMapResult<'a, HtmlContext, Handle, RenderNode>> {
    use RenderNodeInfo::*;
    use TreeMapResult::*;

    Ok(match handle.clone().data {
        Document => pending(handle, |_context, cs| {
            Ok(Some(RenderNode::new(Container(cs))))
        }),
        Comment { .. } => Nothing,
        Element {
            ref name,
            ref attrs,
            ..
        } => {
            let mut frag_from_name_attr = false;

            #[cfg(feature = "css")]
            let css_colour;
            #[cfg(feature = "css")]
            let computed = {
                let computed = context.style_data.computed_style(&handle, context.use_doc_css);
                if let Some(true) = computed.display_none {
                    return Ok(Nothing);
                }
                css_colour = computed.colour;
                computed
            };
            #[cfg(not(feature = "css"))]
            let computed = Default::default();

            let result = match name.expanded() {
                expanded_name!(html "html") | expanded_name!(html "body") => {
                    /* process children, but don't add anything */
                    pending(handle, move |_, cs| Ok(Some(RenderNode::new_styled(Container(cs), computed))))
                }
                expanded_name!(html "link")
                | expanded_name!(html "meta")
                | expanded_name!(html "hr")
                | expanded_name!(html "script")
                | expanded_name!(html "style")
                | expanded_name!(html "head") => {
                    /* Ignore the head and its children */
                    Nothing
                }
                expanded_name!(html "span") => {
                    /* process children, but don't add anything */
                    pending_noempty(handle, move |_, cs| {
                        Ok(Some(RenderNode::new_styled(Container(cs), computed)))
                    })
                }
                expanded_name!(html "a") => {
                    let borrowed = attrs.borrow();
                    let mut target = None;
                    frag_from_name_attr = true;
                    for attr in borrowed.iter() {
                        if &attr.name.local == "href" {
                            target = Some(&*attr.value);
                            break;
                        }
                    }
                    PendingChildren {
                        children: handle.children.borrow().clone(),
                        cons: if let Some(href) = target {
                            let href: String = href.into();
                            Box::new(move |_, cs: Vec<RenderNode>| {
                                if cs.iter().any(|c| !c.is_shallow_empty()) {
                                    Ok(Some(RenderNode::new(Link(href, cs))))
                                } else {
                                    Ok(None)
                                }
                            })
                        } else {
                            Box::new(|_, cs| Ok(Some(RenderNode::new(Container(cs)))))
                        },
                        prefn: None,
                        postfn: None,
                    }
                }
                expanded_name!(html "em")
                | expanded_name!(html "i")
                | expanded_name!(html "ins") => {
                    pending(handle, |_, cs| Ok(Some(RenderNode::new(Em(cs)))))
                }
                expanded_name!(html "strong") => {
                    pending(handle, |_, cs| Ok(Some(RenderNode::new(Strong(cs)))))
                }
                expanded_name!(html "s") | expanded_name!(html "del") => {
                    pending(handle, |_, cs| Ok(Some(RenderNode::new(Strikeout(cs)))))
                }
                expanded_name!(html "code") => {
                    pending(handle, |_, cs| Ok(Some(RenderNode::new(Code(cs)))))
                }
                expanded_name!(html "img") => {
                    let borrowed = attrs.borrow();
                    let mut title = None;
                    let mut src = None;
                    for attr in borrowed.iter() {
                        if &attr.name.local == "alt" && !attr.value.is_empty() {
                            title = Some(&*attr.value);
                        }
                        if &attr.name.local == "src" && !attr.value.is_empty() {
                            src = Some(&*attr.value);
                        }
                        if title.is_some() && src.is_some() {
                            break;
                        }
                    }
                    if let (Some(title), Some(src)) = (title, src) {
                        Finished(RenderNode::new(Img(src.into(), title.into())))
                    } else {
                        Nothing
                    }
                }
                expanded_name!(html "h1")
                | expanded_name!(html "h2")
                | expanded_name!(html "h3")
                | expanded_name!(html "h4") => {
                    let level: usize = name.local[1..].parse().unwrap();
                    pending(handle, move |_, cs| {
                        Ok(Some(RenderNode::new(Header(level, cs))))
                    })
                }
                expanded_name!(html "p") => {
                    pending_noempty(handle, |_, cs| Ok(Some(RenderNode::new(Block(cs)))))
                }
                expanded_name!(html "li") => {
                    pending(handle, |_, cs| Ok(Some(RenderNode::new(ListItem(cs)))))
                }
                expanded_name!(html "sup") => {
                    pending(handle, |_, cs| Ok(Some(RenderNode::new(Sup(cs)))))
                }
                expanded_name!(html "div") => {
                    pending_noempty(handle, |_, cs| Ok(Some(RenderNode::new(Div(cs)))))
                }
                expanded_name!(html "pre") => {
                    pending(handle, |_, cs| Ok(Some(RenderNode::new(Pre(cs)))))
                }
                expanded_name!(html "br") => Finished(RenderNode::new(Break)),
                expanded_name!(html "table") => table_to_render_tree(handle.clone(), err_out),
                expanded_name!(html "thead") | expanded_name!(html "tbody") => {
                    tbody_to_render_tree(handle.clone(), err_out)
                }
                expanded_name!(html "tr") => tr_to_render_tree(handle.clone(), err_out),
                expanded_name!(html "th") | expanded_name!(html "td") => {
                    td_to_render_tree(handle.clone(), err_out)
                }
                expanded_name!(html "blockquote") => {
                    pending_noempty(handle, |_, cs| Ok(Some(RenderNode::new(BlockQuote(cs)))))
                }
                expanded_name!(html "ul") => {
                    pending_noempty(handle, |_, cs| Ok(Some(RenderNode::new(Ul(cs)))))
                }
                expanded_name!(html "ol") => {
                    let borrowed = attrs.borrow();
                    let mut start = 1;
                    for attr in borrowed.iter() {
                        if &attr.name.local == "start" {
                            start = attr.value.parse().ok().unwrap_or(1);
                            break;
                        }
                    }

                    pending_noempty(handle, move |_, cs| {
                        let cs = cs
                            .into_iter()
                            .filter(|n| matches!(n.info, RenderNodeInfo::ListItem(..)))
                            .collect();
                        Ok(Some(RenderNode::new(Ol(start, cs))))
                    })
                }
                expanded_name!(html "dl") => Finished(RenderNode::new(Dl(
                    desc_list_children_to_render_nodes(handle.clone(), err_out, context)?,
                ))),
                _ => {
                    html_trace!("Unhandled element: {:?}\n", name.local);
                    pending_noempty(handle, |_, cs| Ok(Some(RenderNode::new(Container(cs)))))
                    //None
                }
            };

            let mut fragment = None;
            let borrowed = attrs.borrow();
            for attr in borrowed.iter() {
                if &attr.name.local == "id" || (frag_from_name_attr && &attr.name.local == "name") {
                    fragment = Some(attr.value.to_string());
                    break;
                }
            }

            let result = if let Some(fragname) = fragment {
                match result {
                    Finished(node) => {
                        Finished(prepend_marker(RenderNode::new(FragStart(fragname)), node))
                    }
                    Nothing => Finished(RenderNode::new(FragStart(fragname))),
                    PendingChildren {
                        children,
                        cons,
                        prefn,
                        postfn,
                    } => PendingChildren {
                        children,
                        prefn,
                        postfn,
                        cons: Box::new(move |ctx, ch| {
                            let fragnode = RenderNode::new(FragStart(fragname));
                            match cons(ctx, ch)? {
                                None => Ok(Some(fragnode)),
                                Some(node) => Ok(Some(prepend_marker(fragnode, node))),
                            }
                        }),
                    },
                }
            } else {
                result
            };

            #[cfg(feature = "css")]
            let result = if let Some(colour) = css_colour {
                result.wrap_render_nodes(move |children| Coloured(colour, children))
            } else {
                result
            };

            result
        }
        markup5ever_rcdom::NodeData::Text { contents: ref tstr } => {
            Finished(RenderNode::new(Text((&*tstr.borrow()).into())))
        }
        _ => {
            // NodeData doesn't have a Debug impl.
            writeln!(err_out, "Unhandled node type.").unwrap();
            Nothing
        }
    })
}

fn render_tree_to_string<T: Write, D: TextDecorator>(
    context: &mut HtmlContext,
    renderer: SubRenderer<D>,
    decorator: &D,
    tree: RenderNode,
    err_out: &mut T,
) -> Result<SubRenderer<D>> {
    /* Phase 1: get size estimates. */
    tree_map_reduce(context, &tree, |context, node| {
        precalc_size_estimate(node, context, decorator)
    })?;
    /* Phase 2: actually render. */
    let mut renderer = TextRenderer::new(renderer);
    tree_map_reduce(&mut renderer, tree, |renderer, node| {
        do_render_node(renderer, node, err_out)
    })?;
    let (mut renderer, links) = renderer.into_inner();
    let lines = renderer.finalise(links);
    // And add the links
    if !lines.is_empty() {
        renderer.start_block()?;
        renderer.fmt_links(lines);
    }
    Ok(renderer)
}

fn pending2<
    'a,
    D: TextDecorator,
    F: FnOnce(
            &mut TextRenderer<D>,
            Vec<Option<SubRenderer<D>>>,
        ) -> Result<Option<Option<SubRenderer<D>>>>
        + 'static,
>(
    children: Vec<RenderNode>,
    f: F,
) -> TreeMapResult<'a, TextRenderer<D>, RenderNode, Option<SubRenderer<D>>> {
    TreeMapResult::PendingChildren {
        children,
        cons: Box::new(f),
        prefn: None,
        postfn: None,
    }
}

/// Keep track of what style state has been applied to a renderer so that we
/// can undo it.
#[derive(Default)]
struct PushedStyleInfo {
    bgcolour: bool,
}

impl PushedStyleInfo {
    fn apply<D: TextDecorator>(render: &mut TextRenderer<D>, style: &css::ComputedStyle) -> Self {
        let mut result: PushedStyleInfo = Default::default();
        if let Some(col) = style.bg_colour {
            render.push_bgcolour(col);
            result.bgcolour = true;
        }
        result
    }
    fn unwind<D: TextDecorator>(self, renderer: &mut TextRenderer<D>) {
        if self.bgcolour {
            renderer.pop_bgcolour();
        }
    }
}

fn do_render_node<T: Write, D: TextDecorator>(
    renderer: &mut TextRenderer<D>,
    tree: RenderNode,
    err_out: &mut T,
) -> Result<TreeMapResult<'static, TextRenderer<D>, RenderNode, Option<SubRenderer<D>>>> {
    html_trace!("do_render_node({:?}", tree);
    use RenderNodeInfo::*;
    use TreeMapResult::*;

    let size_estimate = tree.size_estimate.get().unwrap_or_default();

    let pushed_style = PushedStyleInfo::apply(renderer, &tree.style);

    Ok(match tree.info {
        Text(ref tstr) => {
            renderer.add_inline_text(tstr)?;
            pushed_style.unwind(renderer);
            Finished(None)
        }
        Container(children) =>
            pending2(children, |renderer, _| {
                pushed_style.unwind(renderer);
                Ok(Some(None))
            }),
        Link(href, children) => {
            renderer.start_link(&href)?;
            pending2(children, move |renderer: &mut TextRenderer<D>, _| {
                renderer.end_link()?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Em(children) => {
            renderer.start_emphasis()?;
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.end_emphasis()?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Strong(children) => {
            renderer.start_strong()?;
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.end_strong()?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Strikeout(children) => {
            renderer.start_strikeout()?;
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.end_strikeout()?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Code(children) => {
            renderer.start_code()?;
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.end_code()?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Img(src, title) => {
            renderer.add_image(&src, &title)?;
            pushed_style.unwind(renderer);
            Finished(None)
        }
        Block(children) | ListItem(children) => {
            renderer.start_block()?;
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.end_block();
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Header(level, children) => {
            let prefix = renderer.header_prefix(level);
            let prefix_size = size_estimate.prefix_size;
            debug_assert!(prefix.len() == prefix_size);
            let min_width = size_estimate.min_width;
            let inner_width = min_width.saturating_sub(prefix_size);
            let sub_builder =
                renderer.new_sub_renderer(renderer.width_minus(prefix_size, inner_width)?)?;
            renderer.push(sub_builder);
            pending2(children, move |renderer: &mut TextRenderer<D>, _| {
                let sub_builder = renderer.pop();

                renderer.start_block()?;
                renderer.append_subrender(sub_builder, repeat(&prefix[..]))?;
                renderer.end_block();
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Div(children) => {
            renderer.new_line()?;
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.new_line()?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Pre(children) => {
            renderer.new_line()?;
            renderer.start_pre();
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.new_line()?;
                renderer.end_pre();
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        BlockQuote(children) => {
            let prefix = renderer.quote_prefix();
            debug_assert!(size_estimate.prefix_size == prefix.len());
            let inner_width = size_estimate.min_width - prefix.len();
            let sub_builder =
                renderer.new_sub_renderer(renderer.width_minus(prefix.len(), inner_width)?)?;
            renderer.push(sub_builder);
            pending2(children, move |renderer: &mut TextRenderer<D>, _| {
                let sub_builder = renderer.pop();

                renderer.start_block()?;
                renderer.append_subrender(sub_builder, repeat(&prefix[..]))?;
                renderer.end_block();
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Ul(items) => {
            renderer.start_block()?;

            let prefix = renderer.unordered_item_prefix();
            let prefix_len = prefix.len();

            TreeMapResult::PendingChildren {
                children: items,
                cons: Box::new(|renderer, _| {
                    pushed_style.unwind(renderer);
                    Ok(Some(None))
                }),
                prefn: Some(Box::new(move |renderer: &mut TextRenderer<D>, _| {
                    let inner_width = size_estimate.min_width - prefix_len;
                    let sub_builder = renderer
                        .new_sub_renderer(renderer.width_minus(prefix_len, inner_width)?)?;
                    renderer.push(sub_builder);
                    Ok(())
                })),
                postfn: Some(Box::new(move |renderer: &mut TextRenderer<D>, _| {
                    let sub_builder = renderer.pop();

                    let indent = " ".repeat(prefix.len());

                    renderer.append_subrender(
                        sub_builder,
                        once(&prefix[..]).chain(repeat(&indent[..])),
                    )?;
                    Ok(())
                })),
            }
        }
        Ol(start, items) => {
            renderer.start_block()?;

            let num_items = items.len();

            // The prefix width could be at either end if the start is negative.
            let min_number = start;
            // Assumption: num_items can't overflow isize.
            let max_number = start + (num_items as i64) - 1;
            let prefix_width_min = renderer.ordered_item_prefix(min_number).len();
            let prefix_width_max = renderer.ordered_item_prefix(max_number).len();
            let prefix_width = max(prefix_width_min, prefix_width_max);
            let prefixn = format!("{: <width$}", "", width = prefix_width);
            let i: Cell<_> = Cell::new(start);

            TreeMapResult::PendingChildren {
                children: items,
                cons: Box::new(|renderer, _| {
                    pushed_style.unwind(renderer);
                    Ok(Some(None))
                }),
                prefn: Some(Box::new(move |renderer: &mut TextRenderer<D>, _| {
                    let inner_min = size_estimate.min_width - size_estimate.prefix_size;
                    let sub_builder = renderer
                        .new_sub_renderer(renderer.width_minus(prefix_width, inner_min)?)?;
                    renderer.push(sub_builder);
                    Ok(())
                })),
                postfn: Some(Box::new(move |renderer: &mut TextRenderer<D>, _| {
                    let sub_builder = renderer.pop();
                    let prefix1 = renderer.ordered_item_prefix(i.get());
                    let prefix1 = format!("{: <width$}", prefix1, width = prefix_width);

                    renderer.append_subrender(
                        sub_builder,
                        once(prefix1.as_str()).chain(repeat(prefixn.as_str())),
                    )?;
                    i.set(i.get() + 1);
                    Ok(())
                })),
            }
        }
        Dl(items) => {
            renderer.start_block()?;

            TreeMapResult::PendingChildren {
                children: items,
                cons: Box::new(|renderer, _| {
                    pushed_style.unwind(renderer);
                    Ok(Some(None))
                }),
                prefn: None,
                postfn: None,
            }
        }
        Dt(children) => {
            renderer.new_line()?;
            renderer.start_emphasis()?;
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.end_emphasis()?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Dd(children) => {
            let inner_min = size_estimate.min_width - 2;
            let sub_builder = renderer.new_sub_renderer(renderer.width_minus(2, inner_min)?)?;
            renderer.push(sub_builder);
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                let sub_builder = renderer.pop();
                renderer.append_subrender(sub_builder, repeat("  "))?;
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Break => {
            renderer.new_line_hard()?;
            pushed_style.unwind(renderer);
            Finished(None)
        }
        Table(tab) => render_table_tree(renderer, tab, err_out)?,
        TableRow(row, false) => render_table_row(renderer, row, err_out),
        TableRow(row, true) => render_table_row_vert(renderer, row, err_out),
        TableBody(_) => unimplemented!("Unexpected TableBody while rendering"),
        TableCell(cell) => render_table_cell(renderer, cell, err_out),
        FragStart(fragname) => {
            renderer.record_frag_start(&fragname);
            pushed_style.unwind(renderer);
            Finished(None)
        }
        Coloured(colour, children) => {
            renderer.push_colour(colour);
            pending2(children, |renderer: &mut TextRenderer<D>, _| {
                renderer.pop_colour();
                pushed_style.unwind(renderer);
                Ok(Some(None))
            })
        }
        Sup(children) => {
            // Special case for digit-only superscripts - use superscript
            // characters.
            fn sup_digits(children: &[RenderNode]) -> Option<String> {
                if children.len() != 1 {
                    return None;
                }
                if let Text(s) = &children[0].info {
                    if s.chars().all(|d| d.is_ascii_digit()) {
                        // It's just a string of digits - replace by superscript characters.
                        const SUPERSCRIPTS: [char; 10] =
                            ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'];
                        return Some(
                            s.bytes()
                                .map(|b| SUPERSCRIPTS[(b - b'0') as usize])
                                .collect(),
                        );
                    }
                }
                None
            }
            if let Some(digitstr) = sup_digits(&children) {
                renderer.add_inline_text(&digitstr)?;
                pushed_style.unwind(renderer);
                Finished(None)
            } else {
                renderer.start_superscript()?;
                pending2(children, |renderer: &mut TextRenderer<D>, _| {
                    renderer.end_superscript()?;
                    pushed_style.unwind(renderer);
                    Ok(Some(None))
                })
            }
        }
    })
}

fn render_table_tree<T: Write, D: TextDecorator>(
    renderer: &mut TextRenderer<D>,
    table: RenderTable,
    _err_out: &mut T,
) -> Result<TreeMapResult<'static, TextRenderer<D>, RenderNode, Option<SubRenderer<D>>>> {
    /* Now lay out the table. */
    let num_columns = table.num_columns;

    /* Heuristic: scale the column widths according to how much content there is. */
    let mut col_sizes: Vec<SizeEstimate> = vec![Default::default(); num_columns];

    for row in table.rows() {
        let mut colno = 0;
        for cell in row.cells() {
            // FIXME: get_size_estimate is still recursive.
            let mut estimate = cell.get_size_estimate();

            // If the cell has a colspan>1, then spread its size between the
            // columns.
            estimate.size /= cell.colspan;
            estimate.min_width /= cell.colspan;
            for i in 0..cell.colspan {
                col_sizes[colno + i] = (col_sizes[colno + i]).max(estimate);
            }
            colno += cell.colspan;
        }
    }
    // TODO: remove empty columns
    let tot_size: usize = col_sizes.iter().map(|est| est.size).sum();
    let min_size: usize = col_sizes.iter().map(|est| est.min_width).sum::<usize>()
        + col_sizes.len().saturating_sub(1);
    let width = renderer.width();

    let vert_row = renderer.options.raw || (min_size > width || width == 0);

    let mut col_widths: Vec<usize> = if !vert_row {
        col_sizes
            .iter()
            .map(|sz| {
                if sz.size == 0 {
                    0
                } else {
                    min(
                        sz.size,
                        if usize::MAX / width <= sz.size {
                            // The provided width is too large to multiply by width,
                            // so do it the other way around.
                            max((width / tot_size) * sz.size, sz.min_width)
                        } else {
                            max(sz.size * width / tot_size, sz.min_width)
                        },
                    )
                }
            })
            .collect()
    } else {
        col_sizes.iter().map(|_| width).collect()
    };

    if !vert_row {
        let num_cols = col_widths.len();
        if num_cols > 0 {
            loop {
                let cur_width = col_widths.iter().sum::<usize>() + num_cols - 1;
                if cur_width <= width {
                    break;
                }
                let (i, _) = col_widths
                    .iter()
                    .enumerate()
                    .max_by_key(|&(colno, width)| {
                        (
                            width.saturating_sub(col_sizes[colno].min_width),
                            width,
                            usize::max_value() - colno,
                        )
                    })
                    .unwrap();
                col_widths[i] -= 1;
            }
        }
    }

    let table_width = if vert_row {
        width
    } else {
        col_widths.iter().cloned().sum::<usize>()
            + col_widths
                .iter()
                .filter(|&w| w > &0)
                .count()
                .saturating_sub(1)
    };

    renderer.start_block()?;

    if table_width != 0 && renderer.options.draw_borders {
        renderer.add_horizontal_border_width(table_width)?;
    }

    Ok(TreeMapResult::PendingChildren {
        children: table.into_rows(col_widths, vert_row),
        cons: Box::new(|_, _| Ok(Some(None))),
        prefn: Some(Box::new(|_, _| Ok(()))),
        postfn: Some(Box::new(|_, _| Ok(()))),
    })
}

fn render_table_row<T: Write, D: TextDecorator>(
    _renderer: &mut TextRenderer<D>,
    row: RenderTableRow,
    _err_out: &mut T,
) -> TreeMapResult<'static, TextRenderer<D>, RenderNode, Option<SubRenderer<D>>> {
    TreeMapResult::PendingChildren {
        children: row.into_cells(false),
        cons: Box::new(|builders, children| {
            let children: Vec<_> = children.into_iter().map(Option::unwrap).collect();
            if children.iter().any(|c| !c.empty()) {
                builders.append_columns_with_borders(children, true)?;
            }
            Ok(Some(None))
        }),
        prefn: Some(Box::new(|renderer: &mut TextRenderer<D>, node| {
            if let RenderNodeInfo::TableCell(ref cell) = node.info {
                let sub_builder = renderer.new_sub_renderer(cell.col_width.unwrap())?;
                renderer.push(sub_builder);
                Ok(())
            } else {
                panic!()
            }
        })),
        postfn: Some(Box::new(|_renderer: &mut TextRenderer<D>, _| Ok(()))),
    }
}

fn render_table_row_vert<T: Write, D: TextDecorator>(
    _renderer: &mut TextRenderer<D>,
    row: RenderTableRow,
    _err_out: &mut T,
) -> TreeMapResult<'static, TextRenderer<D>, RenderNode, Option<SubRenderer<D>>> {
    TreeMapResult::PendingChildren {
        children: row.into_cells(true),
        cons: Box::new(|builders, children| {
            let children: Vec<_> = children.into_iter().map(Option::unwrap).collect();
            builders.append_vert_row(children)?;
            Ok(Some(None))
        }),
        prefn: Some(Box::new(|renderer: &mut TextRenderer<D>, node| {
            if let RenderNodeInfo::TableCell(ref cell) = node.info {
                let sub_builder = renderer.new_sub_renderer(cell.col_width.unwrap())?;
                renderer.push(sub_builder);
                Ok(())
            } else {
                Err(Error::Fail)
            }
        })),
        postfn: Some(Box::new(|_renderer: &mut TextRenderer<D>, _| Ok(()))),
    }
}

fn render_table_cell<T: Write, D: TextDecorator>(
    _renderer: &mut TextRenderer<D>,
    cell: RenderTableCell,
    _err_out: &mut T,
) -> TreeMapResult<'static, TextRenderer<D>, RenderNode, Option<SubRenderer<D>>> {
    pending2(cell.content, |renderer: &mut TextRenderer<D>, _| {
        let sub_builder = renderer.pop();
        Ok(Some(Some(sub_builder)))
    })
}

pub mod config {
    //! Configure the HTML to text translation using the `Config` type, which can be
    //! constructed using one of the functions in this module.

    use super::{Discard, Error};
    #[cfg(feature = "css")]
    use crate::css::StyleData;
    use crate::{
        render::text_renderer::{
            PlainDecorator, RichAnnotation, RichDecorator, TaggedLine, TextDecorator,
        },
        HtmlContext, RenderTree, Result, MIN_WIDTH,
    };

    /// Configure the HTML processing.
    pub struct Config<D: TextDecorator> {
        decorator: D,

        max_wrap_width: Option<usize>,

        #[cfg(feature = "css")]
        style: StyleData,
        #[cfg(feature = "css")]
        use_doc_css: bool,

        pad_block_width: bool,

        allow_width_overflow: bool,
        min_wrap_width: usize,
        raw: bool,
        draw_borders: bool,
        wrap_links: bool,
    }

    impl<D: TextDecorator> Config<D> {
        /// Make the HtmlContext from self.
        fn make_context(&self) -> HtmlContext {
            HtmlContext {
                #[cfg(feature = "css")]
                style_data: self.style.clone(),
                #[cfg(feature = "css")]
                use_doc_css: self.use_doc_css,

                max_wrap_width: self.max_wrap_width,
                pad_block_width: self.pad_block_width,
                allow_width_overflow: self.allow_width_overflow,
                min_wrap_width: self.min_wrap_width,
                raw: self.raw,
                draw_borders: self.draw_borders,
                wrap_links: self.wrap_links,
            }
        }
        /// Parse with context.
        fn do_parse<R: std::io::Read>(
            &mut self,
            context: &mut HtmlContext,
            input: R,
        ) -> Result<RenderTree> {
            super::parse_with_context(input, context)
        }

        /// Parse the HTML into a DOM structure.
        pub fn parse_html<R: std::io::Read>(&self, mut input: R) -> Result<super::RcDom> {
            use html5ever::tendril::TendrilSink;
            let opts = super::ParseOpts {
                tree_builder: super::TreeBuilderOpts {
                    drop_doctype: true,
                    ..Default::default()
                },
                ..Default::default()
            };
            Ok(super::parse_document(super::RcDom::default(), opts)
                .from_utf8()
                .read_from(&mut input)?)
        }

        /// Convert an HTML DOM into a RenderTree.
        pub fn dom_to_render_tree(&self, dom: &super::RcDom) -> Result<RenderTree> {
            Ok(RenderTree(
                super::dom_to_render_tree_with_context(
                    dom.document.clone(),
                    &mut Discard {},
                    &mut self.make_context(),
                )?
                .ok_or(Error::Fail)?,
            ))
        }

        /// Render an existing RenderTree into a string.
        pub fn render_to_string(&self, render_tree: RenderTree, width: usize) -> Result<String> {
            render_tree
                .render_with_context(
                    &mut self.make_context(),
                    width,
                    self.decorator.make_subblock_decorator(),
                )?
                .into_string()
        }

        /// Take an existing RenderTree, and returns text wrapped to `width` columns.
        /// The text is returned as a `Vec<TaggedLine<_>>`; the annotations are vectors
        /// of the provided text decorator's `Annotation`.  The "outer" annotation comes first in
        /// the `Vec`.
        pub fn render_to_lines(
            &self,
            render_tree: RenderTree,
            width: usize,
        ) -> Result<Vec<TaggedLine<Vec<D::Annotation>>>> {
            render_tree
                .render_with_context(
                    &mut self.make_context(),
                    width,
                    self.decorator.make_subblock_decorator(),
                )?
                .into_lines()
        }

        /// Reads HTML from `input`, and returns a `String` with text wrapped to
        /// `width` columns.
        pub fn string_from_read<R: std::io::Read>(
            mut self,
            input: R,
            width: usize,
        ) -> Result<String> {
            let mut context = self.make_context();
            self.do_parse(&mut context, input)?
                .render_with_context(&mut context, width, self.decorator)?
                .into_string()
        }

        /// Reads HTML from `input`, and returns text wrapped to `width` columns.
        /// The text is returned as a `Vec<TaggedLine<_>>`; the annotations are vectors
        /// of the provided text decorator's `Annotation`.  The "outer" annotation comes first in
        /// the `Vec`.
        pub fn lines_from_read<R: std::io::Read>(
            mut self,
            input: R,
            width: usize,
        ) -> Result<Vec<TaggedLine<Vec<D::Annotation>>>> {
            let mut context = self.make_context();
            self.do_parse(&mut context, input)?
                .render_with_context(&mut context, width, self.decorator)?
                .into_lines()
        }

        #[cfg(feature = "css")]
        /// Add some CSS rules which will be used (if supported) with any
        /// HTML processed.
        pub fn add_css(mut self, css: &str) -> Result<Self> {
            self.style.add_user_css(css)?;
            Ok(self)
        }

        #[cfg(feature = "css")]
        /// Parse CSS from any \<style\> elements and use supported rules.
        pub fn use_doc_css(mut self) -> Self {
            self.use_doc_css = true;
            self
        }

        /// Pad lines out to the full render width.
        pub fn pad_block_width(mut self) -> Self {
            self.pad_block_width = true;
            self
        }

        /// Set the maximum text wrap width.
        /// When set, paragraphs will be wrapped to that width even if there
        /// is more total width available for rendering.
        pub fn max_wrap_width(mut self, wrap_width: usize) -> Self {
            self.max_wrap_width = Some(wrap_width);
            self
        }

        /// Allow the output to be wider than the max width.  When enabled,
        /// then output wider than the specified width will be returned
        /// instead of returning `Err(TooNarrow)` if the output wouldn't
        /// otherwise fit.
        pub fn allow_width_overflow(mut self) -> Self {
            self.allow_width_overflow = true;
            self
        }

        /// Set the minimum width for text wrapping.  The default is 3.
        /// Blocks of text will be forced to have at least this width
        /// (unless the text inside is less than that).  Increasing this
        /// can increase the chance that the width will overflow, leading
        /// to a TooNarrow error unless `allow_width_overflow()` is set.
        pub fn min_wrap_width(mut self, min_wrap_width: usize) -> Self {
            self.min_wrap_width = min_wrap_width;
            self
        }

        /// Raw extraction, ensures text in table cells ends up rendered together
        /// This traverses tables as if they had a single column and every cell is its own row.
        /// Implies `no_table_borders()`
        pub fn raw_mode(mut self, raw: bool) -> Self {
            self.raw = raw;
            self.draw_borders = false;
            self
        }

        /// Do not render table borders
        pub fn no_table_borders(mut self) -> Self {
            self.draw_borders = false;
            self
        }
        /// Do not wrap links
        pub fn no_link_wrapping(mut self) -> Self {
            self.wrap_links = false;
            self
        }
    }

    impl Config<RichDecorator> {
        /// Return coloured text.  `colour_map` is a function which takes
        /// a list of `RichAnnotation` and some text, and returns the text
        /// with any terminal escapes desired to indicate those annotations
        /// (such as colour).
        pub fn coloured<R, FMap>(
            mut self,
            input: R,
            width: usize,
            colour_map: FMap,
        ) -> Result<String>
        where
            R: std::io::Read,
            FMap: Fn(&[RichAnnotation], &str) -> String,
        {
            let mut context = self.make_context();
            let lines = self
                .do_parse(&mut context, input)?
                .render_with_context(&mut context, width, self.decorator)?
                .into_lines()?;

            let mut result = String::new();
            for line in lines {
                for ts in line.tagged_strings() {
                    result.push_str(&colour_map(&ts.tag, &ts.s));
                }
                result.push('\n');
            }
            Ok(result)
        }

        /// Return coloured text from a RenderTree.  `colour_map` is a function which takes a list
        /// of `RichAnnotation` and some text, and returns the text with any terminal escapes
        /// desired to indicate those annotations (such as colour).
        pub fn render_coloured<FMap>(
            &self,
            render_tree: RenderTree,
            width: usize,
            colour_map: FMap,
        ) -> Result<String>
        where
            FMap: Fn(&[RichAnnotation], &str) -> String,
        {
            let lines = self.render_to_lines(render_tree, width)?;

            let mut result = String::new();
            for line in lines {
                for ts in line.tagged_strings() {
                    result.push_str(&colour_map(&ts.tag, &ts.s));
                }
                result.push('\n');
            }
            Ok(result)
        }
    }

    /// Return a Config initialized with a `RichDecorator`.
    pub fn rich() -> Config<RichDecorator> {
        Config {
            decorator: RichDecorator::new(),
            #[cfg(feature = "css")]
            style: Default::default(),
            #[cfg(feature = "css")]
            use_doc_css: false,
            max_wrap_width: None,
            pad_block_width: false,
            allow_width_overflow: false,
            min_wrap_width: MIN_WIDTH,
            raw: false,
            draw_borders: true,
            wrap_links: true,
        }
    }

    /// Return a Config initialized with a `PlainDecorator`.
    pub fn plain() -> Config<PlainDecorator> {
        Config {
            decorator: PlainDecorator::new(),
            #[cfg(feature = "css")]
            style: Default::default(),
            #[cfg(feature = "css")]
            use_doc_css: false,
            max_wrap_width: None,
            pad_block_width: false,
            allow_width_overflow: false,
            min_wrap_width: MIN_WIDTH,
            raw: false,
            draw_borders: true,
            wrap_links: true,
        }
    }

    /// Return a Config initialized with a custom decorator.
    pub fn with_decorator<D: TextDecorator>(decorator: D) -> Config<D> {
        Config {
            decorator,
            #[cfg(feature = "css")]
            style: Default::default(),
            #[cfg(feature = "css")]
            use_doc_css: false,
            max_wrap_width: None,
            pad_block_width: false,
            allow_width_overflow: false,
            min_wrap_width: MIN_WIDTH,
            raw: false,
            draw_borders: true,
            wrap_links: true,
        }
    }
}

/// The structure of an HTML document that can be rendered using a [`TextDecorator`][].
///
/// [`TextDecorator`]: render/text_renderer/trait.TextDecorator.html

#[derive(Clone, Debug)]
pub struct RenderTree(RenderNode);

impl RenderTree {
    /// Render this document using the given `decorator` and wrap it to `width` columns.
    fn render_with_context<D: TextDecorator>(
        self,
        context: &mut HtmlContext,
        width: usize,
        decorator: D,
    ) -> Result<RenderedText<D>> {
        if width == 0 {
            return Err(Error::TooNarrow);
        }
        let render_options = RenderOptions {
            wrap_width: context.max_wrap_width,
            pad_block_width: context.pad_block_width,
            allow_width_overflow: context.allow_width_overflow,
            raw: context.raw,
            draw_borders: context.draw_borders,
            wrap_links: context.wrap_links,
        };
        let test_decorator = decorator.make_subblock_decorator();
        let builder = SubRenderer::new(width, render_options, decorator);
        let builder =
            render_tree_to_string(context, builder, &test_decorator, self.0, &mut Discard {})?;
        Ok(RenderedText(builder))
    }

    /// Render this document using the given `decorator` and wrap it to `width` columns.
    fn render<D: TextDecorator>(self, width: usize, decorator: D) -> Result<RenderedText<D>> {
        self.render_with_context(&mut Default::default(), width, decorator)
    }
}

/// A rendered HTML document.
struct RenderedText<D: TextDecorator>(SubRenderer<D>);

impl<D: TextDecorator> RenderedText<D> {
    /// Convert the rendered HTML document to a string.
    fn into_string(self) -> Result<String> {
        self.0.into_string()
    }

    /// Convert the rendered HTML document to a vector of lines with the annotations created by the
    /// decorator.
    fn into_lines(self) -> Result<Vec<TaggedLine<Vec<D::Annotation>>>> {
        Ok(self
            .0
            .into_lines()?
            .into_iter()
            .map(RenderLine::into_tagged_line)
            .collect())
    }
}

fn parse_with_context(mut input: impl io::Read, context: &mut HtmlContext) -> Result<RenderTree> {
    let opts = ParseOpts {
        tree_builder: TreeBuilderOpts {
            drop_doctype: true,
            ..Default::default()
        },
        ..Default::default()
    };
    let dom = parse_document(RcDom::default(), opts)
        .from_utf8()
        .read_from(&mut input)?;
    let render_tree =
        dom_to_render_tree_with_context(dom.document.clone(), &mut Discard {}, context)?
            .ok_or(Error::Fail)?;
    Ok(RenderTree(render_tree))
}

/// Reads and parses HTML from `input` and prepares a render tree.
pub fn parse(input: impl io::Read) -> Result<RenderTree> {
    parse_with_context(input, &mut Default::default())
}

/// Reads HTML from `input`, decorates it using `decorator`, and
/// returns a `String` with text wrapped to `width` columns.
pub fn from_read_with_decorator<R, D>(input: R, width: usize, decorator: D) -> Result<String>
where
    R: io::Read,
    D: TextDecorator,
{
    config::with_decorator(decorator).string_from_read(input, width)
}

/// Reads HTML from `input`, and returns a `String` with text wrapped to
/// `width` columns.
pub fn from_read<R>(input: R, width: usize) -> Result<String>
where
    R: io::Read,
{
    config::plain().string_from_read(input, width)
}

/// Reads HTML from `input`, and returns text wrapped to `width` columns.
/// The text is returned as a `Vec<TaggedLine<_>>`; the annotations are vectors
/// of `RichAnnotation`.  The "outer" annotation comes first in the `Vec`.
pub fn from_read_rich<R>(input: R, width: usize) -> Result<Vec<TaggedLine<Vec<RichAnnotation>>>>
where
    R: io::Read,
{
    config::rich().lines_from_read(input, width)
}

mod ansi_colours;

pub use ansi_colours::from_read_coloured;

#[cfg(test)]
mod tests;

fn calc_ol_prefix_size<D: TextDecorator>(start: i64, num_items: usize, decorator: &D) -> usize {
    // The prefix width could be at either end if the start is negative.
    let min_number = start;
    // Assumption: num_items can't overflow isize.
    let max_number = start + (num_items as i64) - 1;

    // This assumes that the decorator gives the same width as default.
    let prefix_width_min = decorator.ordered_item_prefix(min_number).len();
    let prefix_width_max = decorator.ordered_item_prefix(max_number).len();
    max(prefix_width_min, prefix_width_max)
}
