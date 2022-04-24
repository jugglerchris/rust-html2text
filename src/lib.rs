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
//! assert_eq!(from_read(&html[..], 20),
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

#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]
#![deny(missing_docs)]

#[macro_use]
extern crate html5ever;
extern crate unicode_width;

#[macro_use]
mod macros;

pub mod render;

use render::text_renderer::{
    PlainDecorator, RenderLine, RichAnnotation, RichDecorator, TaggedLine, TextDecorator,
    TextRenderer,
};
use render::Renderer;

use html5ever::driver::ParseOpts;
use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use html5ever::tree_builder::TreeBuilderOpts;
mod markup5ever_rcdom;
use markup5ever_rcdom::{
    Handle,
    NodeData::{Comment, Document, Element},
    RcDom,
};
use std::cell::Cell;
use std::cmp::{min,max};
use std::io;
use std::io::Write;
use std::iter::{once, repeat};
use std::ops::{Deref, DerefMut};

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
#[derive(Debug, Copy, Clone)]
pub struct SizeEstimate {
    size: usize,      // Rough overall size
    min_width: usize, // The narrowest possible
}

impl Default for SizeEstimate {
    fn default() -> SizeEstimate {
        SizeEstimate {
            size: 0,
            min_width: 0,
        }
    }
}

impl SizeEstimate {
    /// Combine two estimates into one (add size and widest required)
    pub fn add(self, other: SizeEstimate) -> SizeEstimate {
        SizeEstimate {
            size: self.size + other.size,
            min_width: max(self.min_width, other.min_width),
        }
    }

    /// Combine two estimates into one (take max of each)
    pub fn max(self, other: SizeEstimate) -> SizeEstimate {
        SizeEstimate {
            size: max(self.size, other.size),
            min_width: max(self.min_width, other.min_width),
        }
    }
}

#[derive(Clone, Debug)]
/// Render tree table cell
pub struct RenderTableCell {
    colspan: usize,
    content: Vec<RenderNode>,
    size_estimate: Cell<Option<SizeEstimate>>,
    col_width: Option<usize>, // Actual width to use
}

impl RenderTableCell {
    /// Render this cell to a builder.
    pub fn render<T: Write, R: Renderer>(&mut self, _builder: &mut R, _err_out: &mut T) {
        unimplemented!()
        //render_tree_children_to_string(builder, &mut self.content, err_out)
    }

    /// Calculate or return the estimate size of the cell
    pub fn get_size_estimate(&self) -> SizeEstimate {
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
pub struct RenderTableRow {
    cells: Vec<RenderTableCell>,
    col_sizes: Option<Vec<usize>>,
}

impl RenderTableRow {
    /// Return a mutable iterator over the cells.
    pub fn cells(&self) -> std::slice::Iter<RenderTableCell> {
        self.cells.iter()
    }
    /// Return a mutable iterator over the cells.
    pub fn cells_mut(&mut self) -> std::slice::IterMut<RenderTableCell> {
        self.cells.iter_mut()
    }
    /// Count the number of cells in the row.
    /// Takes into account colspan.
    pub fn num_cells(&self) -> usize {
        self.cells.iter().map(|cell| cell.colspan).sum()
    }
    /// Return an iterator over (column, &cell)s, which
    /// takes into account colspan.
    pub fn cell_columns(&mut self) -> Vec<(usize, &mut RenderTableCell)> {
        let mut result = Vec::new();
        let mut colno = 0;
        for cell in &mut self.cells {
            let colspan = cell.colspan;
            result.push((colno, cell));
            colno += colspan;
        }
        result
    }

    /// Return the contained cells as RenderNodes, annotated with their
    /// widths if available.  Skips cells with no width allocated.
    pub fn into_cells(self, vertical: bool) -> Vec<RenderNode> {
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
pub struct RenderTable {
    rows: Vec<RenderTableRow>,
    num_columns: usize,
    size_estimate: Cell<Option<SizeEstimate>>,
}

impl RenderTable {
    /// Create a new RenderTable with the given rows
    pub fn new(rows: Vec<RenderTableRow>) -> RenderTable {
        let num_columns = rows.iter().map(|r| r.num_cells()).max().unwrap_or(0);
        RenderTable {
            rows,
            num_columns,
            size_estimate: Cell::new(None),
        }
    }

    /// Return an iterator over the rows.
    pub fn rows(&self) -> std::slice::Iter<RenderTableRow> {
        self.rows.iter()
    }

    /// Return an iterator over the rows.
    pub fn rows_mut(&mut self) -> std::slice::IterMut<RenderTableRow> {
        self.rows.iter_mut()
    }
    /// Consume this and return a Vec<RenderNode> containing the children;
    /// the children know the column sizes required.
    pub fn into_rows(self, col_sizes: Vec<usize>, vert: bool) -> Vec<RenderNode> {
        self.rows
            .into_iter()
            .map(|mut tr| {
                tr.col_sizes = Some(col_sizes.clone());
                RenderNode::new(RenderNodeInfo::TableRow(tr, vert))
            })
            .collect()
    }

    fn calc_size_estimate(&self) {
        if self.num_columns == 0 {
            self.size_estimate.set(Some(SizeEstimate {
                size: 0,
                min_width: 0,
            }));
            return;
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
                        sizes[colno + colnum].min_width / cell.colspan,
                        cellsize.min_width,
                    );
                }
                colno += cell.colspan;
            }
        }
        let size = sizes.iter().map(|s| s.size).sum(); // Include borders?
        let min_width = sizes.iter().map(|s| s.min_width).sum::<usize>() + self.num_columns - 1;
        self.size_estimate.set(Some(SizeEstimate {
            size, min_width,
        }));
    }

    /// Calculate and store (or return stored value) of estimated size
    pub fn get_size_estimate(&self) -> SizeEstimate {
        if self.size_estimate.get().is_none() {
            self.calc_size_estimate();
        }
        self.size_estimate.get().unwrap()
    }
}

/// The node-specific information distilled from the DOM.
#[derive(Clone, Debug)]
pub enum RenderNodeInfo {
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
    /// An image (title)
    Img(String),
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
    /// A term (from a <dl>)
    Dt(Vec<RenderNode>),
    /// A definition (from a <dl>)
    Dd(Vec<RenderNode>),
    /// A line break
    Break,
    /// A table
    Table(RenderTable),
    /// A set of table rows (from either <thead> or <tbody>
    TableBody(Vec<RenderTableRow>),
    /// Table row (must only appear within a table body)
    /// If the boolean is true, then the cells are drawn vertically
    /// instead of horizontally (because of space).
    TableRow(RenderTableRow, bool),
    /// Table cell (must only appear within a table row)
    TableCell(RenderTableCell),
    /// Start of a named HTML fragment
    FragStart(String),
}

/// Common fields from a node.
#[derive(Clone, Debug)]
pub struct RenderNode {
    size_estimate: Cell<Option<SizeEstimate>>,
    info: RenderNodeInfo,
}

impl RenderNode {
    /// Create a node from the RenderNodeInfo.
    pub fn new(info: RenderNodeInfo) -> RenderNode {
        RenderNode {
            size_estimate: Cell::new(None),
            info,
        }
    }

    /// Get a size estimate (~characters)
    pub fn get_size_estimate(&self) -> SizeEstimate {
        // If it's already calculated, then just return the answer.
        if let Some(s) = self.size_estimate.get() {
            return s;
        };

        use RenderNodeInfo::*;

        // Otherwise, make an estimate.
        let estimate = match self.info {
            Text(ref t) | Img(ref t) => {
                let mut len = t.trim().len();
                // Add one for preceding whitespace.
                if let Some(true) = t.chars().next().map(|c| c.is_whitespace()) {
                    len += 1;
                }
                SizeEstimate {
                    size: len,
                    min_width: len.min(MIN_WIDTH),
                }
            }

            Container(ref v)
            | Link(_, ref v)
            | Em(ref v)
            | Strong(ref v)
            | Strikeout(ref v)
            | Code(ref v)
            | Block(ref v)
            | Div(ref v)
            | Pre(ref v)
            | BlockQuote(ref v)
            | Dl(ref v)
            | Dt(ref v)
            | Dd(ref v)
            | Ul(ref v)
            | Ol(_, ref v) => v
                .iter()
                .map(RenderNode::get_size_estimate)
                .fold(Default::default(), SizeEstimate::add),
            Header(level, ref v) => v
                .iter()
                .map(RenderNode::get_size_estimate)
                .fold(Default::default(), SizeEstimate::add)
                .add(SizeEstimate {
                    size: 0,
                    min_width: MIN_WIDTH + level + 2,
                }),
            Break => SizeEstimate {
                size: 1,
                min_width: 1,
            },
            Table(ref t) => t.get_size_estimate(),
            TableRow(..) | TableBody(_) | TableCell(_) => unimplemented!(),
            FragStart(_) => Default::default(),
        };
        self.size_estimate.set(Some(estimate));
        estimate
    }

    /// Return true if this node is definitely empty.  This is used to quickly
    /// remove e.g. links with no anchor text in most cases, but can't recurse
    /// and look more deeply.
    pub fn is_shallow_empty(&self) -> bool {
        use RenderNodeInfo::*;

        // Otherwise, make an estimate.
        match self.info {
            Text(ref t) | Img(ref t) => {
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
            | Div(ref v)
            | Pre(ref v)
            | BlockQuote(ref v)
            | Dl(ref v)
            | Dt(ref v)
            | Dd(ref v)
            | Ul(ref v)
            | Ol(_, ref v) => v.is_empty(),
            Header(_level, ref v) => v.is_empty(),
            Break => true,
            Table(ref _t) => false,
            TableRow(..) | TableBody(_) | TableCell(_) => false,
            FragStart(_) => true,
        }
    }
}

fn precalc_size_estimate<'a>(node: &'a RenderNode) -> TreeMapResult<(), &'a RenderNode, ()> {
    use RenderNodeInfo::*;
    if node.size_estimate.get().is_some() {
        return TreeMapResult::Nothing;
    }
    match node.info {
        Text(_) | Img(_) | Break | FragStart(_) => {
            let _ = node.get_size_estimate();
            TreeMapResult::Nothing
        }

        Container(ref v)
        | Link(_, ref v)
        | Em(ref v)
        | Strong(ref v)
        | Strikeout(ref v)
        | Code(ref v)
        | Block(ref v)
        | Div(ref v)
        | Pre(ref v)
        | BlockQuote(ref v)
        | Ul(ref v)
        | Ol(_, ref v)
        | Dl(ref v)
        | Dt(ref v)
        | Dd(ref v)
        | Header(_, ref v) => TreeMapResult::PendingChildren {
            children: v.iter().collect(),
            cons: Box::new(move |_, _cs| {
                node.get_size_estimate();
                None
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
                cons: Box::new(move |_, _cs| {
                    node.get_size_estimate();
                    None
                }),
                prefn: None,
                postfn: None,
            }
        }
        TableRow(..) | TableBody(_) | TableCell(_) => unimplemented!(),
    }
}

/// Make a Vec of RenderNodes from the children of a node.
fn children_to_render_nodes<T: Write>(handle: Handle, err_out: &mut T) -> Vec<RenderNode> {
    /* process children, but don't add anything */
    let children = handle
        .children
        .borrow()
        .iter()
        .flat_map(|ch| dom_to_render_tree(ch.clone(), err_out))
        .collect();
    children
}

/// Make a Vec of RenderNodes from the <li> children of a node.
fn list_children_to_render_nodes<T: Write>(handle: Handle, err_out: &mut T) -> Vec<RenderNode> {
    let mut children = Vec::new();

    for child in handle.children.borrow().iter() {
        match child.data {
            Element { ref name, .. } => match name.expanded() {
                expanded_name!(html "li") => {
                    let li_children = children_to_render_nodes(child.clone(), err_out);
                    children.push(RenderNode::new(RenderNodeInfo::Block(li_children)));
                }
                _ => {}
            },
            Comment { .. } => {}
            _ => {
                html_trace!("Unhandled in list: {:?}\n", child);
            }
        }
    }
    children
}

/// Make a Vec of DtElements from the <dt> and <dd> children of a node.
fn desc_list_children_to_render_nodes<T: Write>(
    handle: Handle,
    err_out: &mut T,
) -> Vec<RenderNode> {
    let mut children = Vec::new();

    for child in handle.children.borrow().iter() {
        match child.data {
            Element { ref name, .. } => match name.expanded() {
                expanded_name!(html "dt") => {
                    let dt_children = children_to_render_nodes(child.clone(), err_out);
                    children.push(RenderNode::new(RenderNodeInfo::Dt(dt_children)));
                }
                expanded_name!(html "dd") => {
                    let dd_children = children_to_render_nodes(child.clone(), err_out);
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
    children
}

/// Convert a table into a RenderNode
fn table_to_render_tree<'a, 'b, T: Write>(
    handle: Handle,
    _err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, RenderNode> {
    pending(handle, |_, rowset| {
        let mut rows = vec![];
        for bodynode in rowset {
            if let RenderNodeInfo::TableBody(body) = bodynode.info {
                rows.extend(body);
            } else {
                html_trace!("Found in table: {:?}", bodynode.info);
            }
        }
        Some(RenderNode::new(RenderNodeInfo::Table(RenderTable::new(
            rows,
        ))))
    })
}

/// Add rows from a thead or tbody.
fn tbody_to_render_tree<'a, 'b, T: Write>(
    handle: Handle,
    _err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, RenderNode> {
    pending(handle, |_, rowchildren| {
        let rows = rowchildren
            .into_iter()
            .flat_map(|rownode| {
                if let RenderNodeInfo::TableRow(row, _) = rownode.info {
                    Some(row)
                } else {
                    html_trace!("  [[tbody child: {:?}]]", rownode);
                    None
                }
            })
            .collect();
        Some(RenderNode::new(RenderNodeInfo::TableBody(rows)))
    })
}

/// Convert a table row to a RenderTableRow
fn tr_to_render_tree<'a, 'b, T: Write>(
    handle: Handle,
    _err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, RenderNode> {
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
        Some(RenderNode::new(RenderNodeInfo::TableRow(RenderTableRow {
            cells,
            col_sizes: None,
        }, false)))
    })
}

/// Convert a single table cell to a render node.
fn td_to_render_tree<'a, 'b, T: Write>(
    handle: Handle,
    _err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, RenderNode> {
    let mut colspan = 1;
    if let Element { ref attrs, .. } = handle.data {
        for attr in attrs.borrow().iter() {
            if &attr.name.local == "colspan" {
                let v: &str = &*attr.value;
                colspan = v.parse().unwrap_or(1);
            }
        }
    }
    pending(handle, move |_, children| {
        Some(RenderNode::new(RenderNodeInfo::TableCell(
            RenderTableCell {
                colspan,
                content: children,
                size_estimate: Cell::new(None),
                col_width: None,
            },
        )))
    })
}

/// A reducer which combines results from mapping children into
/// the result for the current node.  Takes a context and a
/// vector of results and returns a new result (or nothing).
type ResultReducer<'a, C, R> = dyn Fn(&mut C, Vec<R>) -> Option<R> + 'a;

/// A closure to call before processing a child node.
type ChildPreFn<C, N> = dyn Fn(&mut C, &N);

/// A closure to call after processing a child node,
/// before adding the result to the processed results
/// vector.
type ChildPostFn<C, R> = dyn Fn(&mut C, &R);

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

fn tree_map_reduce<'a, C, N, R, M>(context: &mut C, top: N, mut process_node: M) -> Option<R>
where
    M: for<'c> FnMut(&'c mut C, N) -> TreeMapResult<'a, C, N, R>,
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

    let mut pending_stack = vec![PendingNode {
        // We only expect one child, which we'll just return.
        construct: Box::new(|_, mut cs| cs.pop()),
        prefn: None,
        postfn: None,
        children: Vec::new(),
        to_process: vec![top].into_iter(),
    }];
    loop {
        // Get the next child node to process
        let next_node = pending_stack.last_mut().unwrap().to_process.next();
        if let Some(h) = next_node {
            pending_stack
                .last_mut()
                .unwrap()
                .prefn
                .as_ref()
                .map(|ref f| f(context, &h));
            match process_node(context, h) {
                TreeMapResult::Finished(result) => {
                    pending_stack
                        .last_mut()
                        .unwrap()
                        .postfn
                        .as_ref()
                        .map(|ref f| f(context, &result));
                    pending_stack.last_mut().unwrap().children.push(result);
                }
                TreeMapResult::PendingChildren {
                    children,
                    cons,
                    prefn,
                    postfn,
                } => {
                    pending_stack.push(PendingNode {
                        construct: cons,
                        prefn,
                        postfn,
                        children: Vec::new(),
                        to_process: children.into_iter(),
                    });
                }
                TreeMapResult::Nothing => {}
            };
        } else {
            // No more children, so finally construct the parent.
            let completed = pending_stack.pop().unwrap();
            let reduced = (completed.construct)(context, completed.children);
            if let Some(node) = reduced {
                if let Some(parent) = pending_stack.last_mut() {
                    parent.postfn.as_ref().map(|ref f| f(context, &node));
                    parent.children.push(node);
                } else {
                    // Finished the whole stack!
                    break Some(node);
                }
            } else {
                /* Finished the stack, and have nothing */
                if pending_stack.is_empty() {
                    break None;
                }
            }
        }
    }
}

/// Convert a DOM tree or subtree into a render tree.
pub fn dom_to_render_tree<T: Write>(handle: Handle, err_out: &mut T) -> Option<RenderNode> {
    html_trace!("### dom_to_render_tree: HTML: {:?}", handle);
    let result = tree_map_reduce(&mut (), handle, |_, handle| {
        process_dom_node(handle, err_out)
    });

    html_trace!("### dom_to_render_tree: out= {:#?}", result);
    result
}

fn pending<'a, F>(handle: Handle, f: F) -> TreeMapResult<'a, (), Handle, RenderNode>
where
    //for<'a> F: Fn(&'a mut C, Vec<RenderNode>) -> Option<RenderNode>+'static
    for<'r> F: Fn(&'r mut (), std::vec::Vec<RenderNode>) -> Option<RenderNode> + 'static,
{
    TreeMapResult::PendingChildren {
        children: handle.children.borrow().clone(),
        cons: Box::new(f),
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
            if rrow.cells.len() > 0 {
                rrow.cells[0].content.insert(0, prefix);
            }
        }

        TableBody(ref mut rows) | Table(RenderTable { ref mut rows, .. }) => {
            // If the row is empty, then there isn't really anything
            // to attach the fragment start to.
            if rows.len() > 0 {
                let rrow = &mut rows[0];
                if rrow.cells.len() > 0 {
                    rrow.cells[0].content.insert(0, prefix);
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

fn process_dom_node<'a, 'b, T: Write>(
    handle: Handle,
    err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, RenderNode> {
    use RenderNodeInfo::*;
    use TreeMapResult::*;

    match handle.clone().data {
        Document => pending(handle, |&mut (), cs| Some(RenderNode::new(Container(cs)))),
        Comment { .. } => Nothing,
        Element {
            ref name,
            ref attrs,
            ..
        } => {
            let mut frag_from_name_attr = false;
            let result = match name.expanded() {
                expanded_name!(html "html")
                | expanded_name!(html "span")
                | expanded_name!(html "body") => {
                    /* process children, but don't add anything */
                    pending(handle, |_, cs| Some(RenderNode::new(Container(cs))))
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
                            // We need the closure to own the string it's going to use.
                            // Unfortunately that means we ideally want FnOnce; but
                            // that doesn't yet work in a Box.  Box<FnBox()> does, but
                            // is unstable.  So we'll just move a string in and clone
                            // it on use.
                            let href: String = href.into();
                            Box::new(move |_, cs: Vec<RenderNode>| {
                                if cs
                                    .iter()
                                    .any(|c| !c.is_shallow_empty()) {
                                    Some(RenderNode::new(Link(href.clone(), cs)))
                                } else {
                                    None
                                }
                            })
                        } else {
                            Box::new(|_, cs| Some(RenderNode::new(Container(cs))))
                        },
                        prefn: None,
                        postfn: None,
                    }
                }
                expanded_name!(html "em") => pending(handle, |_, cs| Some(RenderNode::new(Em(cs)))),
                expanded_name!(html "strong") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Strong(cs))))
                }
                expanded_name!(html "s") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Strikeout(cs))))
                }
                expanded_name!(html "code") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Code(cs))))
                }
                expanded_name!(html "img") => {
                    let borrowed = attrs.borrow();
                    let mut title = None;
                    for attr in borrowed.iter() {
                        if &attr.name.local == "alt" && !attr.value.is_empty() {
                            title = Some(&*attr.value);
                            break;
                        }
                    }
                    if let Some(title) = title {
                        Finished(RenderNode::new(Img(title.into())))
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
                        Some(RenderNode::new(Header(level, cs)))
                    })
                }
                expanded_name!(html "p") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Block(cs))))
                }
                expanded_name!(html "div") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Div(cs))))
                }
                expanded_name!(html "pre") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Pre(cs))))
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
                    pending(handle, |_, cs| Some(RenderNode::new(BlockQuote(cs))))
                }
                expanded_name!(html "ul") => Finished(RenderNode::new(Ul(
                    list_children_to_render_nodes(handle.clone(), err_out),
                ))),
                expanded_name!(html "ol") => {
                    let borrowed = attrs.borrow();
                    let mut start = 1;
                    for attr in borrowed.iter() {
                        if &attr.name.local == "start" {
                            start = attr.value.parse().ok().unwrap_or(1);
                            break;
                        }
                    }

                    Finished(RenderNode::new(Ol(
                        start,
                        list_children_to_render_nodes(handle.clone(), err_out),
                    )))
                }
                expanded_name!(html "dl") => Finished(RenderNode::new(Dl(
                    desc_list_children_to_render_nodes(handle.clone(), err_out),
                ))),
                _ => {
                    html_trace!("Unhandled element: {:?}\n", name.local);
                    pending(handle, |_, cs| Some(RenderNode::new(Container(cs))))
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

            if let Some(fragname) = fragment {
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
                    } => {
                        let fragname: String = fragname.into();
                        PendingChildren {
                            children,
                            prefn,
                            postfn,
                            cons: Box::new(move |ctx, ch| {
                                let fragnode = RenderNode::new(FragStart(fragname.clone()));
                                match cons(ctx, ch) {
                                    None => Some(fragnode),
                                    Some(node) => Some(prepend_marker(fragnode, node)),
                                }
                            }),
                        }
                    }
                }
            } else {
                result
            }
        }
        markup5ever_rcdom::NodeData::Text { contents: ref tstr } => {
            Finished(RenderNode::new(Text((&*tstr.borrow()).into())))
        }
        _ => {
            // NodeData doesn't have a Debug impl.
            write!(err_out, "Unhandled node type.\n").unwrap();
            Nothing
        }
    }
}

/// Context to use during tree parsing.
/// This mainly gives access to a Renderer, but needs to be able to push
/// new ones on for nested structures.
struct BuilderStack<R: Renderer> {
    builders: Vec<R>,
}

impl<R: Renderer> BuilderStack<R> {
    pub fn new(builder: R) -> BuilderStack<R> {
        BuilderStack {
            builders: vec![builder],
        }
    }

    /// Push a new builder onto the stack
    pub fn push(&mut self, builder: R) {
        self.builders.push(builder);
    }

    /// Pop off the top builder and return it.
    /// Panics if empty
    pub fn pop(&mut self) -> R {
        self.builders.pop().unwrap()
    }

    /// Pop off the only builder and return it.
    /// panics if there aren't exactly 1 available.
    pub fn into_inner(mut self) -> R {
        assert_eq!(self.builders.len(), 1);
        self.builders.pop().unwrap()
    }
}

impl<R: Renderer> Deref for BuilderStack<R> {
    type Target = R;
    fn deref(&self) -> &R {
        self.builders.last().expect("Underflow in BuilderStack")
    }
}

impl<R: Renderer> DerefMut for BuilderStack<R> {
    fn deref_mut(&mut self) -> &mut R {
        self.builders.last_mut().expect("Underflow in BuilderStack")
    }
}

fn render_tree_to_string<T: Write, R: Renderer>(
    builder: R,
    tree: RenderNode,
    err_out: &mut T,
) -> R {
    /* Phase 1: get size estimates. */
    tree_map_reduce(&mut (), &tree, |_, node| precalc_size_estimate(&node));

    /* Phase 2: actually render. */
    let mut bs = BuilderStack::new(builder);
    tree_map_reduce(&mut bs, tree, |builders, node| {
        do_render_node(builders, node, err_out)
    });
    bs.into_inner()
}

fn pending2<
    'a,
    R: Renderer,
    F: Fn(&mut BuilderStack<R>, Vec<Option<R>>) -> Option<Option<R>> + 'static,
>(
    children: Vec<RenderNode>,
    f: F,
) -> TreeMapResult<'a, BuilderStack<R>, RenderNode, Option<R>> {
    TreeMapResult::PendingChildren {
        children,
        cons: Box::new(f),
        prefn: None,
        postfn: None,
    }
}

fn do_render_node<'a, 'b, T: Write, R: Renderer>(
    builder: &mut BuilderStack<R>,
    tree: RenderNode,
    err_out: &'b mut T,
) -> TreeMapResult<'static, BuilderStack<R>, RenderNode, Option<R>> {
    html_trace!("do_render_node({:?}", tree);
    use RenderNodeInfo::*;
    use TreeMapResult::*;
    match tree.info {
        Text(ref tstr) => {
            builder.add_inline_text(tstr);
            Finished(None)
        }
        Container(children) => pending2(children, |_, _| Some(None)),
        Link(href, children) => {
            builder.start_link(&href);
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.end_link();
                Some(None)
            })
        }
        Em(children) => {
            builder.start_emphasis();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.end_emphasis();
                Some(None)
            })
        }
        Strong(children) => {
            builder.start_strong();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.end_strong();
                Some(None)
            })
        }
        Strikeout(children) => {
            builder.start_strikeout();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.end_strikeout();
                Some(None)
            })
        }
        Code(children) => {
            builder.start_code();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.end_code();
                Some(None)
            })
        }
        Img(title) => {
            builder.add_image(&title);
            Finished(None)
        }
        Block(children) => {
            builder.start_block();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.end_block();
                Some(None)
            })
        }
        Header(level, children) => {
            let prefix = builder.header_prefix(level);
            let min_width = max(builder.width(), 1 + prefix.len());
            let sub_builder = builder.new_sub_renderer(min_width - prefix.len());
            builder.push(sub_builder);
            pending2(children, move |builder: &mut BuilderStack<R>, _| {
                let sub_builder = builder.pop();

                builder.start_block();
                builder.append_subrender(sub_builder, repeat(&prefix[..]));
                builder.end_block();
                Some(None)
            })
        }
        Div(children) => {
            builder.new_line();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.new_line();
                Some(None)
            })
        }
        Pre(children) => {
            builder.new_line();
            builder.start_pre();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.new_line();
                builder.end_pre();
                Some(None)
            })
        }
        BlockQuote(children) => {
            let prefix = builder.quote_prefix();
            let sub_builder = builder.new_sub_renderer(builder.width() - prefix.len());
            builder.push(sub_builder);
            pending2(children, move |builder: &mut BuilderStack<R>, _| {
                let sub_builder = builder.pop();

                builder.start_block();
                builder.append_subrender(sub_builder, repeat(&prefix[..]));
                builder.end_block();
                Some(None)
            })
        }
        Ul(items) => {
            builder.start_block();

            let prefix = builder.unordered_item_prefix();
            let prefix_len = prefix.len();

            TreeMapResult::PendingChildren {
                children: items,
                cons: Box::new(|_, _| Some(None)),
                prefn: Some(Box::new(move |builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.new_sub_renderer(builder.width() - prefix_len);
                    builder.push(sub_builder);
                })),
                postfn: Some(Box::new(move |builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.pop();

                    let indent = " ".repeat(prefix.len());

                    builder.append_subrender(
                        sub_builder,
                        once(&prefix[..]).chain(repeat(&indent[..])),
                    );
                })),
            }
        }
        Ol(start, items) => {
            builder.start_block();

            let num_items = items.len();

            // The prefix width could be at either end if the start is negative.
            let min_number = start;
            // Assumption: num_items can't overflow isize.
            let max_number = start + (num_items as i64) - 1;
            let prefix_width_min = builder.ordered_item_prefix(min_number).len();
            let prefix_width_max = builder.ordered_item_prefix(max_number).len();
            let prefix_width = max(prefix_width_min, prefix_width_max);
            let prefixn = format!("{: <width$}", "", width = prefix_width);
            let i: Cell<_> = Cell::new(start);

            TreeMapResult::PendingChildren {
                children: items,
                cons: Box::new(|_, _| Some(None)),
                prefn: Some(Box::new(move |builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.new_sub_renderer(builder.width() - prefix_width);
                    builder.push(sub_builder);
                })),
                postfn: Some(Box::new(move |builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.pop();
                    let prefix1 = builder.ordered_item_prefix(i.get());
                    let prefix1 = format!("{: <width$}", prefix1, width = prefix_width);

                    builder.append_subrender(
                        sub_builder,
                        once(prefix1.as_str()).chain(repeat(prefixn.as_str())),
                    );
                    i.set(i.get() + 1);
                })),
            }
        }
        Dl(items) => {
            builder.start_block();

            TreeMapResult::PendingChildren {
                children: items,
                cons: Box::new(|_, _| Some(None)),
                prefn: None,
                postfn: None,
            }
        }
        Dt(children) => {
            builder.new_line();
            builder.start_emphasis();
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                builder.end_emphasis();
                Some(None)
            })
        }
        Dd(children) => {
            let sub_builder = builder.new_sub_renderer(builder.width() - 2);
            builder.push(sub_builder);
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                let sub_builder = builder.pop();
                builder.append_subrender(sub_builder, repeat("  "));
                Some(None)
            })
        }
        Break => {
            builder.new_line_hard();
            Finished(None)
        }
        Table(tab) => render_table_tree(builder.deref_mut(), tab, err_out),
        TableRow(row, false) => render_table_row(builder.deref_mut(), row, err_out),
        TableRow(row, true) => render_table_row_vert(builder.deref_mut(), row, err_out),
        TableBody(_) => unimplemented!("Unexpected TableBody while rendering"),
        TableCell(cell) => render_table_cell(builder.deref_mut(), cell, err_out),
        FragStart(fragname) => {
            builder.record_frag_start(&fragname);
            Finished(None)
        }
    }
}

fn render_table_tree<T: Write, R: Renderer>(
    builder: &mut R,
    table: RenderTable,
    _err_out: &mut T,
) -> TreeMapResult<'static, BuilderStack<R>, RenderNode, Option<R>> {
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
    let min_size: usize = col_sizes.iter().map(|est| est.min_width).sum::<usize>() +
                          col_sizes.len().saturating_sub(1);
    let width = builder.width();

    let vert_row = min_size > width;

    let mut col_widths: Vec<usize> = if !vert_row {
        col_sizes
            .iter()
            .map(|sz| {
                if sz.size == 0 {
                    0
                } else {
                    min(sz.size,
                        max(sz.size * width / tot_size, sz.min_width))
                }
            })
            .collect()
    } else {
        col_sizes
            .iter()
            .map(|_| width)
            .collect()
    };

    if !vert_row {
        loop {
            let cur_width = col_widths.iter().cloned().sum::<usize>();
            if cur_width <= width {
                break;
            }
            let (i, _) = col_widths
                .iter()
                .cloned()
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

    builder.start_block();

    let table_width = if vert_row {
        width
    } else {
        col_widths.iter().cloned().sum::<usize>() +
        col_widths.iter().filter(|&w| w > &0).count().saturating_sub(1)
    };

    builder.add_horizontal_border_width(table_width);

    TreeMapResult::PendingChildren {
        children: table.into_rows(col_widths, vert_row),
        cons: Box::new(|_, _| Some(None)),
        prefn: Some(Box::new(|_, _| {})),
        postfn: Some(Box::new(|_, _| {})),
    }
}

fn render_table_row<T: Write, R: Renderer>(
    _builder: &mut R,
    row: RenderTableRow,
    _err_out: &mut T,
) -> TreeMapResult<'static, BuilderStack<R>, RenderNode, Option<R>> {
    TreeMapResult::PendingChildren {
        children: row.into_cells(false),
        cons: Box::new(|builders, children| {
            let children: Vec<_> = children.into_iter().map(Option::unwrap).collect();
            if children.iter().any(|c| !c.empty()) {
                builders.append_columns_with_borders(children, true);
            }
            Some(None)
        }),
        prefn: Some(Box::new(|builder: &mut BuilderStack<R>, node| {
            if let RenderNodeInfo::TableCell(ref cell) = node.info {
                let sub_builder = builder.new_sub_renderer(cell.col_width.unwrap());
                builder.push(sub_builder);
            } else {
                panic!()
            }
        })),
        postfn: Some(Box::new(|_builder: &mut BuilderStack<R>, _| {})),
    }
}

fn render_table_row_vert<T: Write, R: Renderer>(
    _builder: &mut R,
    row: RenderTableRow,
    _err_out: &mut T,
) -> TreeMapResult<'static, BuilderStack<R>, RenderNode, Option<R>> {
    TreeMapResult::PendingChildren {
        children: row.into_cells(true),
        cons: Box::new(|builders, children| {
            let children: Vec<_> = children.into_iter().map(Option::unwrap).collect();
            builders.append_vert_row(children);
            Some(None)
        }),
        prefn: Some(Box::new(|builder: &mut BuilderStack<R>, node| {
            if let RenderNodeInfo::TableCell(ref cell) = node.info {
                let sub_builder = builder.new_sub_renderer(cell.col_width.unwrap());
                builder.push(sub_builder);
            } else {
                panic!()
            }
        })),
        postfn: Some(Box::new(|_builder: &mut BuilderStack<R>, _| {})),
    }
}

fn render_table_cell<T: Write, R: Renderer>(
    _builder: &mut R,
    cell: RenderTableCell,
    _err_out: &mut T,
) -> TreeMapResult<'static, BuilderStack<R>, RenderNode, Option<R>> {
    pending2(cell.content, |builder: &mut BuilderStack<R>, _| {
        let sub_builder = builder.pop();
        Some(Some(sub_builder))
    })
}

/// The structure of an HTML document that can be rendered using a [`TextDecorator`][].
///
/// [`TextDecorator`]: render/text_renderer/trait.TextDecorator.html

#[derive(Clone, Debug)]
pub struct RenderTree(RenderNode);

impl RenderTree {
    /// Render this document using the given `decorator` and wrap it to `width` columns.
    pub fn render<D: TextDecorator>(self, width: usize, decorator: D) -> RenderedText<D> {
        let builder = TextRenderer::new(width, decorator);
        let builder = render_tree_to_string(builder, self.0, &mut Discard {});
        RenderedText(builder)
    }

    /// Render this document as plain text using the [`PlainDecorator`][] and wrap it to `width`
    /// columns.
    ///
    /// [`PlainDecorator`]: render/text_renderer/struct.PlainDecorator.html
    pub fn render_plain(self, width: usize) -> RenderedText<PlainDecorator> {
        self.render(width, PlainDecorator::new())
    }

    /// Render this document as rich text using the [`RichDecorator`][] and wrap it to `width`
    /// columns.
    ///
    /// [`RichDecorator`]: render/text_renderer/struct.RichDecorator.html
    pub fn render_rich(self, width: usize) -> RenderedText<RichDecorator> {
        self.render(width, RichDecorator::new())
    }
}

/// A rendered HTML document.
pub struct RenderedText<D: TextDecorator>(TextRenderer<D>);

impl<D: TextDecorator> RenderedText<D> {
    /// Convert the rendered HTML document to a string.
    pub fn into_string(self) -> String {
        self.0.into_string()
    }

    /// Convert the rendered HTML document to a vector of lines with the annotations created by the
    /// decorator.
    pub fn into_lines(self) -> Vec<TaggedLine<Vec<D::Annotation>>> {
        self.0
            .into_lines()
            .into_iter()
            .map(RenderLine::into_tagged_line)
            .collect()
    }
}

/// Reads and parses HTML from `input` and prepares a render tree.
pub fn parse(mut input: impl io::Read) -> RenderTree {
    let opts = ParseOpts {
        tree_builder: TreeBuilderOpts {
            drop_doctype: true,
            ..Default::default()
        },
        ..Default::default()
    };
    let dom = parse_document(RcDom::default(), opts)
        .from_utf8()
        .read_from(&mut input)
        .unwrap();
    let render_tree = dom_to_render_tree(dom.document.clone(), &mut Discard {}).unwrap();
    RenderTree(render_tree)
}

/// Reads HTML from `input`, decorates it using `decorator`, and
/// returns a `String` with text wrapped to `width` columns.
pub fn from_read_with_decorator<R, D>(input: R, width: usize, decorator: D) -> String
where
    R: io::Read,
    D: TextDecorator,
{
    parse(input).render(width, decorator).into_string()
}

/// Reads HTML from `input`, and returns a `String` with text wrapped to
/// `width` columns.
pub fn from_read<R>(input: R, width: usize) -> String
where
    R: io::Read,
{
    let decorator = PlainDecorator::new();
    from_read_with_decorator(input, width, decorator)
}

/// Reads HTML from `input`, and returns text wrapped to `width` columns.
/// The text is returned as a `Vec<TaggedLine<_>>`; the annotations are vectors
/// of `RichAnnotation`.  The "outer" annotation comes first in the `Vec`.
pub fn from_read_rich<R>(input: R, width: usize) -> Vec<TaggedLine<Vec<RichAnnotation>>>
where
    R: io::Read,
{
    parse(input)
        .render(width, RichDecorator::new())
        .into_lines()
}

#[cfg(feature = "ansi_colours")]
mod ansi_colours;

#[cfg(feature = "ansi_colours")]
pub use ansi_colours::from_read_coloured;

#[cfg(test)]
mod tests;
