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

#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![deny(missing_docs)]

extern crate html5ever_atoms;
#[macro_use] extern crate html5ever;
extern crate unicode_width;

#[macro_use]
mod macros;

pub mod render;

use render::Renderer;
use render::text_renderer::{TextRenderer,PlainDecorator,RichDecorator,
                            RichAnnotation,TaggedLine,RenderLine};

use std::io;
use std::io::Write;
use std::cmp::max;
use std::iter::{once,repeat};
use std::ops::{Deref,DerefMut};
use html5ever::{parse_document};
use html5ever::driver::ParseOpts;
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::rcdom::{self,RcDom,Handle,NodeData::{Text,Element,Document,Comment}};
use html5ever::tendril::TendrilSink;

/// A dummy writer which does nothing
struct Discard {}
impl Write for Discard {
    fn write(&mut self, bytes: &[u8]) -> std::result::Result<usize, io::Error> { Ok(bytes.len()) }
    fn flush(&mut self) -> std::result::Result<(), io::Error> { Ok(()) }
}

fn get_text(handle: Handle) -> String {
    let node = &*handle;
    let mut result = String::new();
    if let Text { contents: ref tstr } = node.data {
        result.push_str(&tstr.borrow());
    } else {
        for child in &*node.children.borrow() {
            result.push_str(&get_text(child.clone()));
        }
    }
    result
}

const MIN_WIDTH: usize = 5;

/// Size information/estimate
#[derive(Debug,Copy,Clone)]
pub struct SizeEstimate {
    size: usize,       // Rough overall size
    min_width: usize,  // The narrowest possible
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
}

#[derive(Debug)]
/// Render tree table cell
pub struct RenderTableCell {
    colspan: usize,
    content: Vec<RenderNode>,
    size_estimate: Option<SizeEstimate>,
    col_width: Option<usize>,  // Actual width to use
}

impl RenderTableCell {
    /// Render this cell to a builder.
    pub fn render<T:Write, R:Renderer>(&mut self, _builder: &mut R, _err_out: &mut T)
    {
        unimplemented!()
        //render_tree_children_to_string(builder, &mut self.content, err_out)
    }

    /// Calculate or return the estimate size of the cell
    pub fn get_size_estimate(&mut self) -> SizeEstimate {
        if self.size_estimate.is_none() {
            let size = self.content
                           .iter_mut()
                           .map(|node| node.get_size_estimate())
                           .fold(Default::default(), SizeEstimate::add);
            self.size_estimate = Some(size);
        }
        self.size_estimate.unwrap()
    }
}

#[derive(Debug)]
/// Render tree table row
pub struct RenderTableRow {
    cells: Vec<RenderTableCell>,
    col_sizes: Option<Vec<usize>>,
}

impl RenderTableRow {
    /// Return a mutable iterator over the cells.
    pub fn cells(&mut self) -> std::slice::IterMut<RenderTableCell> {
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
    pub fn into_cells(self) -> Vec<RenderNode> {
        let mut result = Vec::new();
        let mut colno = 0;
        let col_sizes = self.col_sizes.unwrap();
        for mut cell in self.cells {
            let colspan = cell.colspan;
            let col_width: usize = col_sizes[colno..colno+cell.colspan].iter().sum();
            if col_width > 1 {
                cell.col_width = Some(col_width - 1);
                result.push(RenderNode::new(RenderNodeInfo::TableCell(cell)));
            }
            colno += colspan;
        }
        result
    }
}

#[derive(Debug)]
/// A representation of a table render tree with metadata.
pub struct RenderTable {
    rows: Vec<RenderTableRow>,
    num_columns: usize,
    size_estimate: Option<SizeEstimate>,
}

impl RenderTable {
    /// Create a new RenderTable with the given rows
    pub fn new(rows: Vec<RenderTableRow>) -> RenderTable {
        let num_columns = rows.iter()
                              .map(|r| r.num_cells()).max().unwrap_or(0);
        RenderTable {
            rows: rows,
            num_columns: num_columns,
            size_estimate: None,
        }
    }

    /// Return an iterator over the rows.
    pub fn rows(&mut self) -> std::slice::IterMut<RenderTableRow> {
        self.rows.iter_mut()
    }

    /// Consume this and return a Vec<RenderNode> containing the children;
    /// the children know the column sizes required.
    pub fn into_rows(self, col_sizes: Vec<usize>) -> Vec<RenderNode> {
        self.rows
            .into_iter()
            .map(|mut tr| {
                tr.col_sizes = Some(col_sizes.clone());
                RenderNode::new(RenderNodeInfo::TableRow(tr))
             })
            .collect()
    }

    fn calc_size_estimate(&mut self) {
        if self.num_columns == 0 {
            self.size_estimate = Some(SizeEstimate { size: 0, min_width: 0 });
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
                    sizes[colno + colnum].min_width = max(sizes[colno+colnum].min_width/cell.colspan, cellsize.min_width);
                }
                colno += cell.colspan;
            }
        }
        let size = sizes.iter().map(|s| s.size).sum();  // Include borders?
        let min_width = sizes.iter().map(|s| s.min_width).sum::<usize>() + self.num_columns-1;
        self.size_estimate = Some(SizeEstimate { size: size, min_width: min_width });
    }

    /// Calculate and store (or return stored value) of estimated size
    pub fn get_size_estimate(&mut self) -> SizeEstimate {
        if self.size_estimate.is_none() {
            self.calc_size_estimate();
        }
        self.size_estimate.unwrap()
    }
}

/// The node-specific information distilled from the DOM.
#[derive(Debug)]
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
    Pre(String),
    /// A blockquote
    BlockQuote(Vec<RenderNode>),
    /// An unordered list
    Ul(Vec<RenderNode>),
    /// An ordered list
    Ol(Vec<RenderNode>),
    /// A line break
    Break,
    /// A table
    Table(RenderTable),
    /// A set of table rows (from either <thead> or <tbody>
    TableBody(Vec<RenderTableRow>),
    /// Table row (must only appear within a table body)
    TableRow(RenderTableRow),
    /// Table cell (must only appear within a table row)
    TableCell(RenderTableCell),
}

/// Common fields from a node.
#[derive(Debug)]
pub struct RenderNode {
    size_estimate: Option<SizeEstimate>,
    info: RenderNodeInfo,
}

impl RenderNode {
    /// Create a node from the RenderNodeInfo.
    pub fn new(info: RenderNodeInfo) -> RenderNode {
        RenderNode {
            size_estimate: None,
            info: info,
        }
    }

    /// Get a size estimate (~characters)
    pub fn get_size_estimate(&mut self) -> SizeEstimate {
        // If it's already calculated, then just return the answer.
        if let Some(s) = self.size_estimate {
            return s;
        };

        use RenderNodeInfo::*;

        // Otherwise, make an estimate.
        let estimate = match self.info {
            Text(ref t) |
            Img(ref t) |
            Pre(ref t) => {
                let len = t.trim().len();
                SizeEstimate {
                    size: len,
                    min_width: if len > 0 { MIN_WIDTH } else { 0 },
                }
            },

            Container(ref mut v) |
            Link(_, ref mut v) |
            Em(ref mut v) |
            Strong(ref mut v) |
            Code(ref mut v) |
            Block(ref mut v) |
            Header(_, ref mut v) |
            Div(ref mut v) |
            BlockQuote(ref mut v) |
            Ul(ref mut v) |
            Ol(ref mut v) => {
                v.iter_mut()
                 .map(RenderNode::get_size_estimate)
                 .fold(Default::default(), SizeEstimate::add)
            },
            Break => SizeEstimate { size: 1, min_width: 1 },
            Table(ref mut t) => {
                t.get_size_estimate()
            },
            TableRow(_)|TableBody(_)|TableCell(_) => {
                unimplemented!()
            },
        };
        self.size_estimate = Some(estimate);
        estimate
    }
}

/// Make a Vec of RenderNodes from the children of a node.
fn children_to_render_nodes<T:Write>(handle: Handle, err_out: &mut T) -> Vec<RenderNode> {
    /* process children, but don't add anything */
    let children = handle.children
                         .borrow()
                         .iter()
                         .flat_map(|ch| dom_to_render_tree(ch.clone(), err_out))
                         .collect();
    children
}

/// Make a Vec of RenderNodes from the <li>children of a node.
fn list_children_to_render_nodes<T:Write>(handle: Handle, err_out: &mut T) -> Vec<RenderNode> {
    let mut children = Vec::new();

    for child in handle.children.borrow().iter() {
        match child.data {
            Element { ref name, .. } => {
                match name.expanded() {
                    expanded_name!(html "li") => {
                        let li_children = children_to_render_nodes(child.clone(), err_out);
                        children.push(RenderNode::new(RenderNodeInfo::Block(li_children)));
                    },
                    _ => {},
                }
            },
            Comment { .. } => {},
            _ => { html_trace!("Unhandled in list: {:?}\n", child); },
        }
    }
    children
}

/// Convert a table into a RenderNode
fn table_to_render_tree<T:Write>(handle: Handle, _err_out: &mut T) ->  TreeMapResult<(), Handle, RenderNode> {
    pending(handle, |_,rowset| {
        eprintln!("making a table");
        let mut rows = vec![];
        for bodynode in rowset {
            if let RenderNodeInfo::TableBody(body) = bodynode.info {
                rows.extend(body);
            } else {
                html_trace!("Found in table: {:?}", body);
            }
        }
        Some(RenderNode::new(RenderNodeInfo::Table(RenderTable::new(rows))))
    })
}

/// Add rows from a thead or tbody.
fn tbody_to_render_tree<T:Write>(handle: Handle, _err_out: &mut T) ->  TreeMapResult<(), Handle, RenderNode> {
    pending(handle, |_,rowchildren| {
        eprintln!("Making a tbody");
        let rows = rowchildren.into_iter()
                              .flat_map(|rownode| {
                                  if let RenderNodeInfo::TableRow(row) = rownode.info {
                                      Some(row)
                                  } else {
                                      html_trace!("  [[tbody child: {:?}]]", rownode);
                                      None
                                  }})
                              .collect();
        Some(RenderNode::new(RenderNodeInfo::TableBody(rows)))
    })
}

/// Convert a table row to a RenderTableRow
fn tr_to_render_tree<T:Write>(handle: Handle, _err_out: &mut T) ->  TreeMapResult<(), Handle, RenderNode> {
    pending(handle, |_, cellnodes| {
        eprintln!("Making a tr");
        let cells = cellnodes.into_iter()
                             .flat_map(|cellnode| {
                                 if let RenderNodeInfo::TableCell(cell) = cellnode.info {
                                     Some(cell)
                                 } else {
                                     html_trace!("  [[tr child: {:?}]]", cellnode);
                                     None
                                 }})
                             .collect();
        Some(RenderNode::new(RenderNodeInfo::TableRow(RenderTableRow{cells, col_sizes: None})))
    })
}

/// Convert a single table cell to a render node.
fn td_to_render_tree<T:Write>(handle: Handle, _err_out: &mut T) ->  TreeMapResult<(), Handle, RenderNode> {
    let mut colspan = 1;
    if let Element { ref attrs, .. } = handle.data {
        for attr in attrs.borrow().iter() {
            if &attr.name.local == "colspan" {
                let v:&str = &*attr.value;
                colspan = v.parse().unwrap_or(1);
            }
        }
    }
    pending(handle, move |_, children| {
        eprintln!("Making a td");
        Some(RenderNode::new(RenderNodeInfo::TableCell(RenderTableCell {
            colspan: colspan,
            content: children,
            size_estimate: None,
            col_width: None,
        })))
    })
}

/// A reducer which combines results from mapping children into
/// the result for the current node.  Takes a context and a
/// vector of results and returns a new result (or nothing).
type ResultReducer<C, R> = Fn(&mut C, Vec<R>) -> Option<R>;

/// A closure to call before processing a child node.
type ChildPreFn<C, N> = Fn(&mut C, &N);

/// A closure to call after processing a child node,
/// before adding the result to the processed results
/// vector.
type ChildPostFn<C, R> = Fn(&mut C, &R);

/// The result of trying to render one node.
enum TreeMapResult<C, N, R> {
    /// A completed result.
    Finished(R),
    /// Deferred completion - can be turned into a result
    /// once the vector of children are processed.
    PendingChildren {
        children: Vec<N>,
        cons: Box<ResultReducer<C, R>>,
        prefn: Option<Box<ChildPreFn<C, N>>>,
        postfn: Option<Box<ChildPostFn<C, R>>>,
    },
    /// Nothing (e.g. a comment or other ignored element).
    Nothing
}

fn tree_map_reduce<C, N, R, M>(context: &mut C,
                               top: N,
                               mut process_node: M) -> Option<R>
    where M: FnMut(&mut C, N) -> TreeMapResult<C, N, R>,
{
    /// A node partially decoded, waiting for its children to
    /// be processed.
    struct PendingNode<C, R, N> {
        /// How to make the node once finished
        construct: Box<ResultReducer<C, R>>,
        /// Called before processing each child
        prefn: Option<Box<ChildPreFn<C, N>>>,
        /// Called after processing each child
        postfn: Option<Box<ChildPostFn<C, R>>>,
        /// Children already processed
        children: Vec<R>,
        /// Iterator of child nodes not yet processed
        to_process: std::vec::IntoIter<N>,
    }

    let mut pending_stack = vec![
        PendingNode {
            // We only expect one child, which we'll just return.
            construct: Box::new(|_, mut cs| cs.pop()),
            prefn: None,
            postfn: None,
            children: Vec::new(),
            to_process: vec![top].into_iter(),
        }
    ];
    loop {
        // Get the next child node to process
        let next_node = pending_stack.last_mut()
                                     .unwrap()
                                     .to_process
                                     .next();
        if let Some(h) = next_node {
            pending_stack.last_mut().unwrap().prefn.as_ref().map(|ref f| f(context, &h));
            match process_node(context, h) {
                TreeMapResult::Finished(result) => {
                    pending_stack.last_mut().unwrap().postfn.as_ref().map(|ref f| f(context, &result));
                    pending_stack.last_mut().unwrap().children.push(result);
                }
                TreeMapResult::PendingChildren { children, cons, prefn, postfn } => {
                    pending_stack.push(PendingNode {
                        construct: cons,
                        prefn,
                        postfn,
                        children: Vec::new(),
                        to_process: children.into_iter(),
                    });
                },
                TreeMapResult::Nothing => {},
            };
        } else {
            // No more children, so finally construct the parent.
            let mut completed = pending_stack.pop().unwrap();
            let reduced = (completed.construct)(context, completed.children);
            if let Some(node) = reduced {
                if let Some(parent) = pending_stack.last_mut() {
                    parent.postfn.as_ref().map(|ref f| f(context, &node));
                    parent.children.push(node);
                } else {
                    // Finished the whole stack!
                    break Some(node);
                }
            }
        }
    }
}

/// Convert a DOM tree or subtree into a render tree.
pub fn dom_to_render_tree<T:Write>(handle: Handle, err_out: &mut T) -> Option<RenderNode> {
    let result = tree_map_reduce(&mut (), handle,
                    |_, handle| process_dom_node(handle, err_out),
                );

    html_trace!("### dom_to_render_tree: HTML: {:?}", handle);
    html_trace!("### dom_to_render_tree: out= {:#?}", result);
    result
}

fn pending<F>(handle: Handle, f: F) -> TreeMapResult<(), Handle, RenderNode>
where //for<'a> F: Fn(&'a mut C, Vec<RenderNode>) -> Option<RenderNode>+'static
      for<'r> F: Fn(&'r mut (), std::vec::Vec<RenderNode>) -> Option<RenderNode>+'static
{
    TreeMapResult::PendingChildren{
        children: handle.children.borrow().clone(),
        cons: Box::new(f),
        prefn: None,
        postfn: None
    }
}

fn process_dom_node<T:Write>(handle: Handle, err_out: &mut T) -> TreeMapResult<(), Handle, RenderNode> {
    use TreeMapResult::*;
    use RenderNodeInfo::*;

    match handle.clone().data {
        Document => pending(handle, |&mut (), cs| Some(RenderNode::new(Container(cs)))),
        Comment { .. } => Nothing,
        Element { ref name, ref attrs, .. } => {
            match name.expanded() {
                expanded_name!(html "html") |
                expanded_name!(html "span") |
                expanded_name!(html "body") => {
                    /* process children, but don't add anything */
                    pending(handle, |_,cs| Some(RenderNode::new(Container(cs))))
                },
                expanded_name!(html "link") |
                expanded_name!(html "meta") |
                expanded_name!(html "hr") |
                expanded_name!(html "script") |
                expanded_name!(html "style") |
                expanded_name!(html "head") => {
                    /* Ignore the head and its children */
                    Nothing
                },
                expanded_name!(html "a") => {
                    let borrowed = attrs.borrow();
                    let mut target = None;
                    for attr in borrowed.iter() {
                        if &attr.name.local == "href" {
                            target = Some(&*attr.value);
                            break;
                        }
                    }
                    PendingChildren{
                        children: handle.children.borrow().clone(),
                        cons: if let Some(href) = target {
                                // We need the closure to own the string it's going to use.
                                // Unfortunately that means we ideally want FnOnce; but
                                // that doesn't yet work in a Box.  Box<FnBox()> does, but
                                // is unstable.  So we'll just move a string in and clone
                                // it on use.
                                let href: String = href.into();
                                Box::new(move |_, cs| Some(RenderNode::new(Link(href.clone(), cs))))
                            } else {
                                Box::new(|_, cs| Some(RenderNode::new(Container(cs))))
                            },
                        prefn: None, postfn: None,
                        }
                },
                expanded_name!(html "em") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Em(cs))))
                },
                expanded_name!(html "strong") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Strong(cs))))
                },
                expanded_name!(html "code") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Code(cs))))
                },
                expanded_name!(html "img") => {
                    let borrowed = attrs.borrow();
                    let mut title = None;
                    for attr in borrowed.iter() {
                        if &attr.name.local == "alt" {
                            title = Some(&*attr.value);
                            break;
                        }
                    }
                    if let Some(title) = title {
                        Finished(RenderNode::new(Img(title.into())))
                    } else {
                        Nothing
                    }
                },
                expanded_name!(html "h1") |
                expanded_name!(html "h2") |
                expanded_name!(html "h3") |
                expanded_name!(html "h4") => {
                    let level: usize = name.local[1..].parse().unwrap();
                    pending(handle, move |_, cs| Some(RenderNode::new(Header(level, cs))))
                },
                expanded_name!(html "p") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Block(cs))))
                },
                expanded_name!(html "div") => {
                    pending(handle, |_, cs| Some(RenderNode::new(Div(cs))))
                },
                expanded_name!(html "pre") => {
                    Finished(RenderNode::new(Pre(get_text(handle))))
                },
                expanded_name!(html "br") => {
                    Finished(RenderNode::new(Break))
                }
                expanded_name!(html "table") => {
                    table_to_render_tree(handle.clone(), err_out)
                },
                expanded_name!(html "thead") |
                expanded_name!(html "tbody") => {
                    tbody_to_render_tree(handle.clone(), err_out)
                },
                expanded_name!(html "tr") => {
                    tr_to_render_tree(handle.clone(), err_out)
                },
                expanded_name!(html "th") |
                expanded_name!(html "td") => {
                    td_to_render_tree(handle.clone(), err_out)
                }
                expanded_name!(html "blockquote") => {
                    pending(handle, |_, cs| Some(RenderNode::new(BlockQuote(cs))))
                },
                expanded_name!(html "ul") => {
                    Finished(RenderNode::new(Ul(list_children_to_render_nodes(handle.clone(), err_out))))
                },
                expanded_name!(html "ol") => {
                    Finished(RenderNode::new(Ol(list_children_to_render_nodes(handle.clone(), err_out))))
                },
                _ => {
                    html_trace!("Unhandled element: {:?}\n", name.local);
                    pending(handle, |_, cs| Some(RenderNode::new(Container(cs))))
                    //None
                },
            }
          },
        rcdom::NodeData::Text { contents: ref tstr } => {
            Finished(RenderNode::new(Text((&*tstr.borrow()).into())))
        }
        _ => {
            // NodeData doesn't have a Debug impl.
            write!(err_out, "Unhandled node type.\n").unwrap();
            Nothing
        },
    }
}

/// Context to use during tree parsing.
/// This mainly gives access to a Renderer, but needs to be able to push
/// new ones on for nested structures.
struct BuilderStack<R:Renderer> {
    builders: Vec<R>,
}

impl<R:Renderer> BuilderStack<R> {
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

impl<R:Renderer> Deref for BuilderStack<R> {
    type Target = R;
    fn deref(&self) -> &R {
        self.builders.last().expect("Underflow in BuilderStack")
    }
}

impl<R:Renderer> DerefMut for BuilderStack<R> {
    fn deref_mut(&mut self) -> &mut R {
        self.builders.last_mut().expect("Underflow in BuilderStack")
    }
}

fn render_tree_to_string<T:Write, R:Renderer>(builder: R, tree: RenderNode,
                          err_out: &mut T) -> R {
    let mut bs = BuilderStack::new(builder);
    tree_map_reduce(&mut bs, tree,
        |builders, node| do_render_node(builders, node, err_out),
    );
    bs.into_inner()
}

fn pending2<R: Renderer, F: Fn(&mut BuilderStack<R>, Vec<Option<R>>) -> Option<Option<R>> + 'static>(children: Vec<RenderNode>, f: F) -> TreeMapResult<BuilderStack<R>, RenderNode, Option<R>> {
    TreeMapResult::PendingChildren{
        children: children,
        cons: Box::new(f),
        prefn: None,
        postfn: None
    }
}


fn do_render_node<'a, 'b, T: Write, R: Renderer>(builder: &mut BuilderStack<R>,
                                                 tree: RenderNode,
                                                 err_out: &'b mut T)
  -> TreeMapResult<BuilderStack<R>, RenderNode, Option<R>>
{
    use TreeMapResult::*;
    use RenderNodeInfo::*;
    match tree.info {
        Text(ref tstr) => {
            builder.add_inline_text(tstr);
            Finished(None)
        },
        Container(children) => {
            pending2(children, |_, _| Some(None))
        },
        Link(href, children) => {
            builder.start_link(&href);
            pending2(children, |builder:&mut BuilderStack<R>, _| {
                builder.end_link();
                Some(None)
            })
        },
        Em(children) => {
            builder.start_emphasis();
            pending2(children, |builder:&mut BuilderStack<R>, _| {
                builder.end_emphasis();
                Some(None)
            })
        },
        Strong(children) => {
            builder.start_strong();
            pending2(children, |builder:&mut BuilderStack<R>, _| {
                builder.end_strong();
                Some(None)
            })
        },
        Code(children) => {
            builder.start_code();
            pending2(children, |builder:&mut BuilderStack<R>, _| {
                builder.end_code();
                Some(None)
            })
        },
        Img(title) => {
            builder.add_image(&title);
            Finished(None)
        },
        Block(children) => {
            builder.start_block();
            pending2(children, |builder:&mut BuilderStack<R>, _| {
                builder.end_block();
                Some(None)
            })
        },
        Header(level, children) => {
            let mut sub_builder = builder.new_sub_renderer(builder.width()-(1+level));
            builder.push(sub_builder);
            pending2(children, move |builder: &mut BuilderStack<R>, _| {
                let sub_builder = builder.pop();

                let qs: String = "#".repeat(level) + " ";

                builder.start_block();
                builder.append_subrender(sub_builder, repeat(&qs[..]));
                builder.end_block();
                Some(None)
            })
        },
        Div(children) => {
            builder.new_line();
            pending2(children, |builder:&mut BuilderStack<R>, _| {
                builder.new_line();
                Some(None)
            })
        },
        Pre(ref formatted) => {
            builder.add_preformatted_block(formatted);
            Finished(None)
        },
        BlockQuote(children) => {
            let mut sub_builder = builder.new_sub_renderer(builder.width()-2);
            builder.push(sub_builder);
            pending2(children, |builder: &mut BuilderStack<R>, _| {
                let sub_builder = builder.pop();

                builder.start_block();
                builder.append_subrender(sub_builder, repeat("> "));
                builder.end_block();
                Some(None)
            })
        },
        Ul(items) => {
            builder.start_block();

            TreeMapResult::PendingChildren{
                children: items,
                cons: Box::new(|_, _| Some(None)),
                prefn: Some(Box::new(|builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.new_sub_renderer(builder.width()-2);
                    builder.push(sub_builder);
                })),
                postfn: Some(Box::new(|builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.pop();
                    builder.append_subrender(sub_builder, once("* ").chain(repeat("  ")));
                })),
            }
        },
        Ol(items) => {
            builder.start_block();

            let num_items = items.len();
            let prefix_width = format!("{}", num_items).len() + 2;
            let prefixn = format!("{: <width$}", "", width=prefix_width);
            use std::cell::Cell;
            let i: Cell<_> = Cell::new(1);

            TreeMapResult::PendingChildren{
                children: items,
                cons: Box::new(|_, _| Some(None)),
                prefn: Some(Box::new(move |builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.new_sub_renderer(builder.width()-prefix_width);
                    builder.push(sub_builder);
                })),
                postfn: Some(Box::new(move |builder: &mut BuilderStack<R>, _| {
                    let sub_builder = builder.pop();
                    let prefix1 = format!("{}.", i.get());
                    let prefix1 = format!("{: <width$}", prefix1, width=prefix_width);

                    builder.append_subrender(sub_builder, once(prefix1.as_str()).chain(repeat(prefixn.as_str())));
                    i.set(i.get() + 1);
                })),
            }
        },
        Break => {
            builder.new_line();
            Finished(None)
        },
        Table(tab) => {
            render_table_tree(builder.deref_mut(), tab, err_out)
        },
        TableRow(row) => {
            render_table_row(builder.deref_mut(), row, err_out)
        },
        TableBody(_) => {
            unimplemented!("Unexpected TableBody while rendering")
        },
        TableCell(cell) => {
            render_table_cell(builder.deref_mut(), cell, err_out)
        },
    }
}

fn render_table_tree<T:Write, R:Renderer>(builder: &mut R, mut table: RenderTable, _err_out: &mut T) -> TreeMapResult<BuilderStack<R>, RenderNode, Option<R>>
{
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
                col_sizes[colno + i] = (col_sizes[colno + i]).add(estimate);
            }
            colno += cell.colspan;
        }
    }
    let tot_size: usize = col_sizes.iter().map(|est| est.size).sum();
    let width = builder.width();
    let mut col_widths:Vec<usize> = col_sizes.iter()
                                         .map(|sz| {
                                             if sz.size == 0 {
                                                 0
                                             } else {
                                                 max(sz.size * width / tot_size, sz.min_width)
                                             }
                                          }).collect();
    /* The minimums may have put the total width too high */
    while col_widths.iter().cloned().sum::<usize>() > width {
        let (i, _) = col_widths.iter()
                               .cloned()
                               .enumerate()
                               .max_by_key(|&(colno, width)| (width.saturating_sub(col_sizes[colno].min_width), width, usize::max_value() - colno ))
                               .unwrap();
        col_widths[i] -= 1;
    }
    if !col_widths.is_empty() {
        // Slight fudge; we're not drawing extreme edges, so one of the columns
        // can gets a free character cell from not having a border.
        // make it the last.
        let last = col_widths.len() - 1;
        col_widths[last] += 1;
    }

    builder.start_block();

    builder.add_horizontal_border();

    TreeMapResult::PendingChildren{
        children: table.into_rows(col_widths),
        cons: Box::new(|_, _| Some(None)),
        prefn: Some(Box::new(|_, _| { })),
        postfn: Some(Box::new(|_, _| { })),
    }
}

fn render_table_row<T:Write, R:Renderer>(_builder: &mut R, row: RenderTableRow, _err_out: &mut T) -> TreeMapResult<BuilderStack<R>, RenderNode, Option<R>>
{
    TreeMapResult::PendingChildren{
        children: row.into_cells(),
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
        postfn: Some(Box::new(|_builder: &mut BuilderStack<R>, _| {
        })),
    }
}

fn render_table_cell<T:Write, R:Renderer>(_builder: &mut R, cell: RenderTableCell, _err_out: &mut T) -> TreeMapResult<BuilderStack<R>, RenderNode, Option<R>>
{
    pending2(cell.content, |builder: &mut BuilderStack<R>, _| {
        let sub_builder = builder.pop();
        Some(Some(sub_builder))
    })
}

/// Reads HTML from `input`, and returns a `String` with text wrapped to
/// `width` columns.
pub fn from_read<R>(mut input: R, width: usize) -> String where R: io::Read {
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

    let decorator = PlainDecorator::new();
    let builder = TextRenderer::new(width, decorator);

    let render_tree = dom_to_render_tree(dom.document, &mut Discard{}).unwrap();
    let builder = render_tree_to_string(builder, render_tree, &mut Discard{});
    builder.into_string()
}

/// Reads HTML from `input`, and returns text wrapped to `width` columns.
/// The text is returned as a `Vec<TaggedLine<_>>`; the annotations are vectors
/// of `RichAnnotation`.  The "outer" annotation comes first in the `Vec`.
pub fn from_read_rich<R>(mut input: R, width: usize) -> Vec<TaggedLine<Vec<RichAnnotation>>>
        where R: io::Read
{
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

    let decorator = RichDecorator::new();
    let builder = TextRenderer::new(width, decorator);
    let render_tree = dom_to_render_tree(dom.document, &mut Discard{}).unwrap();
    let builder = render_tree_to_string(builder, render_tree, &mut Discard{});
    builder.into_lines().into_iter().map(RenderLine::into_tagged_line).collect()
}

#[cfg(test)]
mod tests {
    use super::{from_read};

    /// Like assert_eq!(), but prints out the results normally as well
    macro_rules! assert_eq_str {
        ($a:expr, $b:expr) => {
            if $a != $b {
                println!("<<<\n{}===\n{}>>>", $a, $b);
                assert_eq!($a, $b);
            }
        }
    }
    fn test_html(input: &[u8], expected: &str, width: usize) {
        assert_eq_str!(from_read(input, width), expected);
    }

    #[test]
    fn test_table() {
        test_html(br##"
       <table>
         <tr>
           <td>1</td>
           <td>2</td>
           <td>3</td>
         </tr>
       </table>
"##, r#"───┬───┬────
1  │2  │3   
───┴───┴────
"#, 12);
     }

    #[test]
    fn test_thead() {
        test_html(br##"
       <table>
         <thead>
           <tr>
             <th>Col1</th>
             <th>Col2</th>
             <th>Col3</th>
           </tr>
         </thead>
         <tbody>
           <tr>
             <td>1</td>
             <td>2</td>
             <td>3</td>
           </tr>
         </tbody>
       </table>
"##, r#"────┬────┬─────
Col1│Col2│Col3 
────┼────┼─────
1   │2   │3    
────┴────┴─────
"#, 15);
     }

     #[test]
     fn test_colspan() {
        test_html(br##"
       <table>
         <tr>
           <td>1</td>
           <td>2</td>
           <td>3</td>
         </tr>
         <tr>
           <td colspan="2">12</td>
           <td>3</td>
         </tr>
         <tr>
           <td>1</td>
           <td colspan="2">23</td>
         </tr>
       </table>
"##, r#"───┬───┬────
1  │2  │3   
───┴───┼────
12     │3   
───┬───┴────
1  │23      
───┴────────
"#, 12);
     }

     #[test]
     fn test_para() {
        assert_eq_str!(from_read(&b"<p>Hello</p>"[..], 10),
                   "Hello\n");
     }

     #[test]
     fn test_para2() {
        assert_eq_str!(from_read(&b"<p>Hello, world!</p>"[..], 20),
                   "Hello, world!\n");
     }

     #[test]
     fn test_blockquote() {
        assert_eq_str!(from_read(&br#"<p>Hello</p>
        <blockquote>One, two, three</blockquote>
        <p>foo</p>
"#[..], 12), r#"Hello

> One, two,
> three

foo
"#);
     }

     #[test]
     fn test_ul() {
         test_html(br#"
            <ul>
              <li>Item one</li>
              <li>Item two</li>
              <li>Item three</li>
            </ul>
         "#, r#"* Item one
* Item two
* Item
  three
"#, 10);
     }

     #[test]
     fn test_ol1() {
         test_html(br#"
            <ol>
              <li>Item one</li>
              <li>Item two</li>
              <li>Item three</li>
            </ol>
         "#, r#"1. Item one
2. Item two
3. Item
   three
"#, 11);
     }

     #[test]
     fn test_ol2() {
         test_html(br#"
            <ol>
              <li>Item one</li>
              <li>Item two</li>
              <li>Item three</li>
              <li>Item four</li>
              <li>Item five</li>
              <li>Item six</li>
              <li>Item seven</li>
              <li>Item eight</li>
              <li>Item nine</li>
              <li>Item ten</li>
            </ol>
         "#, r#"1.  Item one
2.  Item two
3.  Item three
4.  Item four
5.  Item five
6.  Item six
7.  Item seven
8.  Item eight
9.  Item nine
10. Item ten
"#, 20);
     }

     #[test]
     fn test_strip_nl() {
         test_html(br#"
            <p>
               One
               Two
               Three
            </p>
         "#, "One Two Three\n", 40);
     }
     #[test]
     fn test_strip_nl2() {
         test_html(br#"
            <p>
               One
               <span>
                   Two
               </span>
               Three
            </p>
         "#, "One Two Three\n", 40);
     }
     #[test]
     fn test_strip_nl_tbl() {
         test_html(br#"
           <table>
             <tr>
                <td>
                   One
                   <span>
                       Two
                   </span>
                   Three
                </td>
              </tr>
            </table>
         "#, r"────────────────────
One Two Three       
────────────────────
", 20);
     }
     #[test]
     fn test_unknown_element() {
         test_html(br#"
           <foo>
           <table>
             <tr>
                <td>
                   One
                   <span><yyy>
                       Two
                   </yyy></span>
                   Three
                </td>
              </tr>
            </table>
            </foo>
         "#, r"────────────────────
One Two Three       
────────────────────
", 20);
     }
     #[test]
     fn test_strip_nl_tbl_p() {
         test_html(br#"
           <table>
             <tr>
                <td><p>
                   One
                   <span>
                       Two
                   </span>
                   Three
                </p></td>
              </tr>
            </table>
         "#, r"────────────────────
One Two Three       
────────────────────
", 20);
     }
     #[test]
     fn test_pre() {
         test_html(br#"
           <pre>foo
    bar
  wib   asdf;
</pre>
<p>Hello</p>
         "#, r"foo
    bar
  wib   asdf;

Hello
", 20);
    }
     #[test]
     fn test_link() {
         test_html(br#"
           <p>Hello, <a href="http://www.example.com/">world</a></p>"#, r"Hello, [world][1]

[1] http://www.example.com/
", 80);
    }
     #[test]
     fn test_link2() {
         test_html(br#"
           <p>Hello, <a href="http://www.example.com/">world</a>!</p>"#, r"Hello, [world][1]!

[1] http://www.example.com/
", 80);
     }

     #[test]
     fn test_link3() {
         test_html(br#"
           <p>Hello, <a href="http://www.example.com/">w</a>orld</p>"#, r"Hello, [w][1]orld

[1] http://www.example.com/
", 80);
     }

     #[test]
     fn test_link_wrap() {
         test_html(br#"
           <a href="http://www.example.com/">Hello</a>"#, r"[Hello][1]

[1] http:/
/www.examp
le.com/
", 10);
     }

     #[test]
     fn test_wrap() {
         test_html(br"<p>Hello, world.  Superlongwordreally</p>",
                   r#"Hello,
world.
Superlon
gwordrea
lly
"#, 8);
     }

     #[test]
     fn test_wrap2() {
         test_html(br"<p>Hello, world.  This is a long sentence with a
few words, which we want to be wrapped correctly.</p>",
r#"Hello, world. This
is a long sentence
with a few words,
which we want to be
wrapped correctly.
"#, 20);
     }

     #[test]
     fn test_wrap3() {
         test_html(br#"<p><a href="dest">http://example.org/blah/</a> one two three"#,
r#"[http://example.org/blah/
][1] one two three

[1] dest
"#, 25);
     }

     #[test]
     fn test_div() {
         test_html(br"<p>Hello</p><div>Div</div>",
r#"Hello

Div
"#, 20);
         test_html(br"<p>Hello</p><div>Div</div><div>Div2</div>",
r#"Hello

Div
Div2
"#, 20);
     }

     #[test]
     fn test_img_alt() {
         test_html(br"<p>Hello <img src='foo.jpg' alt='world'></p>",
                   "Hello [world]\n", 80);
     }

     #[test]
     fn test_br() {
         test_html(br"<p>Hello<br/>World</p>",
                   "Hello\nWorld\n", 20);
     }

     #[test]
     fn test_subblock() {
         test_html(br#"<div>
         <div>Here's a <a href="https://example.com/">link</a>.</div>
         <div><ul>
         <li>Bullet</li>
         <li>Bullet</li>
         <li>Bullet</li>
         </ul></div>
         </div>"#,
r"Here's a [link][1].

* Bullet
* Bullet
* Bullet

[1] https://example.com/
", 80);
     }

     #[test]
     fn test_controlchar() {
         test_html("Foo\u{0080}Bar".as_bytes(), "FooBar\n", 80);
         test_html("Foo\u{0080}Bar".as_bytes(), "FooB\nar\n", 4);
         test_html("FooBa\u{0080}r".as_bytes(), "FooB\nar\n", 4);
     }

     #[test]
     fn test_nested_table_1() {
        test_html(br##"
       <table>
         <tr>
           <td>
              <table><tr><td>1</td><td>2</td><td>3</td></tr></table>
           </td>
           <td>
              <table><tr><td>4</td><td>5</td><td>6</td></tr></table>
           </td>
           <td>
              <table><tr><td>7</td><td>8</td><td>9</td></tr></table>
           </td>
         </tr>
         <tr>
           <td>
              <table><tr><td>1</td><td>2</td><td>3</td></tr></table>
           </td>
           <td>
              <table><tr><td>4</td><td>5</td><td>6</td></tr></table>
           </td>
           <td>
              <table><tr><td>7</td><td>8</td><td>9</td></tr></table>
           </td>
         </tr>
         <tr>
           <td>
              <table><tr><td>1</td><td>2</td><td>3</td></tr></table>
           </td>
           <td>
              <table><tr><td>4</td><td>5</td><td>6</td></tr></table>
           </td>
           <td>
              <table><tr><td>7</td><td>8</td><td>9</td></tr></table>
           </td>
         </tr>
       </table>
"##, r#"─┬─┬──┬─┬─┬──┬─┬─┬───
1│2│3 │4│5│6 │7│8│9  
─┼─┼──┼─┼─┼──┼─┼─┼───
1│2│3 │4│5│6 │7│8│9  
─┼─┼──┼─┼─┼──┼─┼─┼───
1│2│3 │4│5│6 │7│8│9  
─┴─┴──┴─┴─┴──┴─┴─┴───
"#, 21);
     }

     #[test]
     fn test_nested_table_2() {
        test_html(br##"
       <table>
         <tr>
           <td>
              <table>
                 <tr><td>1</td><td>a</td></tr>
                 <tr><td>2</td><td>b</td></tr>
              </table>
           </td>
           <td><pre>one
two
three
four
five
</pre>
           </td>
         </tr>
       </table>
"##, r#"─┬───┬─────
1│a  │one  
─┼───│two  
2│b  │three
 │   │four 
 │   │five 
─┴───┴─────
"#, 11);
    }

    #[test]
    fn test_h1() {
        test_html(br##"
       <h1>Hi</h1>
       <p>foo</p>
"##, r#"# Hi

foo
"#, 21);
    }

    #[test]
    fn test_h3() {
        test_html(br##"
       <h3>Hi</h3>
       <p>foo</p>
"##, r#"### Hi

foo
"#, 21);
    }

    // General test that spacing is preserved
    #[test]
    fn test_pre2() {
        test_html(br##"<pre>Hello  sp
  world</pre>"##, r#"Hello  sp
  world
"#, 21);
    }

    // Check that spans work correctly inside <pre>
    #[test]
    fn test_pre_span() {
        test_html(br##"
<pre>Hello <span>$</span>sp
<span>Hi</span> <span>$</span><span>foo</span>
<span>Hi</span> <span>foo</span><span>, </span><span>bar</span>
</pre>"##, r#"Hello $sp
Hi $foo
Hi foo, bar
"#, 21);
    }

    // Check tab behaviour
    #[test]
    fn test_pre_tab() {
        test_html(b"<pre>\tworld</pre>",         "        world\n", 40);
        test_html(b"<pre>H\tworld</pre>",        "H       world\n", 40);
        test_html(b"<pre>He\tworld</pre>",       "He      world\n", 40);
        test_html(b"<pre>Hel\tworld</pre>",      "Hel     world\n", 40);
        test_html(b"<pre>Hell\tworld</pre>",     "Hell    world\n", 40);
        test_html(b"<pre>Hello\tworld</pre>",    "Hello   world\n", 40);
        test_html(b"<pre>Helloo\tworld</pre>",   "Helloo  world\n", 40);
        test_html(b"<pre>Hellooo\tworld</pre>",  "Hellooo world\n", 40);
        test_html(b"<pre>Helloooo\tworld</pre>", "Helloooo        world\n", 40);
    }

    #[test]
    fn test_em_strong() {
        test_html(br##"
       <p>Hi <em>em</em> <strong>strong</strong></p>
"##, r#"Hi *em* **strong**
"#, 21);
    }

    #[test]
    #[ignore]  // Not yet fixed!
    fn test_nbsp_indent() {
        test_html(br##"
       <div>Top</div>
       <div>&nbsp;Indented</div>
       <div>&nbsp;&nbsp;Indented again</div>
"##, r#"Top
 Indented
  Indented again
"#, 21);
    }

    #[test]
    fn test_deeply_nested() {
        use ::std::iter::repeat;
        let html = repeat("<foo>")
                         .take(1000)
                         .collect::<Vec<_>>()
                         .concat();
        test_html(html.as_bytes(), "", 10);
    }
}
