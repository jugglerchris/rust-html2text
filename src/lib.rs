#[macro_use]
extern crate string_cache;
extern crate html5ever;
extern crate unicode_width;
extern crate backtrace;

#[macro_use]
mod macros;

pub mod render;

use render::Renderer;
use render::text_renderer::TextRenderer;

use std::io;
use std::io::Write;
use std::cmp::max;
use std::iter::{once,repeat};
use html5ever::{parse_document};
use html5ever::driver::ParseOpts;
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::rcdom::{RcDom,Handle,Text,Element,Document,Comment};
use html5ever::tendril::TendrilSink;

/// A dummy writer which does nothing
struct Discard {}
impl Write for Discard {
    fn write(&mut self, bytes: &[u8]) -> std::result::Result<usize, io::Error> { Ok(bytes.len()) }
    fn flush(&mut self) -> std::result::Result<(), io::Error> { Ok(()) }
}

fn get_text(handle: Handle) -> String {
    let node = handle.borrow();
    let mut result = String::new();
    if let Text(ref tstr) = node.node {
        result.push_str(&tstr);
    } else {
        for child in node.children.iter() {
            result.push_str(&get_text(child.clone()));
        }
    }
    result
}

fn render_block<T:Write, R:Renderer>(builder: &mut R, handle: Handle,
                         err_out: &mut T) {
    builder.start_block();
    render_children(builder, handle, err_out);
}

fn render_pre<T:Write, R:Renderer>(builder: &mut R, handle: Handle, _: &mut T) {
    builder.add_preformatted_block(&get_text(handle));
}

fn render_children<T:Write, R:Renderer>(builder: &mut R, handle: Handle,
                            err_out: &mut T) {
    for child in handle.borrow().children.iter() {
        dom_to_string(builder, child.clone(), err_out);
    }
}

fn dom_to_string<T:Write, R:Renderer>(builder: &mut R, handle: Handle,
                          err_out: &mut T) {
    let node = handle.borrow();
    match node.node {
        Document => {},
        Element(ref name, _, ref attrs) => {
            match *name {
                qualname!(html, "html") |
                qualname!(html, "div") |
                qualname!(html, "span") |
                qualname!(html, "body") => {
                    /* process children, but don't add anything */
                },
                qualname!(html, "link") |
                qualname!(html, "meta") |
                qualname!(html, "hr") |
                qualname!(html, "script") |
                qualname!(html, "style") |
                qualname!(html, "head") => {
                    /* Ignore the head and its children */
                    return;
                },
                qualname!(html, "a") => {
                    let mut target = None;
                    for attr in attrs {
                        if &attr.name.local == "href" {
                            target = Some(&*attr.value);
                            break;
                        }
                    }
                    if let Some(href) = target {
                        builder.start_link(href);
                        render_children(builder, handle.clone(), err_out);
                        builder.end_link();
                    } else {
                        render_children(builder, handle.clone(), err_out);
                    }
                    return;
                },
                qualname!(html, "h1") |
                qualname!(html, "h2") |
                qualname!(html, "h3") |
                qualname!(html, "h4") |
                qualname!(html, "p") => {
                    return render_block(builder, handle.clone(), err_out);
                },
                qualname!(html, "pre") => {
                    return render_pre(builder, handle.clone(), err_out);
                },
                qualname!(html, "br") => {
                    builder.add_empty_line();
                    return;
                }
                qualname!(html, "table") => return render_table(builder, handle.clone(), err_out),
                qualname!(html, "blockquote") => return render_blockquote(builder, handle.clone(), err_out),
                qualname!(html, "ul") => return render_ul(builder, handle.clone(), err_out),
                _ => {
                    write!(err_out, "Unhandled element: {:?}\n", name.local).unwrap();
                },
            }
          },
        Text(ref tstr) => {
            builder.add_inline_text(tstr);
            return;
        }
        Comment(_) => {},
        _ => { write!(err_out, "Unhandled: {:?}\n", node).unwrap(); },
    }
    render_children(builder, handle.clone(), err_out);
}

#[derive(Debug)]
struct TableCell {
    colspan: usize,
    content: Handle,
}

#[derive(Debug)]
struct TableRow {
    cells: Vec<TableCell>,
}

#[derive(Debug)]
struct Table {
    rows: Vec<TableRow>,
}

impl Table {
    pub fn new() -> Table {
        Table{ rows: Vec::new() }
    }
    pub fn push(&mut self, row: TableRow) {
        self.rows.push(row);
    }
    pub fn rows(&self) -> std::slice::Iter<TableRow> {
        self.rows.iter()
    }
}

impl TableRow {
    pub fn new() -> TableRow {
        TableRow{ cells: Vec::new() }
    }
    pub fn push(&mut self, cell: TableCell) {
        self.cells.push(cell);
    }
    pub fn cells(&self) -> std::slice::Iter<TableCell> {
        self.cells.iter()
    }
    /// Return an iterator over (column, &cell)s, which
    /// takes into account colspan.
    pub fn cell_columns(&self) -> Vec<(usize, &TableCell)> {
        let mut result = Vec::new();
        let mut colno = 0;
        for cell in &self.cells {
            result.push((colno, cell));
            colno += cell.colspan;
        }
        result
    }
    /// Count the number of cells in the row.
    /// Takes into account colspan.
    pub fn num_cells(&self) -> usize {
        self.cells.iter().map(|cell| cell.colspan).sum()
    }
}

impl TableCell {
    pub fn new(s: Handle) -> TableCell {
        if let Element(_, _, ref attrs) = s.borrow().node {
            let mut colspan = 1;
            for attr in attrs {
                if &attr.name.local == "colspan" {
                    let v:&str = &*attr.value;
                    colspan = v.parse().unwrap_or(1);
                } else {
                    //println!("Attr: {:?}", attr);
                }
            }
            TableCell{ content: s.clone(), colspan: colspan }
        } else {
            panic!("TableCell::new received a non-Element");
        }
    }
    pub fn render<T:Write, R:Renderer>(&self, builder: &mut R, err_out: &mut T)
    {
        dom_to_string(builder, self.content.clone(), err_out)
    }
}

fn handle_td(handle: Handle) -> TableCell {
    TableCell::new(handle)
}

fn handle_tr<T:Write>(handle: Handle, _: &mut T) -> TableRow {
    let node = handle.borrow();

    let mut row = TableRow::new();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, _) => {
                match *name {
                    qualname!(html, "th") |
                    qualname!(html, "td") => {
                        row.push(handle_td(child.clone()));
                    },
                    _ => println!("  [[tr child: {:?}]]", name),
                }
            },
            Comment(_) => {},
            _ => { /*result.push_str(&format!("Unhandled in table: {:?}\n", node));*/ },
        }
    }

    row
}

fn handle_tbody<T:Write, R:Renderer>(builder: &mut R, handle: Handle, err_out: &mut T) {
    let node = handle.borrow();

    let mut table = Table::new();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, _) => {
                match *name {
                    qualname!(html, "tr") => {
                        table.push(handle_tr(child.clone(), err_out));
                    },
                    _ => println!("  [[tbody child: {:?}]]", name),
                }
            },
            Comment(_) => {},
            _ => { /*result.push_str(&format!("Unhandled in table: {:?}\n", node));*/ },
        }
    }

    /* Now lay out the table.  Use the simple option of giving each column
     * same width.  TODO: be cleverer, and handle multi-width cells, etc. */
    let num_columns = table.rows().map(|r| r.num_cells()).max().unwrap();

    /* Heuristic: scale the column widths according to how much content there is. */
    let test_col_width = 1000;  // Render width for measurement; shouldn't make much difference.
    let min_width = 5;
    let mut col_sizes = vec![0usize; num_columns];

    for row in table.rows() {
        let mut colno = 0;
        for cell in row.cells() {
            let mut cellbuilder = builder.new_sub_renderer(test_col_width);
            cell.render(&mut cellbuilder, &mut Discard{});
            let cellsize = cellbuilder.text_len();
            // If the cell has a colspan>1, then spread its size between the
            // columns.
            let col_size = cellsize / cell.colspan;
            for i in 0..cell.colspan {
                col_sizes[colno + i] += col_size;
            }
            colno += cell.colspan;
        }
    }
    let tot_size: usize = col_sizes.iter().sum();
    let width = builder.width();
    let mut col_widths:Vec<usize> = col_sizes.iter()
                                         .map(|sz| {
                                             if *sz == 0 {
                                                 0
                                             } else {
                                                 max(sz * width / tot_size, min_width)
                                             }
                                          }).collect();
    /* The minimums may have put the total width too high */
    while col_widths.iter().cloned().sum::<usize>() > width {
        let (i, _) = col_widths.iter().cloned().enumerate().max_by_key(|k| k.1).unwrap();
        col_widths[i] -= 1;
    }
    if col_widths.len() > 0 {
        // Slight fudge; we're not drawing extreme edges, so one of the columns
        // can gets a free character cell from not having a border.
        // make it the last.
        let last = col_widths.len() - 1;
        col_widths[last] += 1;
    }

    builder.start_block();

    let mut rowline = String::new();
    for width in col_widths.iter().cloned().filter(|w:&usize| *w > 0) {
        rowline.push_str(&(0..(width-1)).map(|_| '-').collect::<String>());
        rowline.push('+');
    }
    if rowline.len() > 0 {
        rowline.pop().unwrap();  // Remove the last '+'.
    }
    builder.add_block_line(&rowline);

    for row in table.rows() {
        let rendered_cells: Vec<R::Sub> = row.cell_columns()
                                             .into_iter()
                                             .flat_map(|(colno, cell)| {
                                                  let col_width:usize = col_widths[colno..colno+cell.colspan]
                                                                     .iter().sum();
                                                  if col_width > 0 {
                                                      let mut cellbuilder = builder.new_sub_renderer(col_width-1);
                                                      cell.render(&mut cellbuilder, err_out);
                                                      Some(cellbuilder)
                                                  } else {
                                                      None
                                                  }
                                              }).collect();
        if rendered_cells.iter().any(|r| !r.empty()) {
            builder.append_columns(rendered_cells, "|");
            builder.add_block_line(&rowline);
        }
    }
}

fn render_table<T:Write, R:Renderer>(builder: &mut R, handle: Handle, err_out: &mut T) {
    let node = handle.borrow();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, _) => {
                match *name {
                    qualname!(html, "tbody") => return handle_tbody(builder, child.clone(), err_out),
                    _ => { writeln!(err_out, "  [[table child: {:?}]]", name).unwrap();},
                }
            },
            Comment(_) => {},
            _ => { /*result.push_str(&format!("Unhandled in table: {:?}\n", node));*/ },
        }
    }
}

fn prepend_block(block: &str, prefix: &str) -> String {
    let mut result = String::new();
    for line in block.lines() {
        result.push_str(prefix);
        result.push_str(line);
        result.push('\n');
    }
    result
}

fn prepend_first_line(block: &str, prefix: &str) -> String {
    let mut result = String::new();
    let mut lines = block.lines();
    if let Some(line) = lines.next() {
        result.push_str(prefix);
        result.push_str(line);
        result.push('\n');
    }
    let ws_prefix: String = (0..prefix.len()).map(|_| ' ').collect();
    for line in lines {
        result.push_str(&ws_prefix);
        result.push_str(line);
        result.push('\n');
    }
    result
}


fn render_blockquote<T:Write, R:Renderer>(builder: &mut R, handle: Handle, err_out: &mut T) {

    let mut sub_builder = builder.new_sub_renderer(builder.width()-2);
    render_children(&mut sub_builder, handle, err_out);

    builder.start_block();
    builder.append_subrender(sub_builder, repeat("> "));
    builder.end_block();
}

fn render_ul<T:Write, R:Renderer>(builder: &mut R, handle: Handle, err_out: &mut T) {
    let node = handle.borrow();

    builder.start_block();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, _) => {
                match *name {
                    qualname!(html, "li") => {
                        let mut sub_builder = builder.new_sub_renderer(builder.width()-2);
                        render_block(&mut sub_builder, child.clone(), err_out);
                        builder.append_subrender(sub_builder, once("* ").chain(repeat("  ")));
                    },
                    _ => println!("  [[ul child: {:?}]]", name),
                }
            },
            Comment(_) => {},
            _ => { /*result.push_str(&format!("Unhandled in table: {:?}\n", node));*/ },
        }
    }
}

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

    let mut builder = TextRenderer::new(width);
    dom_to_string(&mut builder, dom.document, &mut Discard{} /* &mut io::stderr()*/);
    builder.into_string()
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
        assert_eq!(from_read(&br##"
       <table>
         <tr>
           <td>1</td>
           <td>2</td>
           <td>3</td>
         </tr>
       </table>
"##[..], 12), r#"---+---+----
1  |2  |3   
---+---+----
"#);
     }

     #[test]
     fn test_colspan() {
        assert_eq!(from_read(&br##"
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
"##[..], 12), r#"---+---+----
1  |2  |3   
---+---+----
12     |3   
---+---+----
1  |23      
---+---+----
"#);
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
         "#, r"--------------------
One Two Three       
--------------------
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
         "#, r"--------------------
One Two Three       
--------------------
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
}
