#[macro_use]
extern crate string_cache;
extern crate html5ever;
extern crate unicode_width;

use std::io;
use std::io::Write;
use std::cmp::max;
use html5ever::{parse_document};
use html5ever::driver::ParseOpts;
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::rcdom::{RcDom,Handle,Text,Element,Document,Comment};
use html5ever::tendril::TendrilSink;
use unicode_width::{UnicodeWidthStr,UnicodeWidthChar};

/// A dummy writer which does nothing
struct Discard {}
impl Write for Discard {
    fn write(&mut self, bytes: &[u8]) -> std::result::Result<usize, io::Error> { Ok(bytes.len()) }
    fn flush(&mut self) -> std::result::Result<(), io::Error> { Ok(()) }
}

fn wrap_text(text: &str, width: usize) -> String {
    let mut result = String::new();
    let mut xpos = 0usize;
    for word in text.split_whitespace() {
        if width <= (xpos + 1) {
            result.push('\n');
            xpos = 0;
        }
        let space_left = width - xpos - 1;
        let word_width = UnicodeWidthStr::width(word);
        if word_width <= space_left {
            /* It fits; no problem.  Add a space if not at the
             * start of line.*/
            if xpos > 0 {
                result.push(' ');
                xpos += 1;
            }
            result.push_str(word);
            xpos += word_width;
            continue;
        }

        /* It doesn't fit.  If we're not at the start of the line,
         * then go to a new line. */
        if xpos > 0 {
            result.push('\n');
            xpos = 0;
        }

        /* We're now at the start of a line. */
        if word_width > width {
            /* It doesn't fit at all on the line, so break it. */
            for c in word.chars() {
                let c_width = UnicodeWidthChar::width(c).unwrap();
                if c_width + xpos > width {
                    /* Break here */
                    result.push('\n');
                    xpos = 0;
                }
                /* This might happen with really narrow spaces... */
                assert!(c_width <= width);

                result.push(c);
                xpos += c_width;
            }
        } else {
            result.push_str(word);
            xpos += word_width;
        }
    }
    /* Terminate with a newline if needed. */
    if xpos > 0 {
        result.push('\n');
    }
    result
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

fn get_wrapped_text(handle: Handle, width: usize) -> String {
    let text = get_text(handle);
    wrap_text(&text, width)
}

fn dom_to_string<T:Write>(handle: Handle, err_out: &mut T, width: usize) -> String {
    let node = handle.borrow();
    let mut result = String::new();
    match node.node {
        Document => {},
        Element(ref name, _, _) => {
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
                qualname!(html, "head") => {
                    /* Ignore the head and its children */
                    return result;
                },
                qualname!(html, "h1") |
                qualname!(html, "h2") |
                qualname!(html, "h3") |
                qualname!(html, "h4") |
                qualname!(html, "p") => {
                    return get_wrapped_text(handle.clone(), width) + "\n";
                },
                qualname!(html, "br") => {
                    result.push('\n');
                }
                qualname!(html, "table") => return table_to_string(handle.clone(), err_out, width),
                _ => {
                    write!(err_out, "Unhandled element: {:?}\n", name.local).unwrap();
                },
            }
          },
        Text(ref tstr) => {
            return wrap_text(tstr, width);
        }
        Comment(_) => {},
        _ => { write!(err_out, "Unhandled: {:?}\n", node).unwrap(); },
    }
    for child in node.children.iter() {
        result.push_str(&dom_to_string(child.clone(), err_out, width));
    }
    result
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
    pub fn render<T:Write>(&self, width: usize, err_out: &mut T) -> String
    {
        dom_to_string(self.content.clone(), err_out, width)
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

fn handle_tbody<T:Write>(handle: Handle, err_out: &mut T, width: usize) -> String {
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

    let mut result = String::new();

    /* Heuristic: scale the column widths according to how much content there is. */
    let test_col_width = 1000;  // Render width for measurement; shouldn't make much difference.
    let min_width = 5;
    let mut col_sizes = vec![0usize; num_columns];

    for row in table.rows() {
        let mut colno = 0;
        for cell in row.cells() {
            let celldata = cell.render(test_col_width, &mut Discard{});
            // If the cell has a colspan>1, then spread its size between the
            // columns.
            let col_size = celldata.len() / cell.colspan;
            for i in 0..cell.colspan {
                col_sizes[colno + i] += col_size;
            }
            colno += cell.colspan;
        }
    }
    let tot_size: usize = col_sizes.iter().sum();
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

    let mut rowline = String::new();
    for width in col_widths.iter().cloned().filter(|w:&usize| *w > 0) {
        rowline.push_str(&(0..(width-1)).map(|_| '-').collect::<String>());
        rowline.push('+');
    }
    if rowline.len() > 0 {
        rowline.pop().unwrap();  // Remove the last '+'.
    }
    result.push_str(&rowline);
    result.push('\n');

    for row in table.rows() {
        let (used_widths, formatted_cells): (Vec<usize>, Vec<String>) = row.cell_columns()
                                              .into_iter()
                                              .flat_map(|(colno, cell)| {
                                                   let col_width:usize = col_widths[colno..colno+cell.colspan]
                                                                      .iter().sum();
                                                   if col_width > 0 {
                                                       Some((col_width, cell.render(col_width-1, err_out)))
                                                   } else {
                                                       None
                                                   }
                                               })
                                              .unzip();
        let line_sets: Vec<Vec<&str>> = formatted_cells.iter()
                                                       .map(|s| s.lines()
                                                                 .map(|line| line.trim_right())
                                                                 .collect())
                                                       .collect();
        let cell_height = line_sets.iter()
                                   .map(|v| v.len())
                                   .max().unwrap_or(0);
        for i in 0..cell_height {
            for (cellno, ls) in line_sets.iter().enumerate() {
                result.push_str(&format!("{: <width$}", ls.get(i).cloned().unwrap_or(""), width = used_widths[cellno]-1));
                if cellno == line_sets.len()-1 {
                    result.push('\n')
                } else {
                    result.push('|')
                }
            }
        }
        if cell_height > 0 {
            result.push_str(&rowline);
            result.push('\n');
        }
    }

    result
}

fn table_to_string<T:Write>(handle: Handle, err_out: &mut T, width: usize) -> String {
    let node = handle.borrow();
    let result = String::new();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, _) => {
                match *name {
                    qualname!(html, "tbody") => return handle_tbody(child.clone(), err_out, width),
                    _ => { writeln!(err_out, "  [[table child: {:?}]]", name).unwrap();},
                }
            },
            Comment(_) => {},
            _ => { /*result.push_str(&format!("Unhandled in table: {:?}\n", node));*/ },
        }
    }

    result
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

    dom_to_string(dom.document, &mut io::stderr(), width)
}

#[cfg(test)]
mod tests {
    use super::{from_read,wrap_text};
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
"##[..], 12), r#"---+---+---
1  |2  |3  
---+---+---
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
"##[..], 12), r#"---+---+---
1  |2  |3  
---+---+---
12     |3  
---+---+---
1  |23     
---+---+---
"#);
     }

     #[test]
     fn test_para() {
        assert_eq!(from_read(&b"<p>Hello</p>"[..], 10),
                   "Hello\n\n");
     }

     #[test]
     fn test_wrap() {
        assert_eq!(wrap_text("Hello", 10), "Hello\n");
     }
}
