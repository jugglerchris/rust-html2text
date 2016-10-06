#[macro_use]
extern crate string_cache;
extern crate html5ever;

use std::io;
use std::io::Write;
use html5ever::{Parser,parse_document};
use html5ever::driver::ParseOpts;
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::rcdom::{RcDom,Handle,Text,Element,Document,Comment};
use html5ever::tendril::TendrilSink;

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

fn dom_to_string<T:Write>(handle: Handle, err_out: &mut T, width: usize) -> String {
    let node = handle.borrow();
    let mut result = String::new();
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
                qualname!(html, "head") => {
                    /* Ignore the head and its children */
                    return result;
                },
                qualname!(html, "h4") |
                qualname!(html, "p") => {
                    return get_text(handle.clone()) + "\n\n";
                },
                qualname!(html, "br") => {
                    result.push('\n');
                }
                qualname!(html, "table") => return table_to_string(handle.clone(), err_out, width),
                _ => {
                    write!(err_out, "Unhandled element: {:?}\n", name.local);
                },
            }
          },
        Text(ref tstr) => {
            return tstr.to_string();
        }
        Comment(_) => {},
        _ => { write!(err_out, "Unhandled: {:?}\n", node); },
    }
    for child in node.children.iter() {
        result.push_str(&dom_to_string(child.clone(), err_out, width));
    }
    result
}

#[derive(Debug)]
struct TableCell {
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
    pub fn num_cells(&self) -> usize {
        self.cells.len()
    }
}

impl TableCell {
    pub fn new(s: Handle) -> TableCell {
        TableCell{ content: s }
    }
    pub fn render<T:Write>(&self, width: usize, err_out: &mut T) -> String
    {
        dom_to_string(self.content.clone(), err_out, width)
    }
}

fn handle_td(handle: Handle) -> TableCell {
    TableCell::new(handle)
}

fn handle_tr<T:Write>(handle: Handle, err_out: &mut T) -> TableRow {
    let node = handle.borrow();

    let mut row = TableRow::new();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, ref attrs) => {
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
            Element(ref name, _, ref attrs) => {
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

    // Allow for | between columns.
    let col_width = (width - (num_columns-1))/num_columns;


    let cell_bottom: String = (0..col_width).map(|_| '-').collect::<String>() + "+";
    let rowline: String = (0..num_columns-1).map(|_| &cell_bottom[..]).collect::<String>() + &cell_bottom[..col_width];

    let mut result = rowline.clone();
    result.push('\n');

    for row in table.rows() {
        let formatted_cells: Vec<String> = row.cells()
                                              .map(|cell| cell.render(col_width, err_out))
                                              .collect();
        let line_sets: Vec<Vec<&str>> = formatted_cells.iter()
                                                       .map(|s| s.lines()
                                                                 .map(|line| line.trim_right())
                                                                 .collect())
                                                       .collect();
        let cell_height = line_sets.iter()
                                   .map(|v| v.len())
                                   .max().unwrap();
        for i in 0..cell_height {
            for (cellno, ls) in line_sets.iter().enumerate() {
                result.push_str(&format!("{: <width$}", ls.get(i).cloned().unwrap_or(""), width = col_width));
                if cellno == line_sets.len()-1 {
                    result.push('\n')
                } else {
                    result.push('|')
                }
            }
        }
        result.push_str(&rowline);
        result.push('\n');
    }

    result
}

fn table_to_string<T:Write>(handle: Handle, err_out: &mut T, width: usize) -> String {
    let node = handle.borrow();
    let mut result = String::new();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, ref attrs) => {
                match *name {
                    qualname!(html, "tbody") => return handle_tbody(child.clone(), err_out, width),
                    _ => { writeln!(err_out, "  [[table child: {:?}]]", name);},
                }
            },
            Comment(_) => {},
            _ => { /*result.push_str(&format!("Unhandled in table: {:?}\n", node));*/ },
        }
    }

    result
}

pub fn from_read<R>(mut input: R) -> String where R: io::Read {
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

    dom_to_string(dom.document, &mut io::stderr(), 80)
}

#[cfg(test)]
mod tests {
}
