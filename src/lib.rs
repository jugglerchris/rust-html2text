#[macro_use]
extern crate string_cache;
extern crate html5ever;
extern crate unicode_width;
extern crate backtrace;

#[cfg(html_trace)]
macro_rules! html_trace {
    ($fmt:expr) => {
         let bt = ::backtrace::Backtrace::new();
         println!( concat!($fmt, " at {:?}"), bt );
    };
    ($fmt:expr, $( $args:expr ),*) => {
         let bt = ::backtrace::Backtrace::new();
         println!( concat!($fmt, " at {:?}"), $( $args ),* , bt );
    };
}
#[cfg(not(html_trace))]
macro_rules! html_trace {
    ($fmt:expr) => {};
    ($fmt:expr, $( $args:expr ),*) => {};
}

#[cfg(html_trace)]
macro_rules! html_trace_quiet {
    ($fmt:expr) => {
         println!( $fmt );
    };
    ($fmt:expr, $( $args:expr ),*) => {
         println!( $fmt, $( $args ),* );
    };
}

#[cfg(not(html_trace))]
macro_rules! html_trace_quiet {
    ($fmt:expr) => {};
    ($fmt:expr, $( $args:expr ),*) => {};
}
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

/// State corresponding to a partially constructed line.
struct PartialLine {
    text: String,
    /// The width in character cells of the text, or
    /// current position.
    pos: usize,
}

impl PartialLine {
    pub fn new() -> PartialLine {
        PartialLine {
            text: String::new(),
            pos: 0,
        }
    }
}

/// A struct which is passed either inline text or blocks and appends
/// them to its output, wrapping as needed.
struct HtmlBuilder {
    width: usize,
    lines: Vec<String>,
    /// True at the end of a block, meaning we should add
    /// a blank line if any other text is added.
    at_block_end: bool,
    partial_line: Option<PartialLine>,
}

impl HtmlBuilder {
    /// Construct a new empty HtmlBuilder.
    pub fn new(width: usize) -> HtmlBuilder {
        html_trace!("new({})", width);
        HtmlBuilder {
            width: width,
            lines: Vec::new(),
            at_block_end: false,
            partial_line: None,
        }
    }

    /// Take the partial line out of self, or create one
    /// if there wasn't one.
    fn take_partial_line(&mut self) -> PartialLine {
        if let Some(p) = self.partial_line.take() {
            p
        } else {
            PartialLine::new()
        }
    }

    pub fn start_block(&mut self) {
        html_trace!("start_block({})", self.width);
        self.flush_line();
        if self.lines.len() > 0 {
            self.add_empty_line();
        }
        html_trace_quiet!("start_block; at_block_end <- false");
        self.at_block_end = false;
    }

    pub fn add_inline_text(&mut self, text: &str) {
        html_trace!("add_inline_text({}, {})", self.width, text);
        let mut partial = self.take_partial_line();
        for word in text.split_whitespace() {
            if self.width <= (partial.pos + 1) {
                self.push_line(partial.text);
                partial = self.take_partial_line();
            }
            let space_left = self.width - partial.pos - 1;
            let word_width = UnicodeWidthStr::width(word);
            if word_width <= space_left {
                /* It fits; no problem.  Add a space if not at the
                 * start of line.*/
                if partial.pos > 0 {
                    partial.text.push(' ');
                    partial.pos += 1;
                }
                partial.text.push_str(word);
                partial.pos += word_width;
                continue;
            }

            /* It doesn't fit.  If we're not at the start of the line,
             * then go to a new line. */
            if partial.pos > 0 {
                self.push_line(partial.text);
                partial = self.take_partial_line();
            }

            /* We're now at the start of a line. */
            if word_width > self.width {
                /* It doesn't fit at all on the line, so break it. */
                for c in word.chars() {
                    let c_width = UnicodeWidthChar::width(c).unwrap();
                    if c_width + partial.pos > self.width {
                        /* Break here */
                        self.push_line(partial.text);
                        partial = self.take_partial_line();
                    }
                    /* This might happen with really narrow spaces... */
                    assert!(c_width <= self.width);

                    partial.text.push(c);
                    partial.pos += c_width;
                }
            } else {
                partial.text.push_str(word);
                partial.pos += word_width;
            }
        }
        self.partial_line = Some(partial);
    }

    pub fn add_block(&mut self, s: &str) {
        html_trace!("add_block({}, {})", self.width, s);
        self.start_block();
        self.add_subblock(s);
        html_trace_quiet!("add_block: at_block_end <- true");
        self.at_block_end = true;
    }
    pub fn add_subblock(&mut self, s: &str) {
        html_trace!("add_subblock({}, {})", self.width, s);
        self.lines.extend(s.lines().map(|l| l.into()));
    }

    fn push_line(&mut self, s: String) {
        self.lines.push(s);
    }
    fn flush_line(&mut self) {
        if let Some(s) = self.partial_line.take() {
            if s.text.len() > 0 {
                self.push_line(s.text);
            }
        }
    }

    pub fn add_empty_line(&mut self) {
        html_trace!("add_empty_line()");
        self.flush_line();
        self.lines.push("".into());
        html_trace_quiet!("add_empty_line: at_block_end <- false");
        self.at_block_end = false;
        html_trace_quiet!("add_empty_line: new lines: {:?}", self.lines);
    }

    pub fn into_string(mut self) -> String {
        self.flush_line();
        let mut result = String::new();
        for s in self.lines.into_iter() {
            result.push_str(&s);
            result.push('\n');
        }
        html_trace!("into_string({}, {:?})", self.width, result);
        result
    }
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

fn render_block<T:Write>(builder: &mut HtmlBuilder, handle: Handle,
                         err_out: &mut T) {
    builder.start_block();
    render_children(builder, handle, err_out);
}

fn render_children<T:Write>(builder: &mut HtmlBuilder, handle: Handle,
                            err_out: &mut T) {
    for child in handle.borrow().children.iter() {
        dom_to_string(builder, child.clone(), err_out);
    }
}

fn dom_to_string<T:Write>(builder: &mut HtmlBuilder, handle: Handle,
                          err_out: &mut T) {
    let node = handle.borrow();
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
                qualname!(html, "script") |
                qualname!(html, "style") |
                qualname!(html, "head") => {
                    /* Ignore the head and its children */
                    return;
                },
                qualname!(html, "h1") |
                qualname!(html, "h2") |
                qualname!(html, "h3") |
                qualname!(html, "h4") |
                qualname!(html, "p") => {
                    return render_block(builder, handle.clone(), err_out);
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
    pub fn render<T:Write>(&self, builder: &mut HtmlBuilder, err_out: &mut T)
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

fn handle_tbody<T:Write>(builder: &mut HtmlBuilder, handle: Handle, err_out: &mut T) {
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
            let mut cellbuilder = HtmlBuilder::new(test_col_width);
            cell.render(&mut cellbuilder, &mut Discard{});
            let celldata = cellbuilder.into_string();
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
    let width = builder.width;
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
    builder.add_subblock(&rowline);

    for row in table.rows() {
        let (used_widths, formatted_cells): (Vec<usize>, Vec<String>) = row.cell_columns()
                                              .into_iter()
                                              .flat_map(|(colno, cell)| {
                                                   let col_width:usize = col_widths[colno..colno+cell.colspan]
                                                                      .iter().sum();
                                                   if col_width > 0 {
                                                       let mut cellbuilder = HtmlBuilder::new(col_width-1);
                                                       cell.render(&mut cellbuilder, err_out);
                                                       Some((col_width, cellbuilder.into_string()))
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
            let mut line = String::new();
            for (cellno, ls) in line_sets.iter().enumerate() {
                line.push_str(&format!("{: <width$}", ls.get(i).cloned().unwrap_or(""), width = used_widths[cellno]-1));
                if cellno != line_sets.len()-1 {
                    line.push('|')
                }
            }
            builder.add_subblock(&line);
        }
        if cell_height > 0 {
            builder.add_subblock(&rowline);
        }
    }
}

fn render_table<T:Write>(builder: &mut HtmlBuilder, handle: Handle, err_out: &mut T) {
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


fn render_blockquote<T:Write>(builder: &mut HtmlBuilder, handle: Handle, err_out: &mut T) {

    let mut sub_builder = HtmlBuilder::new(builder.width-2);
    render_children(&mut sub_builder, handle, err_out);

    let rendered = prepend_block(&sub_builder.into_string(), "> ");
    builder.add_block(&rendered);
}

fn render_ul<T:Write>(builder: &mut HtmlBuilder, handle: Handle, err_out: &mut T) {
    let node = handle.borrow();

    builder.start_block();

    for child in node.children.iter() {
        match child.borrow().node {
            Element(ref name, _, _) => {
                match *name {
                    qualname!(html, "li") => {
                        let mut sub_builder = HtmlBuilder::new(builder.width-2);
                        render_block(&mut sub_builder, child.clone(), err_out);
                        let li_text = sub_builder.into_string();
                        builder.add_subblock(&prepend_first_line(&li_text, "* "));
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

    let mut builder = HtmlBuilder::new(width);
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
                println!("[[{}]] vs [[{}]]", $a, $b);
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
        assert_eq!(from_read(&b"<p>Hello</p>"[..], 10),
                   "Hello\n");
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
}
