use crate::config::Config;
use crate::render::text_renderer::PlainDecorator;
use crate::{config, Error};

#[cfg(feature = "css")]
use super::render::text_renderer::RichDecorator;
use super::render::text_renderer::{RichAnnotation, TaggedLine, TrivialDecorator};
use super::{from_read, from_read_with_decorator, parse, TextDecorator};

/// Like assert_eq!(), but prints out the results normally as well
macro_rules! assert_eq_str {
    ($a:expr, $b:expr) => {
        if $a != $b {
            println!("<<<\n{}===\n{}>>>", $a, $b);
            assert_eq!($a, $b);
        }
    };
}
#[track_caller]
fn test_html(input: &[u8], expected: &str, width: usize) {
    let output = from_read(input, width).unwrap();
    assert_eq_str!(output, expected);
}
#[track_caller]
fn test_html_conf<F>(input: &[u8], expected: &str, width: usize, conf: F)
where
    F: Fn(Config<PlainDecorator>) -> Config<PlainDecorator>,
{
    let result = conf(config::plain())
        .string_from_read(input, width)
        .unwrap();
    assert_eq_str!(result, expected);
}
#[track_caller]
fn test_html_conf_dec<D: TextDecorator, F>(
    decorator: D,
    input: &[u8],
    expected: &str,
    width: usize,
    conf: F,
) where
    F: Fn(Config<D>) -> Config<D>,
{
    let result = conf(config::with_decorator(decorator))
        .string_from_read(input, width)
        .unwrap();
    assert_eq_str!(result, expected);
}
#[track_caller]
fn test_html_maxwrap(input: &[u8], expected: &str, width: usize, wrap_width: usize) {
    test_html_conf(input, expected, width, |conf| {
        conf.max_wrap_width(wrap_width)
    })
}
#[cfg(feature = "css")]
fn test_html_css(input: &[u8], expected: &str, width: usize) {
    let result = config::plain()
        .use_doc_css()
        .string_from_read(input, width)
        .unwrap();
    assert_eq_str!(result, expected);
}
#[cfg(feature = "css")]
fn test_colour_map(annotations: &[RichAnnotation], s: &str) -> String {
    let mut tags = ("", "");
    let mut bgtags = ("", "");
    for ann in annotations {
        match ann {
            RichAnnotation::Colour(c) => match c {
                crate::Colour {
                    r: 0xff,
                    g: 0,
                    b: 0,
                } => {
                    tags = ("<R>", "</R>");
                }
                crate::Colour {
                    r: 0xff,
                    g: 0xff,
                    b: 0xff,
                } => {
                    tags = ("<W>", "</W>");
                }
                crate::Colour {
                    r: 0,
                    g: 0xff,
                    b: 0,
                } => {
                    tags = ("<G>", "</G>");
                }
                crate::Colour {
                    r: 0,
                    g: 0,
                    b: 0xff,
                } => {
                    tags = ("<B>", "</B>");
                }
                _ => {
                    tags = ("<?>", "</?>");
                }
            },
            RichAnnotation::BgColour(c) => match c {
                crate::Colour {
                    r: 0xff,
                    g: 0,
                    b: 0,
                } => {
                    bgtags = ("<r>", "</r>");
                }
                crate::Colour {
                    r: 0,
                    g: 0xff,
                    b: 0,
                } => {
                    bgtags = ("<g>", "</g>");
                }
                _ => {
                    bgtags = ("<.>", "</.>");
                }
            },
            _ => (),
        }
    }
    format!("{}{}{}{}{}", bgtags.0, tags.0, s, tags.1, bgtags.1)
}

#[cfg(feature = "css")]
#[track_caller]
fn test_html_coloured_conf<F>(input: &[u8], expected: &str, width: usize, conf: F)
where
    F: Fn(Config<RichDecorator>) -> Config<RichDecorator>,
{
    let result = conf(config::rich().use_doc_css())
        .coloured(input, width, test_colour_map)
        .unwrap();
    assert_eq_str!(result, expected);
}
#[cfg(feature = "css")]
#[track_caller]
fn test_html_coloured(input: &[u8], expected: &str, width: usize) {
    test_html_coloured_conf(input, expected, width, |c| c)
}
#[track_caller]
fn test_html_err_conf<F>(input: &[u8], expected: Error, width: usize, conf: F)
where
    F: Fn(Config<PlainDecorator>) -> Config<PlainDecorator>,
{
    let result = conf(config::plain()).string_from_read(input, width);
    match result {
        Err(e) => {
            assert_eq!(e, expected);
        }
        Ok(text) => {
            panic!("Expected error, got: [[{}]]", text);
        }
    }
}
fn test_html_err(input: &[u8], expected: Error, width: usize) {
    test_html_err_conf(input, expected, width, |c| c)
}

#[cfg(feature = "css")]
#[track_caller]
fn test_html_style(input: &[u8], style: &str, expected: &str, width: usize) {
    let result = config::plain()
        .add_css(style)
        .unwrap()
        .string_from_read(input, width)
        .unwrap();
    assert_eq_str!(result, expected);
}

#[track_caller]
fn test_html_decorator<D>(input: &[u8], expected: &str, width: usize, decorator: D)
where
    D: TextDecorator,
{
    let output = from_read_with_decorator(input, width, decorator).unwrap();
    assert_eq_str!(output, expected);
}

#[test]
fn test_table() {
    test_html(
        br##"
   <table>
     <tr>
       <td>1</td>
       <td>2</td>
       <td>3</td>
     </tr>
   </table>
"##,
        r#"─┬─┬─
1│2│3
─┴─┴─
"#,
        12,
    );
}

#[test]
fn test_table2() {
    test_html(
        br##"
   <table>
     <tr>
       <td>1</td>
       <td>2</td>
       <td>3</td>
     </tr>
     <tr>
       <td>4</td>
       <td>5</td>
       <td>6</td>
     </tr>
   </table>
"##,
        r#"─┬─┬─
1│2│3
─┼─┼─
4│5│6
─┴─┴─
"#,
        12,
    );
}

#[test]
fn test_thead() {
    test_html(
        br##"
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
"##,
        r#"────┬────┬────
Col1│Col2│Col3
────┼────┼────
1   │2   │3   
────┴────┴────
"#,
        15,
    );
}

#[test]
fn test_colspan() {
    test_html(
        br##"
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
"##,
        r#"─┬─┬─
1│2│3
─┴─┼─
12 │3
─┬─┴─
1│23 
─┴───
"#,
        12,
    );
}

#[test]
fn test_colspan_zero() {
    test_html(
        br##"
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
       <td colspan="0">23</td>
     </tr>
   </table>
"##,
        r#"─┬─┬─
1│2│3
─┴─┼─
12 │3
─┬─┴─
1│23 
─┴───
"#,
        12,
    );
}

#[test]
fn test_colspan_large() {
    test_html(
        br##"
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
       <td colspan="99">23</td>
     </tr>
   </table>
"##,
        // FIXME: The extra long line blow is not ideal
        r#"─┬─┬─
1│2│3
─┴─┼─
12 │3
─┬─┴─
1│23  
─┴────
"#,
        12,
    );
}

#[test]
fn test_colspan_larger() {
    test_html(
        br##"
   <table>
     <tr>
       <td colspan="50">1</td>
       <td colspan="50">2</td>
       <td colspan="50">3</td>
     </tr>
     <tr>
       <td colspan="100">12</td>
       <td colspan="50">3</td>
     </tr>
     <tr>
       <td colspan="50">1</td>
       <td colspan="100">23</td>
     </tr>
   </table>
"##,
        r#"─┬─┬─
1│2│3
─┴─┼─
12 │3
─┬─┴─
1│23 
─┴───
"#,
        12,
    );
}

#[test]
fn test_para() {
    test_html(&b"<p>Hello</p>"[..], "Hello\n", 10);
}

#[test]
fn test_para2() {
    test_html(&b"<p>Hello, world!</p>"[..], "Hello, world!\n", 20);
}

#[test]
fn test_blockquote() {
    test_html(
        &br#"<p>Hello</p>
    <blockquote>One, two, three</blockquote>
    <p>foo</p>
"#[..],
        r#"Hello

> One, two,
> three

foo
"#,
        12,
    );
}

#[test]
fn test_ul() {
    test_html(
        br#"
        <ul>
          <li>Item one</li>
          <li>Item two</li>
          <li>Item three</li>
        </ul>
     "#,
        r#"* Item one
* Item two
* Item
  three
"#,
        10,
    );
}

#[test]
fn test_ol1() {
    test_html(
        br#"
        <ol>
          <li>Item one</li>
          <li>Item two</li>
          <li>Item three</li>
        </ol>
     "#,
        r#"1. Item one
2. Item two
3. Item
   three
"#,
        11,
    );
}

#[test]
fn test_ol2() {
    test_html(
        br#"
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
     "#,
        r#"1.  Item one
2.  Item two
3.  Item three
4.  Item four
5.  Item five
6.  Item six
7.  Item seven
8.  Item eight
9.  Item nine
10. Item ten
"#,
        20,
    );
}

#[test]
fn test_ol_start() {
    test_html(
        br#"
        <ol start="3">
          <li>Item three</li>
          <li>Item four</li>
        </ol>
     "#,
        r#"3. Item three
4. Item four
"#,
        20,
    );
}

#[test]
fn test_ol_start_9() {
    test_html(
        br#"
        <ol start="9">
          <li>Item nine</li>
          <li>Item ten</li>
        </ol>
     "#,
        r#"9.  Item nine
10. Item ten
"#,
        20,
    );
}

#[test]
fn test_ol_start_neg() {
    test_html(
        br#"
        <ol start="-1">
          <li>Item minus one</li>
          <li>Item zero</li>
          <li>Item one</li>
        </ol>
     "#,
        r#"-1. Item minus one
0.  Item zero
1.  Item one
"#,
        20,
    );
}

#[test]
fn test_strip_nl() {
    test_html(
        br#"
        <p>
           One
           Two
           Three
        </p>
     "#,
        "One Two Three\n",
        40,
    );
}
#[test]
fn test_strip_nl2() {
    test_html(
        br#"
        <p>
           One
           <span>
               Two
           </span>
           Three
        </p>
     "#,
        "One Two Three\n",
        40,
    );
}
#[test]
fn test_strip_nl_tbl() {
    test_html(
        br#"
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
     "#,
        r"──────────────
One Two Three 
──────────────
",
        20,
    );
}
#[test]
fn test_unknown_element() {
    test_html(
        br#"
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
     "#,
        r"──────────────
One Two Three 
──────────────
",
        20,
    );
}
#[test]
fn test_strip_nl_tbl_p() {
    test_html(
        br#"
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
     "#,
        r"──────────────
One Two Three 
──────────────
",
        20,
    );
}
#[test]
fn test_pre() {
    test_html(
        br#"
       <pre>foo
bar
wib   asdf;
</pre>
<p>Hello</p>
     "#,
        r"foo
bar
wib   asdf;

Hello
",
        20,
    );
}
#[test]
fn test_link() {
    test_html(
        br#"
       <p>Hello, <a href="http://www.example.com/">world</a></p>"#,
        r"Hello, [world][1]

[1]: http://www.example.com/
",
        80,
    );
}
#[test]
fn test_link2() {
    test_html(
        br#"
       <p>Hello, <a href="http://www.example.com/">world</a>!</p>"#,
        r"Hello, [world][1]!

[1]: http://www.example.com/
",
        80,
    );
}

#[test]
fn test_link3() {
    test_html(
        br#"
       <p>Hello, <a href="http://www.example.com/">w</a>orld</p>"#,
        r"Hello, [w][1]orld

[1]: http://www.example.com/
",
        80,
    );
}

#[test]
fn test_link_wrap() {
    test_html(
        br#"
       <a href="http://www.example.com/">Hello</a>"#,
        r"[Hello][1]

[1]: http:
//www.exam
ple.com/
",
        10,
    );
}

#[test]
fn test_wrap() {
    test_html(
        br"<p>Hello, world.  Superlongwordreally</p>",
        r#"Hello,
world.
Superlon
gwordrea
lly
"#,
        8,
    );
}

#[test]
fn test_wrap2() {
    test_html(
        br"<p>Hello, world.  This is a long sentence with a
few words, which we want to be wrapped correctly.</p>",
        r#"Hello, world. This
is a long sentence
with a few words,
which we want to be
wrapped correctly.
"#,
        20,
    );
}

#[test]
fn test_wrap3() {
    test_html(
        br#"<p><a href="dest">http://example.org/blah/</a> one two three"#,
        r#"[http://example.org/blah/
][1] one two three

[1]: dest
"#,
        25,
    );
}

#[test]
fn test_wrap4() {
    test_html(
        br#"<table><tr><td colspan="2"><p>Hello, this should be wrapped.</p></table>"#,
        r#"──────────
Hello,    
this      
should be 
wrapped.  
──────────
"#,
        10,
    );
}

#[test]
fn test_wrap_max() {
    test_html_maxwrap(
        br#"
        <p>This is a bit of text to wrap<p>
        <ul>
          <li>This is a bit of text to wrap too</li>
          </li>
        </ul>"#,
        r#"This is a
bit of
text to
wrap


* This is a
  bit of
  text to
  wrap too
"#,
        20,
        10,
    )
}

#[test]
fn test_wrap_max2() {
    test_html_maxwrap(
        br#"
        <p>plain para at the full screen width</p>
        <ul>
          <li>bullet point uses same width so its margin is 2 chars further right

          <ul><li>nested bullets in turn move 2 chars right each time
             <ul><li>result: you never get text squashed too narrow</li></ul>
          </li></ul>
        </li></ul>"#,
        r#"plain para at the
full screen width

* bullet point uses
  same width so its
  margin is 2 chars
  further right
  
  * nested bullets in
    turn move 2 chars
    right each time
    
    * result: you never
      get text squashed
      too narrow
"#,
        80,
        17,
    );
}

#[test]
fn test_wrap_word_boundaries() {
    test_html(br#"Hello there boo"#, "Hello there boo\n", 20);
    test_html(br#"Hello there boo"#, "Hello there boo\n", 15);
    test_html(br#"Hello there boo"#, "Hello there\nboo\n", 14);
    test_html(br#"Hello there boo"#, "Hello there\nboo\n", 13);
    test_html(br#"Hello there boo"#, "Hello there\nboo\n", 12);
    test_html(br#"Hello there boo"#, "Hello there\nboo\n", 11);
    test_html(br#"Hello there boo"#, "Hello\nthere boo\n", 10);
    test_html(br#"Hello there boo"#, "Hello\nthere\nboo\n", 6);
    test_html(br#"Hello there boo"#, "Hello\nthere\nboo\n", 5);
    test_html(br#"Hello there boo"#, "Hell\no\nther\ne\nboo\n", 4);
    test_html(
        br#"Hello there boo"#,
        "H\ne\nl\nl\no\nt\nh\ne\nr\ne\nb\no\no\n",
        1,
    );
    test_html(br#"Hello <em>there</em> boo"#, "Hello *there* boo\n", 20);
    test_html(br#"Hello <em>there</em> boo"#, "Hello *there*\nboo\n", 15);
    test_html(br#"Hello <em>there</em> boo"#, "Hello *there*\nboo\n", 14);
    test_html(br#"Hello <em>there</em> boo"#, "Hello *there*\nboo\n", 13);
    test_html(br#"Hello <em>there</em> boo"#, "Hello\n*there* boo\n", 12);
    test_html(br#"Hello <em>there</em> boo"#, "Hello\n*there* boo\n", 11);
    test_html(br#"Hello <em>there</em> boo"#, "Hello\n*there*\nboo\n", 10);
    test_html(br#"Hello <em>there</em> boo"#, "Hello\n*there\n* boo\n", 6);
    test_html(br#"Hello <em>there</em> boo"#, "Hello\n*ther\ne*\nboo\n", 5);
    test_html(
        br#"Hello <em>there</em> boo"#,
        "Hell\no\n*the\nre*\nboo\n",
        4,
    );
    test_html(
        br#"Hello <em>there</em> boo"#,
        "H\ne\nl\nl\no\n*\nt\nh\ne\nr\ne\n*\nb\no\no\n",
        1,
    );
}

#[test]
fn test_div() {
    test_html(
        br"<p>Hello</p><div>Div</div>",
        r#"Hello

Div
"#,
        20,
    );
    test_html(
        br"<p>Hello</p><div>Div</div><div>Div2</div>",
        r#"Hello

Div
Div2
"#,
        20,
    );
}

#[test]
fn test_img_alt() {
    test_html(
        br"<p>Hello <img src='foo.jpg' alt='world'></p>",
        "Hello [world]\n",
        80,
    );
}

#[test]
fn test_br() {
    test_html(br"<p>Hello<br/>World</p>", "Hello\nWorld\n", 20);
}

#[test]
fn test_br2() {
    test_html(br"<p>Hello<br/><br/>World</p>", "Hello\n\nWorld\n", 20);
}

#[test]
fn test_br3() {
    test_html(br"<p>Hello<br/> <br/>World</p>", "Hello\n\nWorld\n", 20);
}

#[test]
fn test_subblock() {
    test_html(
        br#"<div>
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

[1]: https://example.com/
",
        80,
    );
}

#[test]
fn test_controlchar() {
    test_html("Foo\u{0080}Bar".as_bytes(), "FooBar\n", 80);
    test_html("Foo\u{0080}Bar".as_bytes(), "FooB\nar\n", 4);
    test_html("FooBa\u{0080}r".as_bytes(), "FooB\nar\n", 4);
}

#[test]
fn test_nested_table_1() {
    test_html(
        br##"
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
"##,
        r#"─┬─┬─┬─┬─┬─┬─┬─┬─
1│2│3│4│5│6│7│8│9
─┼─┼─┼─┼─┼─┼─┼─┼─
1│2│3│4│5│6│7│8│9
─┼─┼─┼─┼─┼─┼─┼─┼─
1│2│3│4│5│6│7│8│9
─┴─┴─┴─┴─┴─┴─┴─┴─
"#,
        21,
    );
}

#[test]
fn test_nested_table_2() {
    test_html(
        br##"
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
"##,
        r#"─┬─┬───────
1│a│one    
─┼─│two    
2│b│three  
 │ │four   
 │ │five   
─┴─┴───────
"#,
        11,
    );
}

#[test]
fn test_h1() {
    test_html(
        br##"
   <h1>Hi</h1>
   <p>foo</p>
"##,
        r#"# Hi

foo
"#,
        21,
    );
}

#[test]
fn test_h3() {
    test_html(
        br##"
   <h3>Hi</h3>
   <p>foo</p>
"##,
        r#"### Hi

foo
"#,
        21,
    );
}

// General test that spacing is preserved
#[test]
fn test_pre2() {
    test_html(
        br##"<pre>Hello  sp
world</pre>"##,
        r#"Hello  sp
world
"#,
        21,
    );
}

// Check that spans work correctly inside <pre>
#[test]
fn test_pre_span() {
    test_html(
        br##"
<pre>Hello <span>$</span>sp
<span>Hi</span> <span>$</span><span>foo</span>
<span>Hi</span> <span>foo</span><span>, </span><span>bar</span>
</pre>"##,
        r#"Hello $sp
Hi $foo
Hi foo, bar
"#,
        21,
    );
}

// Check tab behaviour
#[test]
fn test_pre_tab() {
    test_html(b"<pre>\tworld</pre>", "        world\n", 40);
    test_html(b"<pre>H\tworld</pre>", "H       world\n", 40);
    test_html(b"<pre>He\tworld</pre>", "He      world\n", 40);
    test_html(b"<pre>Hel\tworld</pre>", "Hel     world\n", 40);
    test_html(b"<pre>Hell\tworld</pre>", "Hell    world\n", 40);
    test_html(b"<pre>Hello\tworld</pre>", "Hello   world\n", 40);
    test_html(b"<pre>Helloo\tworld</pre>", "Helloo  world\n", 40);
    test_html(b"<pre>Hellooo\tworld</pre>", "Hellooo world\n", 40);
    test_html(b"<pre>Helloooo\tworld</pre>", "Helloooo        world\n", 40);
}

#[test]
fn test_em_strong() {
    test_html(
        br##"
   <p>Hi <em>em</em> <strong>strong</strong></p>
"##,
        r#"Hi *em* **strong**
"#,
        21,
    );
}

#[test]
#[ignore] // Not yet fixed!
fn test_nbsp_indent() {
    test_html(
        br##"
   <div>Top</div>
   <div>&nbsp;Indented</div>
   <div>&nbsp;&nbsp;Indented again</div>
"##,
        r#"Top
Indented
Indented again
"#,
        21,
    );
}

// Some of the tracing output can overflow the stack when tracing some values.
#[cfg(not(feature = "html_trace"))]
#[test]
fn test_deeply_nested() {
    use ::std::iter::repeat;
    let html = repeat("<foo>").take(1000).collect::<Vec<_>>().concat();
    test_html(html.as_bytes(), "", 10);
}

// Some of the tracing output can overflow the stack when tracing some values.
#[cfg(not(feature = "html_trace"))]
#[test]
fn test_deeply_nested_table() {
    use ::std::iter::repeat;
    let rpt = 1000;
    let html = repeat("<table><tr><td>hi</td><td>")
        .take(rpt)
        .collect::<Vec<_>>()
        .concat()
        + &repeat("</td></tr></table>")
            .take(rpt)
            .collect::<Vec<_>>()
            .concat();

    let result = repeat(
        r#"──────────
hi
//////////
"#,
    )
    .take(rpt - 3)
    .collect::<Vec<_>>()
    .concat()
        + r#"──┬────
hi│hi  
  │////
  │──  
  │hi  
  │──  
──┴────
"# + &"──────────\n".repeat(rpt - 3);
    test_html(html.as_bytes(), &result, 10);
}

#[test]
fn test_table_no_id() {
    let html = r#"<html><body><table>
        <tr>
            <td>hi, world</td>
        </tr>
    </table></body></html>"#;
    test_html(
        html.as_bytes(),
        r#"─────────
hi, world
─────────
"#,
        10,
    );
}

#[test]
fn test_table_cell_id() {
    let html = r#"<html><body><table>
        <tr>
            <td id="bodyCell">hi, world</td>
        </tr>
    </table></body></html>"#;
    test_html(
        html.as_bytes(),
        r#"─────────
hi, world
─────────
"#,
        10,
    );
}

#[test]
fn test_table_row_id() {
    let html = r#"<html><body><table>
        <tr id="bodyrow">
            <td>hi, world</td>
        </tr>
    </table></body></html>"#;
    test_html(
        html.as_bytes(),
        r#"─────────
hi, world
─────────
"#,
        10,
    );
}

#[test]
fn test_table_table_id() {
    let html = r#"<html><body><table id="bodytable">
        <tr>
            <td>hi, world</td>
        </tr>
    </table></body></html>"#;
    test_html(
        html.as_bytes(),
        r#"─────────
hi, world
─────────
"#,
        10,
    );
}

#[test]
fn test_table_tbody_id() {
    let html = r#"<html><body><table>
      <tbody id="tb">
        <tr>
            <td>hi, world</td>
        </tr>
      </tbody>
    </table></body></html>"#;
    test_html(
        html.as_bytes(),
        r#"─────────
hi, world
─────────
"#,
        10,
    );
}

#[test]
fn test_header_width() {
    //0 size
    test_html_err(
        br##"
        <h2>
            <table>
                        <h3>Anything</h3>
            </table>
        </h2>
"##,
        Error::TooNarrow,
        7,
    );
    //Underflow
    test_html_err(
        br##"
        <h2>
            <table>
                <h3>Anything</h3>
            </table>
        </h2>
"##,
        Error::TooNarrow,
        5,
    );
}

#[test]
fn test_trivial_decorator() {
    test_html_decorator(
        br#"<div>
     <div>Here's a <a href="https://example.com/">link</a>.</div>
     <div><ul>
     <li>Bullet</li>
     <li>Bullet</li>
     <li>Bullet</li>
     </ul></div>
     </div>"#,
        r"Here's a link.

Bullet
Bullet
Bullet
",
        80,
        TrivialDecorator::new(),
    );
}

#[test]
fn test_issue_16() {
    test_html(b"<ul><li><!----></li></ul>", "", 10);
}

#[test]
fn test_pre_br() {
    test_html(
        b"<pre>Foo<br>Bar</pre>",
        r#"Foo
Bar
"#,
        10,
    );
}

#[test]
fn test_pre_emptyline() {
    test_html(br#"<pre>X<span id="i"> </span></pre>"#, "X\n", 10);
}

#[test]
fn test_link_id_longline() {
    test_html(
        br#"<a href="foo" id="i">quitelongline</a>"#,
        r#"[quitelong
line][1]

[1]: foo
"#,
        10,
    );
}

#[test]
fn test_dl() {
    test_html(
        br#"<dl><dt>Foo</dt><dd>Definition of foo</dd></dl>"#,
        r#"*Foo*
  Definition of foo
"#,
        40,
    );
}

#[test]
fn test_s() {
    test_html(
        br#"Hi <s>you</s>thee!"#,
        "Hi y\u{336}o\u{336}u\u{336}thee!\n",
        40,
    );
}

#[test]
fn test_multi_parse() {
    let html: &[u8] = b"one two three four five six seven eight nine ten eleven twelve thirteen \
                        fourteen fifteen sixteen seventeen";
    let tree = parse(html).unwrap();
    assert_eq!(
        "one two three four five six seven eight nine ten eleven twelve thirteen fourteen\n\
         fifteen sixteen seventeen\n",
        config::plain().render_to_string(tree.clone(), 80).unwrap()
    );
    assert_eq!(
        "one two three four five six seven eight nine ten eleven twelve\n\
         thirteen fourteen fifteen sixteen seventeen\n",
        config::plain().render_to_string(tree.clone(), 70).unwrap()
    );
    assert_eq!(
        "one two three four five six seven eight nine ten\n\
         eleven twelve thirteen fourteen fifteen sixteen\n\
         seventeen\n",
        config::plain().render_to_string(tree.clone(), 50).unwrap()
    );
}

#[test]
fn test_read_rich() {
    let html: &[u8] = b"<strong>bold</strong>";
    let lines = config::rich()
        .render_to_lines(parse(html).unwrap(), 80)
        .unwrap();
    let tag = vec![RichAnnotation::Strong];
    let line = TaggedLine::from_string("*bold*".to_owned(), &tag);
    assert_eq!(vec![line], lines);
}

#[test]
fn test_read_rich_nodecorate() {
    let html: &[u8] = b"<strong>bold</strong>";
    let lines = config::rich_no_decorate()
        .render_to_lines(parse(html).unwrap(), 80)
        .unwrap();
    let tag = vec![RichAnnotation::Strong];
    let line = TaggedLine::from_string("bold".to_owned(), &tag);
    assert_eq!(vec![line], lines);
}

#[test]
fn test_read_custom() {
    let html: &[u8] = b"<strong>bold</strong>";
    let lines = config::with_decorator(TrivialDecorator::new())
        .render_to_lines(parse(html).unwrap(), 80)
        .unwrap();
    let tag = vec![()];
    let line = TaggedLine::from_string("bold".to_owned(), &tag);
    assert_eq!(vec![line], lines);
}

#[test]
fn test_pre_rich() {
    use RichAnnotation::*;
    assert_eq!(
        config::rich()
            .render_to_lines(parse(&b"<pre>test</pre>"[..]).unwrap(), 100)
            .unwrap(),
        [TaggedLine::from_string(
            "test".into(),
            &vec![Preformat(false)]
        )]
    );

    assert_eq!(
        config::rich()
            .render_to_lines(crate::parse("<pre>testlong</pre>".as_bytes()).unwrap(), 4)
            .unwrap(),
        [
            TaggedLine::from_string("test".into(), &vec![Preformat(false)]),
            TaggedLine::from_string("long".into(), &vec![Preformat(true)])
        ]
    );

    // The similar html with <p> and white-space: pre should not have the Preformat
    // tags.
    assert_eq!(
        config::rich()
            .render_to_lines(
                crate::parse(r#"<p style="white-space: pre">testlong</p>"#.as_bytes()).unwrap(),
                4
            )
            .unwrap(),
        [
            TaggedLine::from_string("test".into(), &vec![]),
            TaggedLine::from_string("long".into(), &vec![])
        ]
    );
}

#[test]
fn test_finalise() {
    use crate::render::text_renderer::{TaggedLine, TextDecorator};

    #[derive(Clone, Debug)]
    struct TestDecorator;

    impl TextDecorator for TestDecorator {
        type Annotation = bool;

        fn decorate_link_start(&mut self, _url: &str) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_link_end(&mut self) -> String {
            Default::default()
        }

        fn decorate_em_start(&self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_em_end(&self) -> String {
            Default::default()
        }

        fn decorate_strong_start(&self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_strong_end(&self) -> String {
            Default::default()
        }

        fn decorate_strikeout_start(&self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_strikeout_end(&self) -> String {
            Default::default()
        }

        fn decorate_code_start(&self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_code_end(&self) -> String {
            Default::default()
        }

        fn decorate_preformat_first(&self) -> Self::Annotation {
            Default::default()
        }

        fn decorate_preformat_cont(&self) -> Self::Annotation {
            Default::default()
        }

        fn decorate_image(&mut self, _src: &str, _title: &str) -> (String, Self::Annotation) {
            Default::default()
        }

        fn header_prefix(&self, level: usize) -> String {
            "#".repeat(level) + " "
        }

        fn quote_prefix(&self) -> String {
            "> ".to_string()
        }

        fn unordered_item_prefix(&self) -> String {
            "* ".to_string()
        }

        fn ordered_item_prefix(&self, i: i64) -> String {
            format!("{}. ", i)
        }

        fn finalise(&mut self, _links: Vec<String>) -> Vec<TaggedLine<bool>> {
            vec![TaggedLine::from_string(String::new(), &true)]
        }

        fn make_subblock_decorator(&self) -> Self {
            TestDecorator
        }
    }
    assert_eq!(
        config::with_decorator(TestDecorator)
            .lines_from_read("test".as_bytes(), 80)
            .unwrap(),
        vec![
            TaggedLine::from_string("test".to_owned(), &Vec::new()),
            TaggedLine::new(),
            TaggedLine::new(),
        ]
    );
}

#[test]
fn test_empty_rows() {
    test_html(
        br##"
   <table>
     <tr>
       <td>1</td>
       <td>2</td>
       <td>3</td>
     </tr>
     <tr><td></td><td></td><td></td></tr>
     <tr>
       <td>4</td>
       <td>5</td>
       <td>6</td>
     </tr>
   </table>
"##,
        r#"─┬─┬─
1│2│3
─┼─┼─
4│5│6
─┴─┴─
"#,
        12,
    );
}

#[test]
fn test_empty_cols() {
    test_html(
        br##"
   <table>
     <tr>
       <td></td>
       <td>1</td>
       <td></td>
       <td>2</td>
       <td></td>
     </tr>
     <tr>
       <td></td>
       <td>3</td>
       <td></td>
       <td>4</td>
       <td></td>
     </tr>
     <tr>
       <td></td>
       <td>5</td>
       <td></td>
       <td>6</td>
       <td></td>
     </tr>
   </table>
"##,
        r#"─┬─
1│2
─┼─
3│4
─┼─
5│6
─┴─
"#,
        12,
    );
}

#[test]
fn test_empty_table() {
    test_html(
        br##"
   <table></table>
"##,
        r#""#,
        12,
    );
}

#[test]
fn test_table_empty_single_row() {
    test_html(
        br##"
   <table><tr></tr></table>
"##,
        r#""#,
        12,
    );
}

#[test]
fn test_table_empty_single_row_empty_cell() {
    test_html(
        br##"
   <table><tr><td></td></tr></table>
"##,
        r#""#,
        12,
    );
}

#[test]
fn test_renderer_zero_width() {
    test_html_err(
        br##"<ul><li><table><tr><td>x</td></tr></table></li></ul>
"##,
        Error::TooNarrow,
        2,
    );
}

#[test]
fn test_ul_tiny_table() {
    test_html(
        br##"<ul><li><table><tr><td>x</td></tr></table></li></ul>
"##,
        r#"* ─
  x
  ─
"#,
        12,
    );
}

#[test]
fn test_issue_54_oob() {
    test_html(
        br##"
<html>
<body>
    <table>
        <tr>
            <td>
                <table>
                    <tr>
                        <td>&nbsp;</td>
                        <td>
                            <table>
                                <tr>
                                    <td>Blah blah blah
                                    </td>
                                </tr>
                            </table>
                        </td>
                        <td>&nbsp;</td>
                    </tr>
                </table>
            </td>
        </tr>
    </table>
</body>
"##,
        r#"─┬──────┬─
 │Blah  │ 
 │blah  │ 
 │blah  │ 
─┴──────┴─
"#,
        10,
    );
}

#[test]
fn test_table_vertical_rows() {
    test_html(
        br##"
<table>
    <tr>
        <td>wid</td>
        <td>kin</td>
        <td>der</td>
    </tr>
</table>
"##,
        "─────
wid
/////
kin
/////
der
─────
",
        5,
    );
}

const MULTILINE_CELLS: &[u8] = b"<table><tr>
    <td><ol><li></li></ol></td>
    <td><ol><li>
        Aliquam erat volutpat.  Nunc eleifend leo vitae magna.  In id erat non orci commodo lobortis.
    </li>
    <li>
        Aliquam erat volutpat.
    </li>
    <li></li>
    </ol></td>
    <td><ol><li>
        Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.
    </li></ol></td>
</tr>
</table>";

#[test]
fn test_table_without_borders() {
    let expected = "Aliquam erat volutpat. Nunc eleifend leo     Lorem ipsum dolor sit amet,       
vitae magna. In id erat non orci commodo     consectetuer adipiscing elit.     
lobortis.                                    Donec hendrerit tempor tellus.    
Aliquam erat volutpat.                                                         \n";
    test_html_conf_dec(
        TrivialDecorator::new(),
        MULTILINE_CELLS,
        expected,
        80,
        |c| c.no_table_borders(),
    );
}

#[test]
fn test_table_raw_mode() {
    let expected = "Aliquam erat volutpat. Nunc eleifend leo vitae magna. In id erat non orci
commodo lobortis.
Aliquam erat volutpat.
Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor
tellus.\n";
    test_html_conf_dec(
        TrivialDecorator::new(),
        MULTILINE_CELLS,
        expected,
        80,
        |c| c.raw_mode(true),
    );
}

#[test]
fn test_unicode() {
    test_html(
        "<table>
      <td>နတမစ</td>
      <td>နတမစ</td>
      <td>aaa</td>
</table>"
            .as_bytes(),
        "────┬────┬───
နတမစ│နတမစ│aaa
────┴────┴───
",
        15,
    );
}

#[test]
fn test_list_in_table() {
    test_html(
        b"<table>
<td><ol>
<li>0</li>
<li>1</li>
<li>2</li>
<li>3</li>
<li>4</li>
<li>5</li>
<li>6</li>
<li>7</li>
<li>8</li>
<li>9</li>
<li>10</li>
</ol></td>
</table>",
        "──────
1.  0 
2.  1 
3.  2 
4.  3 
5.  4 
6.  5 
7.  6 
8.  7 
9.  8 
10. 9 
11. 10
──────
",
        6,
    );
}

#[test]
fn test_max_width() {
    let html = r#"<table><td><p>3,266</p>"#;
    let decorator = crate::render::text_renderer::PlainDecorator::new();
    let text = from_read_with_decorator(html.as_bytes(), usize::MAX, decorator.clone()).unwrap();
    println!("{}", text);
}

#[test]
fn test_preserving_empty_newlines_in_pre_blocks() {
    let html = r#"<pre>
Test.


End.
</pre>"#;
    let decorator = crate::render::text_renderer::TrivialDecorator::new();
    let text = from_read_with_decorator(html.as_bytes(), 20, decorator.clone()).unwrap();
    assert_eq!(text, "Test.\n\n\nEnd.\n");
}

#[test]
fn test_links_outside_table() {
    let html = r#"
<table>
<tbody>
<tr><td><a href="https://example.com/verylonglinks"><img src="http://www.twitter.com/img/icon_twitter.png" alt="Twitter"></a></td>
    <td><a href="http://www.facebook.com/pages"><img src="http://www.facebook.com/icon_facebook.png" alt="Facebook"></a></td>
        </tr>
        </tbody>
        </table>
"#;
    let text = from_read(html.as_bytes(), 80).unwrap();
    assert_eq!(
        text,
        "──────────────┬───────────────
[[Twitter]][1]│[[Facebook]][2]
──────────────┴───────────────

[1]: https://example.com/verylonglinks
[2]: http://www.facebook.com/pages
"
    );
}

#[test]
fn test_narrow_width_nested() {
    use crate::Error;
    // Check different things which cause narrowing
    for html in [
        r#"<h1>Hi</h1>"#,
        r#"<blockquote>Hi</blockquote>"#,
        r#"<ul><li>Hi</li></ul>"#,
        r#"<ol><li>Hi</li></ul>"#,
        r#"<dl><dt>Foo</dt><dd>Definition of foo</dd></dl>"#,
    ] {
        let result = config::plain().string_from_read(html.as_bytes(), 1);
        if let Err(Error::TooNarrow) = result {
            // ok
        } else {
            panic!("Expected too narrow, got: {:?}", result);
        }
    }
}

#[test]
fn test_issue_93_x() {
    let data = [
        60, 116, 97, 98, 108, 101, 62, 60, 116, 114, 62, 60, 116, 100, 62, 120, 105, 60, 48, 62, 0,
        0, 0, 60, 116, 97, 98, 108, 101, 62, 58, 58, 58, 62, 58, 62, 62, 62, 58, 60, 112, 32, 32,
        32, 32, 32, 32, 32, 71, 87, 85, 78, 16, 16, 62, 60, 15, 16, 16, 16, 16, 16, 16, 15, 38, 16,
        16, 16, 15, 1, 16, 16, 16, 16, 16, 16, 162, 111, 107, 99, 91, 112, 57, 64, 94, 100, 60,
        111, 108, 47, 62, 127, 60, 108, 73, 62, 125, 109, 121, 102, 99, 122, 110, 102, 114, 98, 60,
        97, 32, 104, 114, 101, 102, 61, 98, 111, 103, 32, 105, 100, 61, 100, 62, 60, 111, 15, 15,
        15, 15, 15, 15, 15, 39, 15, 15, 15, 106, 102, 59, 99, 32, 32, 32, 86, 102, 122, 110, 104,
        93, 108, 71, 114, 117, 110, 100, 96, 121, 57, 60, 107, 116, 109, 247, 62, 60, 32, 60, 122,
        98, 99, 98, 97, 32, 119, 127, 127, 62, 60, 112, 62, 121, 116, 60, 47, 116, 100, 62, 62, 60,
        111, 98, 62, 123, 110, 109, 97, 101, 105, 119, 60, 112, 101, 101, 122, 102, 63, 120, 97,
        62, 60, 101, 62, 60, 120, 109, 112, 32, 28, 52, 55, 50, 50, 49, 52, 185, 150, 99, 62, 255,
        112, 76, 85, 60, 112, 62, 73, 100, 116, 116, 60, 75, 50, 73, 116, 120, 110, 127, 255, 118,
        32, 42, 40, 49, 33, 112, 32, 36, 107, 57, 60, 5, 163, 62, 49, 55, 32, 33, 118, 99, 63, 60,
        109, 107, 43, 119, 100, 62, 60, 104, 58, 101, 163, 163, 163, 163, 220, 220, 220, 220, 220,
        220, 220, 220, 220, 220, 220, 220, 1, 107, 117, 107, 108, 44, 102, 58, 60, 116, 101, 97,
        106, 98, 59, 60, 115, 109, 52, 58, 115, 98, 62, 232, 110, 114, 32, 60, 117, 93, 120, 112,
        119, 111, 59, 98, 120, 61, 206, 19, 61, 206, 19, 59, 1, 110, 102, 60, 115, 0, 242, 64, 203,
        8, 111, 50, 59, 121, 122, 32, 42, 35, 32, 37, 101, 120, 104, 121, 0, 242, 59, 63, 121, 231,
        130, 130, 130, 170, 170, 1, 32, 0, 0, 0, 28, 134, 200, 90, 119, 48, 60, 111, 108, 118, 119,
        116, 113, 59, 100, 60, 117, 43, 110, 99, 9, 216, 157, 137, 216, 157, 246, 167, 62, 60, 104,
        61, 43, 28, 134, 200, 105, 119, 48, 60, 122, 110, 0, 242, 61, 61, 114, 231, 130, 130, 130,
        170, 170, 170, 233, 222, 222, 162, 163, 163, 163, 163, 163, 163, 163, 85, 100, 116, 99, 61,
        60, 163, 163, 163, 163, 163, 220, 220, 1, 109, 112, 105, 10, 59, 105, 220, 215, 10, 59,
        122, 100, 100, 121, 97, 43, 43, 43, 102, 122, 100, 60, 62, 114, 116, 122, 115, 61, 60, 115,
        101, 62, 215, 215, 215, 215, 215, 98, 59, 60, 109, 120, 57, 60, 97, 102, 113, 229, 43, 43,
        43, 43, 43, 43, 43, 43, 43, 35, 43, 43, 101, 58, 60, 116, 98, 101, 107, 98, 43, 43, 43, 43,
        43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
        43, 43, 43, 43, 43, 98, 99, 62, 60, 112, 102, 59, 124, 107, 111, 97, 98, 108, 118, 60, 116,
        102, 101, 104, 97, 62, 60, 255, 127, 46, 60, 116, 101, 62, 60, 105, 102, 63, 116, 116, 60,
        47, 116, 101, 62, 62, 60, 115, 98, 62, 123, 109, 108, 97, 100, 119, 118, 60, 111, 99, 97,
        103, 99, 62, 60, 255, 127, 46, 60, 103, 99, 62, 60, 116, 98, 63, 60, 101, 62, 60, 109, 109,
        231, 130, 130, 130, 213, 213, 213, 233, 222, 222, 59, 101, 103, 58, 60, 100, 111, 61, 65,
        114, 104, 60, 47, 101, 109, 62, 60, 99, 99, 172, 97, 97, 58, 60, 119, 99, 64, 126, 118,
        104, 100, 100, 107, 105, 60, 120, 98, 255, 255, 255, 0, 60, 255, 127, 46, 60, 113, 127,
    ];
    config::with_decorator(TrivialDecorator::new())
        .string_from_read(&data[..], 1)
        .unwrap_err();
}

#[test]
fn test_superscript() {
    test_html(br#"Exponential x<sup>y</sup>"#, "Exponential x^{y}\n", 80);
    test_html(br#"Exponential 2<sup>32</sup>"#, "Exponential 2³²\n", 80);
}

#[test]
fn test_header_overflow() {
    let html_hdr = br#"<blockquote><h3>Foo</h3></blockquote>"#;
    test_html(html_hdr, "> ### Foo\n", 20);
    test_html_conf(html_hdr, "> ### F\n> ### o\n> ### o\n", 7, |c| {
        c.min_wrap_width(1)
    });
    test_html_err_conf(html_hdr, Error::TooNarrow, 6, |c| c.min_wrap_width(1));
    test_html_err_conf(html_hdr, Error::TooNarrow, 7, |c| c.min_wrap_width(3));
    test_html_conf(html_hdr, "> ### F\n> ### o\n> ### o\n", 6, |c| {
        c.min_wrap_width(1).allow_width_overflow()
    });
    test_html_conf(html_hdr, "> ### Foo\n", 7, |c| {
        c.min_wrap_width(3).allow_width_overflow()
    });
}

#[test]
fn test_blockquote_overflow() {
    let html_hdr = br#"<blockquote><blockquote>Foo</blockquote></blockquote>"#;
    test_html(html_hdr, "> > Foo\n", 20);
    test_html_conf(html_hdr, "> > F\n> > o\n> > o\n", 5, |c| {
        c.min_wrap_width(1)
    });
    test_html_err_conf(html_hdr, Error::TooNarrow, 3, |c| c.min_wrap_width(1));
    test_html_err_conf(html_hdr, Error::TooNarrow, 4, |c| c.min_wrap_width(3));
    test_html_conf(html_hdr, "> > F\n> > o\n> > o\n", 3, |c| {
        c.min_wrap_width(1).allow_width_overflow()
    });
    test_html_conf(html_hdr, "> > Foo\n", 4, |c| {
        c.min_wrap_width(3).allow_width_overflow()
    });
}

#[test]
fn test_ul_overflow() {
    let html_hdr = br#"<ul><li><ul><li>Foo</li></ul></li></ul>"#;
    test_html(html_hdr, "* * Foo\n", 20);
    test_html_conf(html_hdr, "* * F\n    o\n    o\n", 5, |c| {
        c.min_wrap_width(1)
    });
    test_html_err_conf(html_hdr, Error::TooNarrow, 3, |c| c.min_wrap_width(1));
    test_html_err_conf(html_hdr, Error::TooNarrow, 4, |c| c.min_wrap_width(3));
    test_html_conf(html_hdr, "* * F\n    o\n    o\n", 3, |c| {
        c.min_wrap_width(1).allow_width_overflow()
    });
    test_html_conf(html_hdr, "* * Foo\n", 4, |c| {
        c.min_wrap_width(3).allow_width_overflow()
    });
}

#[test]
fn test_ol_overflow() {
    let html_hdr = br#"<ol><li><ol><li>Foo</li></ol></li></ol>"#;
    test_html(html_hdr, "1. 1. Foo\n", 20);
    test_html_conf(html_hdr, "1. 1. F\n      o\n      o\n", 7, |c| {
        c.min_wrap_width(1)
    });
    test_html_err_conf(html_hdr, Error::TooNarrow, 5, |c| c.min_wrap_width(1));
    test_html_err_conf(html_hdr, Error::TooNarrow, 6, |c| c.min_wrap_width(3));
    test_html_conf(html_hdr, "1. 1. F\n      o\n      o\n", 5, |c| {
        c.min_wrap_width(1).allow_width_overflow()
    });
    test_html_conf(html_hdr, "1. 1. Foo\n", 6, |c| {
        c.min_wrap_width(3).allow_width_overflow()
    });
}

#[test]
fn test_dd_overflow() {
    let html_hdr = br#"<blockquote><dl><dt>Foo</dt><dd>Hello</dd></dl></blockquote>"#;
    test_html(html_hdr, "> *Foo*\n>   Hello\n", 20);
    test_html_conf(
        html_hdr,
        "> *Fo\n> o*\n>   H\n>   e\n>   l\n>   l\n>   o\n",
        5,
        |c| c.min_wrap_width(1),
    );
    test_html_err_conf(html_hdr, Error::TooNarrow, 3, |c| c.min_wrap_width(1));
    test_html_err_conf(html_hdr, Error::TooNarrow, 4, |c| c.min_wrap_width(3));
    test_html_conf(html_hdr, "> *Foo*\n>   Hel\n>   lo\n", 4, |c| {
        c.min_wrap_width(3).allow_width_overflow()
    });
}

#[test]
fn test_overflow_wide_char() {
    // The smiley is a width-2 character.
    let html = "😃".as_bytes();
    test_html_err_conf(html, Error::TooNarrow, 1, |c| c.min_wrap_width(1));
    test_html_conf(html, "😃\n", 1, |c| {
        c.min_wrap_width(1).allow_width_overflow()
    });
}

#[test]
fn test_table_too_narrow() {
    let tbl = "<table><tr>
    <td><ol><li></li></ol></td>
    <td>
        <ol><li>Aliquam erat volutpat. Lorem ipsum dolor sit amet,</li></ol>
    </td>
    <td>
        <ol><li>Lorem ipsum dolor sit</li></ol>
    </td>
</tr></table>"
        .as_bytes();
    from_read(tbl, 80).unwrap();
}

#[test]
fn test_empty_table_in_list() {
    test_html(
        b"
<ul>
  <table>
    <tr></tr>
  </table>
</ul>",
        "",
        80,
    );
}

#[test]
fn test_silly_colspan() {
    test_html(
        br#"
  <table>
    <tr>
      <td colspan="9007199254740991">foo</td.
    </tr>
  </table>
"#,
        r#"───
foo
───
"#,
        80,
    );
}

#[test]
fn test_issue_187() {
    let html = br#"<div><table><tbody><tr><td><div><table><tbody><tr><td><div><pre>na na na na na na na na na na na na na na na</p></div></td></tr>/<tbody></table></div></td></tr>/<tbody></table></div>"#;
    let _ = crate::config::plain().string_from_read(&html[..], 17);
}

#[cfg(feature = "css")]
mod css_tests {
    use super::{
        test_html_coloured, test_html_coloured_conf, test_html_conf, test_html_css, test_html_style,
    };

    #[test]
    fn test_disp_none() {
        test_html_css(
            br#"
          <style>
              .hide { display: none; }
          </style>
        <p>Hello</p>
        <p class="hide">Ignore</p>
        <p>There</p>"#,
            r#"Hello

There
"#,
            20,
        );

        // Same as above, but style supplied separately.
        test_html_style(
            br#"
        <p>Hello</p>
        <p class="hide">Ignore</p>
        <p>There</p>"#,
            " .hide { display: none; }",
            r#"Hello

There
"#,
            20,
        );
    }

    #[test]
    fn test_selector_elementname() {
        test_html_css(
            br#"
          <style>
              div { display: none; }
          </style>
        <p>Hello</p>
        <div>Ignore</div>
        <p>There</p>"#,
            r#"Hello

There
"#,
            20,
        );
    }

    #[test]
    fn test_selector_aoc() {
        test_html_css(
            br#"
          <style>
              .someclass > * > span > span {
                  display: none;
              }
          </style>
        <p>Hello</p>
        <div class="someclass">Ok
        <p>
         <span>Span1<span>Span2</span></span>
        </p>
        <div>
         <span>Span1<span>Span2</span></span>
        </div>
        </div>
        <p>There</p>"#,
            r#"Hello

Ok

Span1

Span1

There
"#,
            20,
        );
    }

    #[test]
    fn test_coloured_a() {
        test_html_coloured(
            br##"
          <style>
              .red {
                  color:#FF0000;
              }
          </style>
        <p>Test <a class="red" href="foo">red</a></p>
        "##,
            r#"Test <R>red</R>
"#,
            20,
        );
    }

    #[test]
    fn test_bgcoloured() {
        test_html_coloured(
            br##"
          <style>
              .red {
                  color:#FF0000;
                  background-color:#00FF00;
              }
          </style>
        <p>Test <span class="red">red</span></p>
        "##,
            r#"Test <g><R>red</R></g>
"#,
            20,
        );
    }

    #[test]
    fn test_bgcoloured2() {
        test_html_coloured(
            br##"
          <style>
              .red {
                  color:#FF0000;
                  background-color:#00FF00;
              }
          </style>
        <p>Test <span class="red">red</span> and <span style="color: #00ff00">green</span></p>
        "##,
            r#"Test <g><R>red</R></g> and <G>green</G>
"#,
            20,
        );
    }

    #[test]
    fn test_bgcoloured3() {
        test_html_coloured(
            br##"
          <style>
              .but {
                  background-color:#00FF00;
              }
          </style>
        <p>Test <span class="but">Two words</span> bg</p>
        "##,
            r#"Test <g>Two words</g> bg
"#,
            20,
        );
    }

    #[test]
    fn test_coloured_element() {
        test_html_coloured(
            br##"
          <style>
              .red {
                  color:#FF0000;
              }
          </style>
        <p>Test <blah class="red" href="foo">red</blah></p>
        "##,
            r#"Test <R>red</R>
"#,
            20,
        );
    }

    #[test]
    fn test_color_attr() {
        test_html_coloured(
            br##"
        <p>Test <font color="red">red</font></p>
        "##,
            r#"Test <R>red</R>
"#,
            20,
        );
    }

    #[test]
    fn test_css_lists() {
        test_html_coloured(
            br##"
          <style>
              .red {
                  color:#FF0000;
              }
          </style>
        <ul>
          <li class="red">Line one</li>
          <li>Line <span class="red">two</span></li>
        </ul>
        "##,
            r#"* <R>Line one</R>
* Line <R>two</R>
"#,
            20,
        );
        test_html_coloured(
            br##"
          <style>
              .red {
                  color:#FF0000;
              }
          </style>
        <ol>
          <li class="red">Line one</li>
          <li>Line <span class="red">two</span></li>
        </ul>
        "##,
            r#"1. <R>Line one</R>
2. Line <R>two</R>
"#,
            20,
        );
    }

    #[test]
    fn test_coloured_multi() {
        use super::test_colour_map;
        let config = crate::config::rich().use_doc_css();
        let dom = config
            .parse_html(
                &br##"
          <style>
              .red {
                  color:#FF0000;
              }
          </style>
        <p>Test paragraph with <span class="red">red</span> text</p>
        "##[..],
            )
            .unwrap();
        let rt = config.dom_to_render_tree(&dom).unwrap();
        assert_eq!(
            config
                .render_coloured(rt.clone(), 10, test_colour_map)
                .unwrap(),
            r#"Test
paragraph
with <R>red</R>
text
"#
        );
        assert_eq!(
            config
                .render_coloured(rt.clone(), 100, test_colour_map)
                .unwrap(),
            r#"Test paragraph with <R>red</R> text
"#
        );
    }

    #[test]
    fn test_coloured_important() {
        use super::test_colour_map;
        let config = crate::config::rich().use_doc_css();
        let dom = config
            .parse_html(
                &br##"
          <style>
              .red {
                  color:#FF0000 !important;
              }
          </style>
        <p>Test paragraph with <span class="red">red</span> text</p>
        "##[..],
            )
            .unwrap();
        let rt = config.dom_to_render_tree(&dom).unwrap();
        assert_eq!(
            config
                .render_coloured(rt.clone(), 10, test_colour_map)
                .unwrap(),
            r#"Test
paragraph
with <R>red</R>
text
"#
        );
        assert_eq!(
            config
                .render_coloured(rt.clone(), 100, test_colour_map)
                .unwrap(),
            r#"Test paragraph with <R>red</R> text
"#
        );
    }

    #[test]
    fn test_wrap_word_boundaries() {
        let html = br#"<head><style>em { color: white; }</style></head>
            <body>
                Hello *<em>there</em>* boo"#;
        test_html_coloured(html, "Hello *<W>there</W>* boo\n", 20);
        test_html_coloured(html, "Hello *<W>there</W>*\nboo\n", 15);
        test_html_coloured(html, "Hello *<W>there</W>*\nboo\n", 14);
        test_html_coloured(html, "Hello *<W>there</W>*\nboo\n", 13);
        test_html_coloured(html, "Hello\n*<W>there</W>* boo\n", 12);
        test_html_coloured(html, "Hello\n*<W>there</W>* boo\n", 11);
        test_html_coloured(html, "Hello\n*<W>there</W>*\nboo\n", 10);
        test_html_coloured(html, "Hello\n*<W>there</W>*\nboo\n", 7);
        test_html_coloured(html, "Hello\n*<W>there</W>\n* boo\n", 6);
        test_html_coloured(html, "Hello\n*<W>ther</W>\n<W>e</W>*\nboo\n", 5);
        test_html_coloured(html, "Hell\no\n*<W>the</W>\n<W>re</W>*\nboo\n", 4);
        test_html_coloured(
            html,
            "H\ne\nl\nl\no\n*\n<W>t</W>\n<W>h</W>\n<W>e</W>\n<W>r</W>\n<W>e</W>\n*\nb\no\no\n",
            1,
        );
    }

    #[test]
    fn test_max_height_0() {
        test_html_css(
            br#"
        <div style="max-height: 0; overflow-y: hidden">This should be hidden</div>
        <p>Hello</p>"#,
            r#"Hello
"#,
            20,
        );
    }

    #[test]
    fn test_height_0() {
        test_html_css(
            br#"
        <div style="height: 0; overflow: hidden">This should be hidden</div>
        <p>Hello</p>"#,
            r#"Hello
"#,
            20,
        );
    }

    #[test]
    fn test_selector_hash() {
        test_html_coloured(
            br#"<head><style>
        #foo {
            color: #f00;
        }
        p#bar {
            color: #0f0;
        }
        div#baz {
            color: #00f;
        }
        *#qux {
            color: #fff;
        }
        </style></head><body>

        <p id="foo">Foo</p>
        <p id="bar">Bar</p>
        <p id="baz">Baz</p>
        <p id="qux">Qux</p>
        "#,
            r#"<R>Foo</R>

<G>Bar</G>

Baz

<W>Qux</W>
"#,
            20,
        );
    }

    #[test]
    fn test_selector_child_desc() {
        test_html_coloured(
            br#"<head><style>
        p.d span { /* descendent */
            color: #f00;
        }
        p.c > span { /* child */
            color: #0f0;
        }
        </style></head><body>

        <p class="d">X<span>C</span><dummy><span>D</span></dummy>Y</p>
        <p class="c"><span>C</span><dummy><span>D</span></dummy></p>
        "#,
            r#"X<R>CD</R>Y

<G>C</G>D
"#,
            20,
        );
    }

    #[test]
    fn test_colour_row() {
        test_html_coloured(
            br#"<head><style>
        tr.r {
            color: #f00;
        }
        </style></head><body>
        <table>
         <tr>
           <td>Row</td><td>One</td>
         </tr>
         <tr class="r">
           <td>Row</td><td>Two</td>
         </tr>
         <tr>
           <td>Row</td><td>Three</td>
         </tr>
        </table>
        "#,
            r#"───┬─────
Row│One  
───┼─────
<R>Row│Two  </R>
<R>───┼─────</R>
Row│Three
───┴─────
"#,
            20,
        );
    }

    #[test]
    fn test_css_levels() {
        test_html_coloured_conf(
            br#"
        <head><style>
            .doc_red { color: #f00; }
            .doc_red_imp { color: #f00 !important; }
        </style></head>
        <body>
            <p class="doc_red">Doc red</p>
            <p class="agent_green">Agent green</p>
            <p class="user_blue">User blue</p>
            <p class="doc_red agent_green">Doc vs agent</p>
            <p class="agent_green user_blue">Agent vs user</p>
            <p class="user_blue doc_red">User vs doc</p>
            <p class="doc_red agent_green_imp">Doc vs agent!</p>
            <p class="agent_green_imp user_blue">Agent! vs user</p>
            <p class="user_blue_imp doc_red">User! vs doc</p>
            <p class="doc_red_imp agent_green_imp">Doc! vs agent!</p>
            <p class="agent_green_imp user_blue_imp">Agent! vs user!</p>
            <p class="user_blue_imp doc_red_imp">User! vs doc!</p>
        </body>"#,
            r#"<R>Doc red</R>

<G>Agent green</G>

<B>User blue</B>

<R>Doc vs agent</R>

<B>Agent vs user</B>

<R>User vs doc</R>

<G>Doc vs agent!</G>

<G>Agent! vs user</G>

<B>User! vs doc</B>

<G>Doc! vs agent!</G>

<G>Agent! vs user!</G>

<B>User! vs doc!</B>
"#,
            80,
            |conf| {
                conf.add_agent_css(
                    r#"
                .agent_green { color: #0f0; }
                .agent_green_imp { color: #0f0 !important; }
                "#,
                )
                .unwrap()
                .add_css(
                    r#"
                .user_blue { color: #00f; }
                .user_blue_imp { color: #00f !important; }
                "#,
                )
                .unwrap()
            },
        );
    }

    #[test]
    fn test_pre_wrap() {
        test_html_conf(
            br#"<p class="prewrap">Hi
 a
  b
   x  longword
c  d  e
</p>"#,
            r#"Hi
 a
  b
   x
longword
c  d  e
"#,
            10,
            |conf| {
                conf.add_css(r#".prewrap { white-space: pre-wrap; }"#)
                    .unwrap()
            },
        );

        test_html_conf(
            br#"<p class="prewrap">Test wrapping of some normal text in pre-wrap mode</p>"#,
            r#"Test wrapping of
some normal text
in pre-wrap mode
"#,
            17,
            |conf| {
                conf.add_css(r#".prewrap { white-space: pre-wrap; }"#)
                    .unwrap()
            },
        );

        test_html_conf(
            br#"<p class="prewrap">This  para  has  double  spacing  which  should  survive  except  at  line  breaks</p>"#,
            r#"This  para  has  double  spacing
which  should  survive  except
at  line  breaks
"#,
            33,
            |conf| {
                conf.add_css(r#".prewrap { white-space: pre-wrap; }"#)
                    .unwrap()
            },
        );
    }

    #[test]
    fn test_nth_child() {
        test_html_coloured(
            br#"
          <style>
              li:nth-child(even) {
                  color: #f00;
              }
          </style>
          <body><ul>
              <li>One</li>
              <li>Two</li>
              <li>Three</li>
              <li>Four</li>
              <li>Five</li>
          </ul>"#,
            r#"* One
* <R>Two</R>
* Three
* <R>Four</R>
* Five
"#,
            20,
        );
        test_html_coloured(
            br#"
          <style>
              li:nth-child(odd) {
                  color: #f00;
              }
          </style>
          <body><ul>
              <li>One</li>
              <li>Two</li>
              <li>Three</li>
              <li>Four</li>
              <li>Five</li>
          </ul>"#,
            r#"* <R>One</R>
* Two
* <R>Three</R>
* Four
* <R>Five</R>
"#,
            20,
        );
        test_html_coloured(
            br#"
          <style>
              li:nth-child(-n+3) {
                  color: #f00;
              }
          </style>
          <body><ul>
              <li>One</li>
              <li>Two</li>
              <li>Three</li>
              <li>Four</li>
              <li>Five</li>
          </ul>"#,
            r#"* <R>One</R>
* <R>Two</R>
* <R>Three</R>
* Four
* Five
"#,
            20,
        );
        test_html_coloured(
            br#"
          <style>
              li:nth-child(2) {
                  color: #f00;
              }
          </style>
          <body><ul>
              <li>One</li>
              <li>Two</li>
              <li>Three</li>
              <li>Four</li>
              <li>Five</li>
          </ul>"#,
            r#"* One
* <R>Two</R>
* Three
* Four
* Five
"#,
            20,
        );
        test_html_coloured(
            br#"
          <style>
              li:nth-child(n+3):nth-child(-n+5) {
                  color: #f00;
              }
          </style>
          <body><ul>
              <li>One</li>
              <li>Two</li>
              <li>Three</li>
              <li>Four</li>
              <li>Five</li>
              <li>Six</li>
              <li>Seven</li>
              <li>Eight</li>
              <li>Nine</li>
              <li>Ten</li>
          </ul>"#,
            r#"* One
* Two
* <R>Three</R>
* <R>Four</R>
* <R>Five</R>
* Six
* Seven
* Eight
* Nine
* Ten
"#,
            20,
        );
    }
}
