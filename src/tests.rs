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
fn test_html(input: &[u8], expected: &str, width: usize) {
    assert_eq_str!(from_read(input, width), expected);
}

fn test_html_decorator<D>(input: &[u8], expected: &str, width: usize, decorator: D)
where
    D: TextDecorator,
{
    let output = from_read_with_decorator(input, width, decorator);
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
        r#"────────────
1
////////////
2
////////////
3
────────────
12
////////////
3
────────────
1
////////////
23
────────────
"#,
        12,
    );
}

#[test]
fn test_para() {
    assert_eq_str!(from_read(&b"<p>Hello</p>"[..], 10), "Hello\n");
}

#[test]
fn test_para2() {
    assert_eq_str!(
        from_read(&b"<p>Hello, world!</p>"[..], 20),
        "Hello, world!\n"
    );
}

#[test]
fn test_blockquote() {
    assert_eq_str!(
        from_read(
            &br#"<p>Hello</p>
    <blockquote>One, two, three</blockquote>
    <p>foo</p>
"#[..],
            12
        ),
        r#"Hello

> One, two,
> three

foo
"#
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

#[test]
fn test_deeply_nested() {
    use ::std::iter::repeat;
    let html = repeat("<foo>").take(1000).collect::<Vec<_>>().concat();
    test_html(html.as_bytes(), "", 10);
}

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
        + &r#"──┬────
hi│hi  
  │////
  │──  
  │hi  
  │──  
──┴────
"# + &repeat("──────────\n").take(rpt - 3).collect::<String>();
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
    test_html(
        br##"
        <h2>
            <table>
                        <h3>Anything</h3>
            </table>
        </h2>
"##,
        r#"## ### A
## ### n
## ### y
## ### t
## ### h
## ### i
## ### n
## ### g
## 
## 
"#,
        7,
    );
    //Underflow
    test_html(
        br##"
        <h2>
            <table>
                <h3>Anything</h3>
            </table>
        </h2>
"##,
        r#"## ### A
## ### n
## ### y
## ### t
## ### h
## ### i
## ### n
## ### g
## 
## 
"#,
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
    test_html(br#"<pre>X<span id="i"> </span></pre>"#, "X  \n", 10);
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
    let tree = parse(html);
    assert_eq!(
        "one two three four five six seven eight nine ten eleven twelve thirteen fourteen\n\
         fifteen sixteen seventeen\n",
        tree.clone().render_plain(80).into_string()
    );
    assert_eq!(
        "one two three four five six seven eight nine ten eleven twelve\n\
         thirteen fourteen fifteen sixteen seventeen\n",
        tree.clone().render_plain(70).into_string()
    );
    assert_eq!(
        "one two three four five six seven eight nine ten\n\
         eleven twelve thirteen fourteen fifteen sixteen\n\
         seventeen\n",
        tree.clone().render_plain(50).into_string()
    );
}

#[test]
fn test_read_rich() {
    let html: &[u8] = b"<strong>bold</strong>";
    let lines = parse(html).render_rich(80).into_lines();
    let tag = vec![RichAnnotation::Strong];
    let line = TaggedLine::from_string("*bold*".to_owned(), &tag);
    assert_eq!(vec![line], lines);
}

#[test]
fn test_read_custom() {
    let html: &[u8] = b"<strong>bold</strong>";
    let lines = parse(html).render(80, TrivialDecorator::new()).into_lines();
    let tag = vec![()];
    let line = TaggedLine::from_string("bold".to_owned(), &tag);
    assert_eq!(vec![line], lines);
}

#[test]
fn test_pre_rich() {
    use RichAnnotation::*;
    assert_eq!(
        crate::parse("<pre>test</pre>".as_bytes())
            .render_rich(100)
            .into_lines(),
        [TaggedLine::from_string(
            "test".into(),
            &vec![Preformat(false)]
        )]
    );

    assert_eq!(
        crate::parse("<pre>testlong</pre>".as_bytes())
            .render_rich(4)
            .into_lines(),
        [
            TaggedLine::from_string("test".into(), &vec![Preformat(false)]),
            TaggedLine::from_string("long".into(), &vec![Preformat(true)])
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

        fn decorate_em_start(&mut self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_em_end(&mut self) -> String {
            Default::default()
        }

        fn decorate_strong_start(&mut self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_strong_end(&mut self) -> String {
            Default::default()
        }

        fn decorate_strikeout_start(&mut self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_strikeout_end(&mut self) -> String {
            Default::default()
        }

        fn decorate_code_start(&mut self) -> (String, Self::Annotation) {
            Default::default()
        }

        fn decorate_code_end(&mut self) -> String {
            Default::default()
        }

        fn decorate_preformat_first(&mut self) -> Self::Annotation {
            Default::default()
        }

        fn decorate_preformat_cont(&mut self) -> Self::Annotation {
            Default::default()
        }

        fn decorate_image(&mut self, _src: &str, _title: &str) -> (String, Self::Annotation) {
            Default::default()
        }

        fn header_prefix(&mut self, level: usize) -> String {
            "#".repeat(level) + " "
        }

        fn quote_prefix(&mut self) -> String {
            "> ".to_string()
        }

        fn unordered_item_prefix(&mut self) -> String {
            "* ".to_string()
        }

        fn ordered_item_prefix(&mut self, i: i64) -> String {
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
        crate::parse("test".as_bytes())
            .render(80, TestDecorator)
            .into_lines(),
        vec![
            TaggedLine::from_string("test".to_owned(), &Vec::new()),
            TaggedLine::new(),
            TaggedLine::from_string("".to_owned(), &vec![true]),
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
        r#"
"#,
        12,
    );
}

#[test]
fn test_table_empty_single_row() {
    test_html(
        br##"
   <table><tr></tr></table>
"##,
        r#"
"#,
        12,
    );
}

#[test]
fn test_table_empty_single_row_empty_cell() {
    test_html(
        br##"
   <table><tr><td></td></tr></table>
"##,
        r#"
"#,
        12,
    );
}

#[test]
fn test_renderer_zero_width() {
    test_html(
        br##"<ul><li><table><tr><td>x</td></tr></table></li></ul>
"##,
// Unfortunately the "x" ends up not being rendered as it doesn't fit.
        r#"* 
  
"#,
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
    let text = from_read_with_decorator(html.as_bytes(), usize::MAX, decorator.clone());
    println!("{}", text);
}

#[test]
fn test_preserving_empty_newlines_in_pre_blocks() {
    let html = r#"<pre>
Test.


End.
</pre>"#;
    let decorator = crate::render::text_renderer::TrivialDecorator::new();
    let text = from_read_with_decorator(html.as_bytes(), usize::MAX, decorator.clone());
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
    let text = from_read(html.as_bytes(), 80);
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
