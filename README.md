# html2text

html2text is a [Rust](http://www.rust-lang.org/) crate which converts HTML to
plain text.

It makes use of the [Servo project](https://github.com/servo/servo)'s HTML
parser, [html5ever](https://github.com/servo/html5ever/), using the DOM to
generate text (which can optionally include annotations for some features such
as hyperlinks).

The project aims to do a reasonable job of rendering reasonable HTML in a
terminal or other places where HTML needs to be converted to text (for
example the text/plain fallback in HTML e-mails).

## Examples

```rust
use html2text::from_read;
let html = b"
       <ul>
         <li>Item one</li>
         <li>Item two</li>
         <li>Item three</li>
       </ul>";
assert_eq!(from_read(&html[..], 20),
           "\
* Item one
* Item two
* Item three
");
```
