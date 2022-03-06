[![jugglerchris](https://circleci.com/gh/jugglerchris/rust-html2text.svg?branch=master&style=svg)](https://app.circleci.com/pipelines/github/jugglerchris/rust-html2text?filter=all)

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

A couple of simple demonstration programs are included as examples:

### html2text

The simplest example uses `from_read` to convert HTML on stdin into plain
text:

```sh
$ cargo run --example html2text < foo.html
[...]
```

### html2term

A very simple example of using the rich interface (`from_read_rich`) for a
slightly interactive console HTML viewer is provided as `html2term`.

```sh
$ cargo run --example html2term foo.html
[...]
```

Note that this example takes the HTML file as a parameter so that it can
read keys from stdin.
