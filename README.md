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

With features (see below) some CSS/colour support is available.

## Examples

The simple functions like `from_read()` return formatted text (in various
formats including plain text).

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

A lower level API gives a bit more control.  This give the same result (except for
returning errors as Result instead of panicking):

```rust
use html2text::config;

let html = b"
       <ul>
         <li>Item one</li>
         <li>Item two</li>
         <li>Item three</li>
       </ul>";

assert_eq!(
    config::plain()
           .string_from_read(&html[..], 20)
           .unwrap(),
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

## Cargo Features

|Feature| Description|
|-------|------------|
|css    | Limited handling of CSS, adding Coloured nodes to the render tree. |
|html\_trace| Add verbose internal logging (not recommended) |
|html\_trace\_bt| Add backtraces to the verbose internal logging |

### CSS support

When the `css` feature is enabled, some simple CSS handling is available.

Style rules are taken from:
* If `Config::use_doc_css()` is called, then style from the document:
  * `<style>` elements
  * Inline `style` attributes (`<div style="...">`)
  * `<font color=...>`
* Independently of `use_doc_css`, extra rules can be added with `Config::add_css(...)`

The following CSS features are implemented:
* Basic selector matching (including child and descendents, classes and element
  types).
* CSS colors (`color`/`background-color`) will add
  `Coloured(...)`/`BgColoured(...)` nodes to the render tree.
* Rules with `display: none` will cause matching elements to be removed from
  the render tree.

The CSS handling is expected to improve in future (PRs welcome), but not to a full-
blown browser style system, which would be overkill for terminal output.

There are two ways to make use of the colours:
* Use `from_read_rich()` or one of its variants.  One of the annotations you may get
  back is `Colour(..)`.
* Use `from_read_coloured()`.  This is similar to `from_read()`, but you provide
  a function to add terminal colours (or other styling) based on the same
  RichAnnotations.  See examples/html2text.rs for an example using termion.
