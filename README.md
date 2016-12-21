# html2text

html2text is a [Rust](http://www.rust-lang.org/) crate which converts HTML to
plain text.

It makes use of the [Servo project](https://github.com/servo/servo)'s HTML
parser, [html5ever](https://github.com/servo/html5ever/), using the DOM to
generate text (which can optionally include annotations for some features such
as hyperlinks).

