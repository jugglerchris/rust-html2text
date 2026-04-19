# html2text

A command-line tool to convert HTML into text in various forms.  Supports some
CSS including colours with default features.

The default, which gives text with ASCII annotations similar to (but not
exactly) markdown for things like `<strong>` elements and lists, and table
formatting. It defaults to reading from standard input and writing to standard
output.

```sh
html2text foo.html
```

For a more fancy terminal output, including ANSI colours, syntax highlighting
using syntect (`css_ext` cargo feature required) and added CSS rules:

```sh
html2text --colour --css --syntax --agent-css 'pre.rust { x-syntax: rs; }' < file_with_rust_code.html
```
