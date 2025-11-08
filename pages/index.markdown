---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

title: html2text API demo
layout: home

# Local additions
h2t_wasm: true
h2t_js: "/assets/demo-main.js"
---

<noscript>
<h2 class="warning">This demo page requires javascript (and WASM) to work.</h2>
</noscript>

An online demonstration of the
[`html2text`](https://github.com/jugglerchris/rust-html2text) Rust crate. Edit
the HTML below and see how `html2text` converts it for text or terminal
display.

This demo uses `html2text` compiled to WASM, which can run in any modern
browser, with [ratzilla](https://github.com/orhun/ratzilla) for the web-based
terminal output.

<div id="h2tmain" markdown="1">

<div id="lib_container" markdown="1">

## Output

The html2text output is updated here:

<div id="lib"></div>

</div>

<div id="input_container">
<h2>Input HTML</h2>
<p>Edit the HTML here - the output will update live.</p>
<textarea id="input_html" onchange="update_html()" oninput="update_html()">
<html>
<style>
.green {
    color: #4f4;
}
</style>
<body>
  <h1>Hi there</h1>
  <p>This is some simple text with a <a href="https://github.com/jugglerchris/html2text/">link to github</a></p>
  <ol>
    <li>Item one</li>
    <li><s>Item two</s></li>
    <li class="green">Item three</li>
  </ol>
<table>
    <tr><th>Heading 1</th><th>Heading 2</th><th>Heading 3</th></tr>
    <tr><td>Data 1</td><td>Data 2</td><td>Data 3</td></tr>
    <tr><td colspan=3>Hello there</td></tr>
</table>
</body></html>
</textarea>
</div>
<div id="configtable" markdown="1">

## Configuration

The following are the configuration settings (accessible via [`html2text::config`](https://docs.rs/html2text/latest/html2text/config/struct.Config.html)).

| <input type="checkbox" id="conf_colour" checked=true>Use Rich output | The [`rich`](https://docs.rs/html2text/latest/html2text/config/fn.rich.html) mode returns spans with attributes (like hyperlinks, emphasis, or colours).  When disabled ([`plain`](https://docs.rs/html2text/latest/html2text/config/fn.plain.html)), the output is a plain `String` (possibly with formatting depending on other settings, e.g. table borders or `**markdown-style**` characters added).  Rich output adds extra information (annotations) to allow, for example, using terminal colours and other features for a nicer TUI.  |
| <input type="checkbox" id="conf_css" checked=true>use_doc_css | Parse CSS from the HTML document (css) |
| <input type="text" id="conf_user_css">User CSS | Add user stylesheet rules (css) |
| <input type="text" id="conf_agent_css">Agent CSS | Add browser stylesheet rules (css) |
| <input type="checkbox" id="conf_pad_block_width">Pad block width | Pad blocks to the width with spaces |
| <input type="number" id="conf_wrap_width">Text wrap width | Wrap text to this width even if overall width is wider |
| <input type="checkbox" id="conf_allow_overflow">Allow width overflow | Allow text to be too wide in extreme cases instead of returning an error |
| <input type="number" id="conf_min_wrap_width">Minimum wrap width | Set the minimum number of columns to use for text blocks. |
| <input type="checkbox" id="conf_raw">Raw mode | Render contents of tables as if they were just text. Implies `no_table_borders` |
| <input type="checkbox" id="conf_no_borders">Don't render table borders | Tables are shown without borders |
| <input type="checkbox" id="conf_no_link_wrap">Don't wrap URLs at the end | Some terminals handle long URLs better if not pre-wrapped |
| <input type="checkbox" id="conf_unicode_so">Use Unicode combining characters for strikeout | This allows crossed out text without terminal codes, but some environments don't render them correctly (e.g. offset). |
| <input type="checkbox" id="conf_do_decorate">Add markdown-like decoration | Add characters, e.g. `*` around `<em>` text even with plain decorators. |
| <input type="checkbox" id="conf_link_footnotes">URL footnotes | Add numbered list of URLs at the end of the output |
| <select name="imgmode" id="conf_img_mode"><option value="">--Select mode--</option><option value="ignore">IgnoreEmpty</option><option value="always">ShowAlways</option><option value="replace">Replace(...)</option><option value="filename">Filename</option></select> | Configure how images with no `alt` text are handled |

</div>

<div id="rust-code-pre" markdown="1">

## Rust API configuration

The code below shows how to use the currently selected settings in the Rust API.

<pre><code id="rust-code"></code></pre>
</div>

<script type="module">
import init, * as bindings from '/rust-html2text/assets/html2text-web-demo.js';
const wasm = await init({ module_or_path: '/rust-html2text/assets/html2text-web-demo_bg.wasm' });

window.wasmBindings = bindings;

dispatchEvent(new CustomEvent("TrunkApplicationStarted", {detail: {wasm}}));

</script>
