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

| <input type="checkbox" id="conf_css" checked=true>use_doc_css | Parse CSS from the HTML document (css) |
| <input type="checkbox" id="conf_colour" checked=true>Enable Colour | Use colours (css) |
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



<textarea id="input_html" onchange="update_html()" oninput="update_html()">
<html>
<style>
.green {
    color: #4f4;
}
</style>
<body>
  <h1>Hi there</h1>
  <p>This is some simple text</p>
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

<div id="lib"></div>

<script type="module">
import init, * as bindings from '/rust-html2text/assets/html2text-web-demo.js';
const wasm = await init({ module_or_path: '/rust-html2text/assets/html2text-web-demo_bg.wasm' });

window.wasmBindings = bindings;

dispatchEvent(new CustomEvent("TrunkApplicationStarted", {detail: {wasm}}));

</script>
