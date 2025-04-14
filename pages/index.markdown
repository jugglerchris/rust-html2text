---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

title: html2text API demo
layout: home

# Local additions
h2t_wasm: true
h2t_js: "/assets/demo-main.js"
---

<input type="checkbox" id="conf_css" checked=true onchange="update_html()">CSS
<input type="checkbox" id="conf_colour" checked=true onchange="update_html()">Colour

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
