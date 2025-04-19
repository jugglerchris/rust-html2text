function update_html() {
    const text = document.getElementById("input_html").value;
    const css = document.getElementById("conf_css").checked;
    const colour = document.getElementById("conf_colour").checked;

    let rust_code = "";

    let conf = wasmBindings.Config.new();
    if (colour) {
        rust_code += "let config = html2text::config::rich()";
        conf.use_colour();
    } else {
        rust_code += "let config = html2text::config::plain()";
    }
    if (css) {
        conf.use_css();
        rust_code += "\n    .use_doc_css()";
    }

    rust_code += ";\n";
    if (colour) {
        rust_code += `
let lines = conf.lines_from_read(input, width);
for line in lines {
    for ts in line.tagged_strings() {
        // examine tags for each text span for colours etc.
    }
}
`;
    } else {
        rust_code += `
let text = conf.string_from_read(input, width);
`;
    }

    let tn = document.createTextNode(rust_code);
    document.getElementById("rust-code").replaceChildren(tn);
    wasmBindings.format_html(conf, text);
}

function start() {
    const confItems = document.querySelectorAll("input");
    confItems.forEach((elt) => {
        elt.addEventListener("change", update_html);
    });
    // Do the first render
    update_html();
}
window.addEventListener("TrunkApplicationStarted", start);
