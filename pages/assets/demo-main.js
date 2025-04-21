function update_html() {
    const text = document.getElementById("input_html").value;
    const css = document.getElementById("conf_css").checked;
    const colour = document.getElementById("conf_colour").checked;
    const user_css = document.getElementById("conf_user_css").value;
    const agent_css = document.getElementById("conf_agent_css").value;

    const pad_block_width = document.getElementById("conf_pad_block_width").checked;
    const wrap_width = document.getElementById("conf_wrap_width").value;
    const allow_overflow = document.getElementById("conf_allow_overflow").checked;
    const min_wrap_width = document.getElementById("conf_min_wrap_width").value;
    const raw = document.getElementById("conf_raw").checked;
    const no_borders = document.getElementById("conf_no_borders").checked;
    const no_link_wrap = document.getElementById("conf_no_link_wrap").checked;
    const unicode_so = document.getElementById("conf_unicode_so").checked;
    const do_decorate = document.getElementById("conf_do_decorate").checked;
    const link_footnotes = document.getElementById("conf_link_footnotes").checked;

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
    if (user_css) {
        conf.add_user_css(user_css);
        rust_code += `\n    .add_css(r#"${user_css}"#)`;
    }
    if (agent_css) {
        conf.add_agent_css(agent_css);
        rust_code += `\n    .add_agent_css(r#"${agent_css}"#)`;
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
