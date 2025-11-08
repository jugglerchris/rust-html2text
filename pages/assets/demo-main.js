const img_modes = {
    "ignore": "IgnoreEmpty",
    "always": "ShowAlways",
    "replace": "Replace(\"XX\")",
    "filename": "Filename",
};
const controls = [
    // Element id, (conf, value) -> "rust code"
    ["conf_css", (conf, value) => { conf.use_css(); return ".use_doc_css()"; }],
    ["conf_user_css", (conf, value) => { conf.add_user_css(value); return `.add_css(r#"{value}"#)`; }],
    ["conf_agent_css", (conf, value) => { conf.add_agent_css(value); return `.add_agent_css(r#"{value}"#)`; }],
    ["conf_pad_block_width", (conf, value) => { conf.bad_block_width(); return `.pad_block_width()`; }],
    ["conf_wrap_width", (conf, value) => { conf.max_wrap_width(value); return `.max_wrap_width({value})`; }],
    ["conf_allow_overflow", (conf, value) => { conf.allow_overflow(); return `.allow_width_overflow()`; }],
    ["conf_min_wrap_width", (conf, value) => { conf.min_wrap_width(value); return `.min_wrap_width({value})`; }],
    ["conf_raw", (conf, value) => { conf.raw_mode(); return `.raw_mode(true)`; }],
    ["conf_no_borders", (conf, value) => { conf.no_borders(); return `.no_table_borders(true)`; }],
    ["conf_no_link_wrap", (conf, value) => { conf.no_link_wrap(); return `.no_link_wrapping(true)`; }],
    ["conf_unicode_so", (conf, value) => { conf.unicode_so(); return `.unicode_strikeout(true)`; }],
    ["conf_do_decorate", (conf, value) => { conf.do_decorate(); return `.do_decorate(true)`; }],
    ["conf_link_footnotes", (conf, value) => { conf.link_footnotes(value); return `.link_footnotes({value})`; }],
    ["conf_img_mode", (conf, value) => { conf.image_mode(value); return '.empty_img_mode(ImageRenderMode::' + img_modes[value] + ")"; }],

];
function update_html() {
    const text = document.getElementById("input_html").value;
    const colour = document.getElementById("conf_colour").checked;

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
    for (const conf_desc of controls) {
        const elt_id = conf_desc[0];
        const handler = conf_desc[1];

        const elt = document.getElementById(elt_id);
        if (elt.type == "checkbox") {
            if (elt.checked) {
                let codefrag = handler(conf, elt.checked);
                if (codefrag) {
                    rust_code += "\n    " + codefrag;
                }
            }
        } else {
            if (elt.value) {
                let codefrag = handler(conf, elt.value);
                if (codefrag) {
                    rust_code += "\n    " + codefrag;
                }
            }
        }
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
    const selectItems = document.querySelectorAll("select");
    selectItems.forEach((elt) => {
        elt.addEventListener("change", update_html);
    });
    // Do the first render
    update_html();
}
window.addEventListener("TrunkApplicationStarted", start);
