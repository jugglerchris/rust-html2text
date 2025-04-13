function update_html() {
    const text = document.getElementById("input_html").value;
    const css = document.getElementById("conf_css").checked;
    const colour = document.getElementById("conf_colour").checked;

    let conf = wasmBindings.Config.new();
    console.log("CSS:", css);
    if (css) {
        conf.use_css();
    }
    if (colour) {
        conf.use_colour();
    }

    wasmBindings.format_html(conf, text);
}
window.addEventListener("TrunkApplicationStarted", update_html);

