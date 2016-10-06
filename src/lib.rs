#[macro_use]
extern crate string_cache;
extern crate html5ever;

use std::io;
use html5ever::{Parser,parse_document};
use html5ever::driver::ParseOpts;
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::rcdom::{RcDom,Handle,Text,Element,Document};
use html5ever::tendril::TendrilSink;

fn get_text(handle: Handle) -> String {
    let node = handle.borrow();
    let mut result = String::new();
    if let Text(ref tstr) = node.node {
        result.push_str(&tstr);
    } else {
        for child in node.children.iter() {
            result.push_str(&get_text(child.clone()));
        }
    }
    result
}

fn dom_to_string(handle: Handle) -> String {
    let node = handle.borrow();
    let mut result = String::new();
    match node.node {
        Document => {},
        Element(ref name, _, ref attrs) => {
            match *name {
                qualname!(html, "html") |
                qualname!(html, "div") |
                qualname!(html, "body") => {
                    /* process children, but don't add anything */
                },
                qualname!(html, "link") |
                qualname!(html, "meta") |
                qualname!(html, "hr") |
                qualname!(html, "head") => {
                    /* Ignore the head and its children */
                    return result;
                },
                qualname!(html, "h4") |
                qualname!(html, "p") => {
                    return get_text(handle.clone());
                }
                _ => {
                    result.push_str(&format!("Unhandled element: {:?}\n", name.local));
                },
            }
          },
        Text(ref tstr) => {
            return tstr.to_string();
        }
        _ => { result.push_str(&format!("Unhandled: {:?}\n", node)); },
    }
    for child in node.children.iter() {
        result.push_str(&dom_to_string(child.clone()));
    }
    result
}

pub fn from_read<R>(mut input: R) -> String where R: io::Read {
    let opts = ParseOpts {
        tree_builder: TreeBuilderOpts {
            drop_doctype: true,
            ..Default::default()
        },
        ..Default::default()
    };
    let dom = parse_document(RcDom::default(), opts)
                   .from_utf8()
                   .read_from(&mut input)
                   .unwrap();

    dom_to_string(dom.document)
}

#[cfg(test)]
mod tests {
}
