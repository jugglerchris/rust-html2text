//! Some basic CSS support.
use std::{collections::HashMap, io::Write};
use std::ops::Deref;

use lightningcss::{stylesheet::{
    ParserOptions, StyleSheet
}, rules::CssRule, properties::Property, values::color::CssColor};

use crate::{TreeMapResult, markup5ever_rcdom::{Handle, NodeData::{Comment, Document, Element, self}}, tree_map_reduce};

/// Stylesheet data which can be used while building the render tree.
#[derive(Clone, Default, Debug)]
pub struct StyleData {
    /// Map from classes to colours
    pub colours: HashMap<String, CssColor>,
}

impl StyleData {
    /// Add some CSS source to be included.  The source will be parsed
    /// and the relevant and supported features extracted.
    pub fn add_css(&mut self, css: &str) {
        let ss = StyleSheet::parse(css, ParserOptions::default()).unwrap();

        for rule in &ss.rules.0 {
            match rule {
                CssRule::Style(style) => {
                    for decl in &style.declarations.declarations {
                        match decl {
                            Property::Color(color) => {
                                for selector in &style.selectors.0 {
                                    for item in selector.iter() {
                                        use lightningcss::selector::Component;
                                        match item {
                                            Component::Class(c) => { 
                                                self.colours.insert(c.0.to_string(), color.clone());
                                            }
                                            _ => {  }
                                        }
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
        }
    }
}

fn pending<'a, F>(handle: Handle, f: F) -> TreeMapResult<'a, (), Handle, Vec<String>>
where
    for<'r> F: Fn(&'r mut (), Vec<Vec<String>>) -> Option<Vec<String>> + 'static,
{
    TreeMapResult::PendingChildren {
        children: handle.children.borrow().clone(),
        cons: Box::new(f),
        prefn: None,
        postfn: None,
    }
}

fn combine_vecs(vecs: Vec<Vec<String>>) -> Vec<String> {
    let mut it = vecs.into_iter();
    let first = it.next();
    match first {
        None => Vec::new(),
        Some(mut first) => {
            for v in it {
                first.extend(v.into_iter());
            }
            first
        }
    }
}

fn extract_style_nodes<'a, 'b, T: Write>(
    handle: Handle,
    err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, Vec<String>> {
    use TreeMapResult::*;

    match handle.clone().data {
        Document => pending(handle, |&mut (), cs| Some(combine_vecs(cs))),
        Comment { .. } => Nothing,
        Element {
            ref name,
            ..
        } => {
            match name.expanded() {
                expanded_name!(html "style") => {
                    let mut result = String::new();
                    // Assume just a flat text node
                    for child in handle.children.borrow().iter() {
                        if let NodeData::Text { ref contents } = child.data {
                            result += &String::from(contents.borrow().deref());
                        }
                    }
                    Finished(vec![result])
                }
                _ => {
                    html_trace!("Unhandled element: {:?}\n", name.local);
                    pending(handle, |_, cs| Some(combine_vecs(cs)))
                    //None
                }
            }
        }
        NodeData::Text { contents: ref _tstr } => {
            Nothing
        }
        _ => {
            // NodeData doesn't have a Debug impl.
            write!(err_out, "Unhandled node type.\n").unwrap();
            Nothing
        }
    }
}

/// Extract stylesheet data from document.
pub fn dom_to_stylesheet<T: Write>(handle: Handle, err_out: &mut T) -> StyleData {
    let styles = tree_map_reduce(&mut (), handle, |_, handle| {
        extract_style_nodes(handle, err_out)
    });

    let mut result = StyleData::default();
    if let Some(styles) = styles {
        for css in styles {
            result.add_css(&css);
        }
    }
    result
}

