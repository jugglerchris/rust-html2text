//! Some basic CSS support.
use std::io::Write;
use std::ops::Deref;

mod parser;

use crate::{
    markup5ever_rcdom::{
        Handle,
        NodeData::{self, Comment, Document, Element},
    },
    tree_map_reduce, Result, TreeMapResult, css::parser::parse_rules, Colour,
};

#[derive(Debug, Clone, PartialEq)]
enum SelectorComponent {
    Class(String),
    Element(String),
    Hash(String),
    Star,
    CombChild,
    CombDescendant,
}

#[derive(Debug, Clone, PartialEq)]
struct Selector {
    // List of components, right first so we match from the leaf.
    components: Vec<SelectorComponent>,
}

impl Selector {
    fn do_matches(comps: &[SelectorComponent], node: &Handle) -> bool {
        match comps.first() {
            None => return true,
            Some(comp) => match comp {
                SelectorComponent::Class(class) => match &node.data {
                    Document
                    | NodeData::Doctype { .. }
                    | NodeData::Text { .. }
                    | Comment { .. }
                    | NodeData::ProcessingInstruction { .. } => {
                        return false;
                    }
                    Element { attrs, .. } => {
                        let attrs = attrs.borrow();
                        for attr in attrs.iter() {
                            if &attr.name.local == "class" {
                                for cls in attr.value.split_whitespace() {
                                    if cls == class {
                                        return Self::do_matches(&comps[1..], node);
                                    }
                                }
                            }
                        }
                        return false;
                    }
                },
                SelectorComponent::Hash(hash) => {
                    if let Element { attrs, .. } = &node.data {
                        let attrs = attrs.borrow();
                        for attr in attrs.iter() {
                            if &attr.name.local == "id" {
                                if &*attr.value == hash {
                                    return Self::do_matches(&comps[1..], node);
                                }
                            }
                        }
                    }
                    false
                }
                SelectorComponent::Element(name) => match &node.data {
                    Element { name: eltname, .. } => {
                        if name == eltname.expanded().local.deref() {
                            return Self::do_matches(&comps[1..], node);
                        } else {
                            return false;
                        }
                    }
                    _ => {
                        return false;
                    }
                },
                SelectorComponent::Star => {
                    return Self::do_matches(&comps[1..], node);
                }
                SelectorComponent::CombChild => {
                    if let Some(parent) = node.parent.take() {
                        let parent_handle = parent.upgrade();
                        node.parent.set(Some(parent));
                        if let Some(ph) = parent_handle {
                            return Self::do_matches(&comps[1..], &ph);
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                SelectorComponent::CombDescendant => {
                    if let Some(parent) = node.parent.take() {
                        let parent_handle = parent.upgrade();
                        node.parent.set(Some(parent));
                        if let Some(ph) = parent_handle {
                            return Self::do_matches(&comps[1..], &ph)
                                || Self::do_matches(comps, &ph);
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            },
        }
    }
    fn matches(&self, node: &Handle) -> bool {
        Self::do_matches(&self.components, node)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Style {
    Colour(Colour),
    BgColour(Colour),
    DisplayNone,
}

#[derive(Debug, Clone)]
struct Ruleset {
    selector: Selector,
    styles: Vec<Style>,
}

/// Stylesheet data which can be used while building the render tree.
#[derive(Clone, Default, Debug)]
pub struct StyleData {
    rules: Vec<Ruleset>,
}

pub(crate) fn parse_style_attribute(text: &str) -> Result<Vec<Style>> {
    html_trace_quiet!("Parsing inline style: {text}");
    let (_rest, decls) = parse_rules(text)
        .map_err(|_| crate::Error::CssParseError)?;

    let styles = styles_from_properties2(&decls);
    html_trace_quiet!("Parsed inline style: {:?}", styles);
    Ok(styles)
}

fn styles_from_properties2(decls: &[parser::Declaration]) -> Vec<Style> {
    let mut styles = Vec::new();
    html_trace_quiet!("styles:from_properties2: {decls:?}");
    let mut overflow_hidden = false;
    let mut height_zero = false;
    for decl in decls {
        html_trace_quiet!("styles:from_properties2: {decl:?}");
        match &decl.data {
            parser::Decl::Unknown { .. } => {},
            parser::Decl::Color { value: parser::Colour::Rgb(r, g, b) } => {
                styles.push(Style::Colour(Colour{
                    r: *r,
                    g: *g, b: *b
                }));
            }
            parser::Decl::BackgroundColor { value: parser::Colour::Rgb(r, g, b) } => {
                styles.push(Style::BgColour(Colour{
                    r: *r,
                    g: *g, b: *b
                }));
            }
            parser::Decl::Height { value } => {
                match value {
                    parser::Height::Auto => (),
                    parser::Height::Length(l, _) => {
                        if *l == 0.0 {
                            height_zero = true;
                        }
                    }
                }
            }
            parser::Decl::MaxHeight { value } => {
                match value {
                    parser::Height::Auto => (),
                    parser::Height::Length(l, _) => {
                        if *l == 0.0 {
                            height_zero = true;
                        }
                    }
                }
            }
            parser::Decl::Overflow { value: parser::Overflow::Hidden } |
            parser::Decl::OverflowY { value: parser::Overflow::Hidden } => {
                overflow_hidden = true;
            }
            parser::Decl::Overflow { .. } |
            parser::Decl::OverflowY { .. } => { }
            parser::Decl::Display { value } => {
                if let parser::Display::None = value {
                    styles.push(Style::DisplayNone);
                }
            }
            /*
            _ => {
                html_trace_quiet!("CSS: Unhandled property {:?}", decl);
            }
            */
        }
    }
    // If the height is set to zero and overflow hidden, treat as display: none
    if height_zero && overflow_hidden {
        styles.push(Style::DisplayNone);
    }
    styles
}

impl StyleData {
    /// Add some CSS source to be included.  The source will be parsed
    /// and the relevant and supported features extracted.
    pub fn add_css(&mut self, css: &str) -> Result<()> {
        let (_, ss) = parser::parse_stylesheet(css)
            .map_err(|_| crate::Error::CssParseError)?;

        for rule in ss {
            let styles = styles_from_properties2(&rule.declarations);
            if !styles.is_empty() {
                for selector in rule.selectors {
                    let ruleset = Ruleset {
                        selector,
                        styles: styles.clone(),
                    };
                    html_trace_quiet!("Adding ruleset {ruleset:?}");
                    self.rules.push(ruleset);
                }
            }
        }
        Ok(())
    }

    /// Merge style data from other into this one.
    /// Data on other takes precedence.
    pub fn merge(&mut self, other: Self) {
        self.rules.extend(other.rules);
    }

    pub(crate) fn matching_rules(&self, handle: &Handle, use_doc_css: bool) -> Vec<Style> {
        let mut result = Vec::new();
        for rule in &self.rules {
            if rule.selector.matches(handle) {
                result.extend(rule.styles.iter().cloned());
            }
        }
        if use_doc_css {
            // Now look for a style attribute
            if let Element { attrs, .. } = &handle.data {
                let borrowed = attrs.borrow();
                for attr in borrowed.iter() {
                    if &attr.name.local == "style" {
                        let rules = parse_style_attribute(&attr.value).unwrap_or_default();
                        result.extend(rules);
                    } else if &*attr.name.local == "color" {
                        if let Ok(colour) = parser::parse_color_attribute(&*attr.value) {
                            result.push(Style::Colour(colour.into()));
                        }
                    } else if &*attr.name.local == "bgcolor" {
                        if let Ok(colour) = parser::parse_color_attribute(&*attr.value) {
                            result.push(Style::BgColour(colour.into()));
                        }
                    }
                }
            }
        }

        result
    }
}

fn pending<'a, F>(handle: Handle, f: F) -> TreeMapResult<'a, (), Handle, Vec<String>>
where
    for<'r> F: Fn(&'r mut (), Vec<Vec<String>>) -> Result<Option<Vec<String>>> + 'static,
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
    _err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, Vec<String>> {
    use TreeMapResult::*;

    match handle.clone().data {
        Document => pending(handle, |&mut (), cs| Ok(Some(combine_vecs(cs)))),
        Comment { .. } => Nothing,
        Element { ref name, .. } => {
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
                _ => pending(handle, |_, cs| Ok(Some(combine_vecs(cs)))),
            }
        }
        NodeData::Text {
            contents: ref _tstr,
        } => Nothing,
        _ => {
            // NodeData doesn't have a Debug impl.
            Nothing
        }
    }
}

/// Extract stylesheet data from document.
pub fn dom_to_stylesheet<T: Write>(handle: Handle, err_out: &mut T) -> Result<StyleData> {
    let styles = tree_map_reduce(&mut (), handle, |_, handle| {
        Ok(extract_style_nodes(handle, err_out))
    })?;

    let mut result = StyleData::default();
    if let Some(styles) = styles {
        for css in styles {
            // Ignore CSS parse errors.
            let _ = result.add_css(&css);
        }
    }
    Ok(result)
}
