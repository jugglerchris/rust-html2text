//! Some basic CSS support.
use std::io::Write;
use std::convert::TryFrom;
use std::ops::Deref;

use lightningcss::{stylesheet::{
    ParserOptions, StyleSheet
}, rules::CssRule, properties::{Property, display::{self, DisplayKeyword}}, values::color::CssColor};

use crate::{Result, TreeMapResult, markup5ever_rcdom::{Handle, NodeData::{Comment, Document, Element, self}}, tree_map_reduce};

#[derive(Debug, Clone)]
enum SelectorComponent {
    Class(String),
    Element(String),
    Star,
    CombChild,
    CombDescendant,
}

#[derive(Debug, Clone)]
struct Selector {
    // List of components, right first so we match from the leaf.
    components: Vec<SelectorComponent>,
}

impl Selector {
    fn do_matches(comps: &[SelectorComponent], node: &Handle) -> bool {
        match comps.first() {
            None => return true,
            Some(comp) => {
                match comp {
                    SelectorComponent::Class(class) => {
                        match &node.data {
                            Document |
                                NodeData::Doctype { .. } |
                                NodeData::Text { .. } |
                                Comment { .. } |
                                NodeData::ProcessingInstruction { .. } => {
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
                        }
                    }
                    SelectorComponent::Element(name) => {
                        match &node.data {
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
                        }
                    }
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
                                return Self::do_matches(&comps[1..], &ph) ||
                                    Self::do_matches(comps, &ph);
                            } else {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                }
            }
        }
    }
    fn matches(&self, node: &Handle) -> bool {
        Self::do_matches(&self.components, node)
    }
}

impl<'r, 'i> TryFrom<&'r lightningcss::selector::Selector<'i>> for Selector {
    type Error = ();

    fn try_from(selector: &'r lightningcss::selector::Selector<'i>) -> std::result::Result<Self, Self::Error> {
        let mut components = Vec::new();

        use lightningcss::selector::Component;
        use lightningcss::selector::Combinator;

        let mut si = selector.iter();
        loop {
            while let Some(item) = si.next() {
                match item {
                    Component::Class(id) => {
                        components.push(SelectorComponent::Class(String::from(id.deref())));
                    }
                    Component::LocalName(name) => {
                        components.push(SelectorComponent::Element(String::from(name.lower_name.deref())));
                    }
                    Component::ExplicitUniversalType => {
                        components.push(SelectorComponent::Star);
                    }
                    _ => {
                        html_trace!("Unknown component {:?}", item);
                        return Err(());
                    }
                }
            }
            if let Some(comb) = si.next_sequence() {
                match comb {
                    Combinator::Child => {
                        components.push(SelectorComponent::CombChild);
                    }
                    Combinator::Descendant => {
                        components.push(SelectorComponent::CombDescendant);
                    }
                    _ => {
                        html_trace!("Unknown combinator {:?}", comb);
                        return Err(());
                    }
                }
            } else {
                break;
            }
        }
        Ok(Selector {
            components
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Style {
    Colour(CssColor),
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

impl StyleData {
    /// Add some CSS source to be included.  The source will be parsed
    /// and the relevant and supported features extracted.
    pub fn add_css(&mut self, css: &str) {
        let ss = StyleSheet::parse(css, ParserOptions::default()).unwrap();
        html_trace!("add css [[{css}]]");

        for rule in &ss.rules.0 {
            match rule {
                CssRule::Style(style) => {
                    let mut styles = Vec::new();
                    for decl in &style.declarations.declarations {
                        match decl {
                            Property::Color(color) => {
                                styles.push(Style::Colour(color.clone()));
                            }
                            Property::Display(disp) => {
                                if let display::Display::Keyword(DisplayKeyword::None) = disp {
                                    styles.push(Style::DisplayNone);
                                }
                            }
                            _ => {}
                        }
                    }
                    if !styles.is_empty() {
                        for selector in &style.selectors.0 {
                            match Selector::try_from(selector) {
                                Ok(selector) => {
                                    self.rules.push(Ruleset {
                                        selector,
                                        styles: styles.clone()
                                    });
                                }
                                Err(_) => {
                                    continue;
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
        }
    }

    /// Merge style data from other into this one.
    /// Data on other takes precedence.
    pub fn merge(&mut self, other: Self) {
        self.rules.extend(other.rules);
    }

    pub(crate) fn matching_rules(&self, handle: &Handle) -> Vec<Style> {
        let mut result = Vec::new();
        for rule in &self.rules {
            if rule.selector.matches(handle) {
                result.extend(rule.styles.iter().cloned());
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
    err_out: &'b mut T,
) -> TreeMapResult<'a, (), Handle, Vec<String>> {
    use TreeMapResult::*;

    match handle.clone().data {
        Document => pending(handle, |&mut (), cs| Ok(Some(combine_vecs(cs)))),
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
                    pending(handle, |_, cs| Ok(Some(combine_vecs(cs))))
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
pub fn dom_to_stylesheet<T: Write>(handle: Handle, err_out: &mut T) -> Result<StyleData> {
    let styles = tree_map_reduce(&mut (), handle, |_, handle| {
        Ok(extract_style_nodes(handle, err_out))
    })?;

    let mut result = StyleData::default();
    if let Some(styles) = styles {
        for css in styles {
            result.add_css(&css);
        }
    }
    Ok(result)
}

