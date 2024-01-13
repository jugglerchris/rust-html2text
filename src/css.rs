//! Some basic CSS support.
use std::io::Write;
use std::convert::TryFrom;
use std::ops::Deref;

use lightningcss::{stylesheet::{
    ParserOptions, StyleSheet, StyleAttribute
}, rules::CssRule, properties::{Property, display::{self, DisplayKeyword}}, values::color::CssColor, declaration::DeclarationBlock};

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
    BgColour(CssColor),
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
    let sattr = StyleAttribute::parse(text, ParserOptions::default())
        .map_err(|_| crate::Error::CssParseError)?;

    let styles = styles_from_properties(&sattr.declarations);
    html_trace_quiet!("Parsed inline style: {:?}", styles);
    Ok(styles)
}

fn is_transparent(color: &CssColor) -> bool {
    match color {
        CssColor::CurrentColor => false,
        CssColor::RGBA(rgba) => {
            rgba.alpha == 0
        }
        CssColor::LAB(_) => false,
        CssColor::Predefined(_) => false,
        CssColor::Float(_) => false,
    }
}

fn styles_from_properties(decls: &DeclarationBlock<'_>) -> Vec<Style> {
    let mut styles = Vec::new();
    html_trace_quiet!("styles:from_properties: {decls:?}");
    for decl in decls.declarations.iter().chain(decls.important_declarations.iter()) {
        html_trace_quiet!("styles:from_properties: {decl:?}");
        match decl {
            Property::Color(color) => {
                if is_transparent(&color) {
                    continue;
                }
                styles.push(Style::Colour(color.clone()));
            }
            Property::Background(bginfo) => {
                let color = bginfo.last().unwrap().color.clone();
                if is_transparent(&color) {
                    continue;
                }
                styles.push(Style::BgColour(color));
            }
            Property::BackgroundColor(color) => {
                if is_transparent(&color) {
                    continue;
                }
                styles.push(Style::BgColour(color.clone()));
            }
            Property::Display(disp) => {
                if let display::Display::Keyword(DisplayKeyword::None) = disp {
                    styles.push(Style::DisplayNone);
                }
            }
            _ => {
                html_trace_quiet!("CSS: Unhandled property {:?}", decl);
            }
        }
    }
    styles
}

impl StyleData {
    /// Add some CSS source to be included.  The source will be parsed
    /// and the relevant and supported features extracted.
    pub fn add_css(&mut self, css: &str) -> Result<()> {
        let ss = StyleSheet::parse(css, ParserOptions::default())
            .map_err(|_| crate::Error::CssParseError)?;

        for rule in &ss.rules.0 {
            match rule {
                CssRule::Style(style) => {
                    let styles = styles_from_properties(&style.declarations);
                    if !styles.is_empty() {
                        for selector in &style.selectors.0 {
                            match Selector::try_from(selector) {
                                Ok(selector) => {
                                    let ruleset = Ruleset {
                                        selector,
                                        styles: styles.clone()
                                    };
                                    html_trace_quiet!("Adding ruleset {ruleset:?}");
                                    self.rules.push(ruleset);
                                }
                                Err(_) => {
                                    html_trace!("Ignoring selector {:?}", selector);
                                    continue;
                                }
                            }
                        }
                    }
                }
                _ => (),
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
                        break;
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
                    pending(handle, |_, cs| Ok(Some(combine_vecs(cs))))
                }
            }
        }
        NodeData::Text { contents: ref _tstr } => {
            Nothing
        }
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

