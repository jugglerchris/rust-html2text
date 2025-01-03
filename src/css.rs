//! Some basic CSS support.
use std::ops::Deref;
use std::{io::Write, rc::Rc};

mod parser;

use crate::{
    css::parser::parse_rules,
    markup5ever_rcdom::{
        Handle,
        NodeData::{self, Comment, Document, Element},
    },
    tree_map_reduce, Colour, ComputedStyle, Result, Specificity, StyleOrigin, TreeMapResult,
    WhiteSpace,
};

use self::parser::Importance;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SelectorComponent {
    Class(String),
    Element(String),
    Hash(String),
    Star,
    CombChild,
    CombDescendant,
    NthChild {
        /* An + B [of sel] */
        a: i32,
        b: i32,
        sel: Selector,
    },
}

impl std::fmt::Display for SelectorComponent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SelectorComponent::Class(name) => write!(f, ".{}", name),
            SelectorComponent::Element(name) => write!(f, "{}", name),
            SelectorComponent::Hash(val) => write!(f, "#{}", val),
            SelectorComponent::Star => write!(f, " * "),
            SelectorComponent::CombChild => write!(f, " > "),
            SelectorComponent::CombDescendant => write!(f, " "),
            SelectorComponent::NthChild { a, b, .. } => write!(f, ":nth-child({}n+{})", a, b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Selector {
    // List of components, right first so we match from the leaf.
    components: Vec<SelectorComponent>,
}

impl std::fmt::Display for Selector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for comp in self.components.iter().rev() {
            comp.fmt(f)?;
        }
        Ok(())
    }
}

impl Selector {
    fn do_matches(comps: &[SelectorComponent], node: &Handle) -> bool {
        match comps.first() {
            None => true,
            Some(comp) => match comp {
                SelectorComponent::Class(class) => match &node.data {
                    Document
                    | NodeData::Doctype { .. }
                    | NodeData::Text { .. }
                    | Comment { .. }
                    | NodeData::ProcessingInstruction { .. } => false,
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
                        false
                    }
                },
                SelectorComponent::Hash(hash) => {
                    if let Element { attrs, .. } = &node.data {
                        let attrs = attrs.borrow();
                        for attr in attrs.iter() {
                            if &attr.name.local == "id" && &*attr.value == hash {
                                return Self::do_matches(&comps[1..], node);
                            }
                        }
                    }
                    false
                }
                SelectorComponent::Element(name) => match &node.data {
                    Element { name: eltname, .. } if name == eltname.expanded().local.deref() => {
                        Self::do_matches(&comps[1..], node)
                    }
                    _ => false,
                },
                SelectorComponent::Star => Self::do_matches(&comps[1..], node),
                SelectorComponent::CombChild => {
                    if let Some(parent) = node.get_parent() {
                        Self::do_matches(&comps[1..], &parent)
                    } else {
                        false
                    }
                }
                SelectorComponent::CombDescendant => {
                    if let Some(parent) = node.get_parent() {
                        Self::do_matches(&comps[1..], &parent) || Self::do_matches(comps, &parent)
                    } else {
                        false
                    }
                }
                SelectorComponent::NthChild { a, b, sel } => {
                    let parent = if let Some(parent) = node.get_parent() {
                        parent
                    } else {
                        return false;
                    };
                    let mut idx = 0i32;
                    for child in parent.children.borrow().iter() {
                        if let Element { .. } = child.data {
                            if sel.matches(child) {
                                idx += 1;
                                if Rc::ptr_eq(child, node) {
                                    break;
                                }
                            } else if Rc::ptr_eq(child, node) {
                                return false;
                            }
                        }
                    }
                    if idx == 0 {
                        // The child wasn't found(?)
                        return false;
                    }
                    /* The selector matches if idx == a*n + b, where
                     * n >= 0
                     */
                    let idx_offset = idx - b;
                    if *a == 0 {
                        return idx_offset == 0 && Self::do_matches(&comps[1..], node);
                    }
                    if (idx_offset % a) != 0 {
                        // Not a multiple
                        return false;
                    }
                    let n = idx_offset / a;
                    n >= 0 && Self::do_matches(&comps[1..], node)
                }
            },
        }
    }
    fn matches(&self, node: &Handle) -> bool {
        Self::do_matches(&self.components, node)
    }
    fn specificity(&self) -> Specificity {
        let mut result: Specificity = Default::default();

        for component in &self.components {
            match component {
                SelectorComponent::Class(_) => {
                    result.class += 1;
                }
                SelectorComponent::Element(_) => {
                    result.typ += 1;
                }
                SelectorComponent::Hash(_) => {
                    result.id += 1;
                }
                SelectorComponent::Star => {}
                SelectorComponent::CombChild => {}
                SelectorComponent::CombDescendant => {}
                SelectorComponent::NthChild { sel, .. } => {
                    result.class += 1;
                    result += &sel.specificity();
                }
            }
        }

        result
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Display {
    /// display: none
    None,
    #[cfg(feature = "css_ext")]
    /// Show node as HTML DOM
    ExtRawDom,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Style {
    Colour(Colour),
    BgColour(Colour),
    Display(Display),
    WhiteSpace(WhiteSpace),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StyleDecl {
    style: Style,
    importance: Importance,
}

impl std::fmt::Display for StyleDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.style {
            Style::Colour(col) => write!(f, "color: {}", col)?,
            Style::BgColour(col) => write!(f, "background-color: {}", col)?,
            Style::Display(Display::None) => write!(f, "display: none")?,
            #[cfg(feature = "css_ext")]
            Style::Display(Display::ExtRawDom) => write!(f, "display: x-raw-dom")?,
            Style::WhiteSpace(_) => todo!(),
        }
        match self.importance {
            Importance::Default => (),
            Importance::Important => write!(f, " !important")?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Ruleset {
    selector: Selector,
    styles: Vec<StyleDecl>,
}

impl std::fmt::Display for Ruleset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "  {} {{", self.selector)?;
        for decl in &self.styles {
            writeln!(f, "    {}", decl)?;
        }
        writeln!(f, "  }}")?;
        Ok(())
    }
}

/// Stylesheet data which can be used while building the render tree.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub(crate) struct StyleData {
    agent_rules: Vec<Ruleset>,
    user_rules: Vec<Ruleset>,
    author_rules: Vec<Ruleset>,
}

pub(crate) fn parse_style_attribute(text: &str) -> Result<Vec<StyleDecl>> {
    html_trace_quiet!("Parsing inline style: {text}");
    let (_rest, decls) = parse_rules(text).map_err(|_| crate::Error::CssParseError)?;

    let styles = styles_from_properties(&decls);
    html_trace_quiet!("Parsed inline style: {:?}", styles);
    Ok(styles)
}

fn styles_from_properties(decls: &[parser::Declaration]) -> Vec<StyleDecl> {
    let mut styles = Vec::new();
    html_trace_quiet!("styles:from_properties2: {decls:?}");
    let mut overflow_hidden = false;
    let mut height_zero = false;
    for decl in decls {
        html_trace_quiet!("styles:from_properties2: {decl:?}");
        match &decl.data {
            parser::Decl::Unknown { .. } => {}
            parser::Decl::Color {
                value: parser::Colour::Rgb(r, g, b),
            } => {
                styles.push(StyleDecl {
                    style: Style::Colour(Colour {
                        r: *r,
                        g: *g,
                        b: *b,
                    }),
                    importance: decl.important,
                });
            }
            parser::Decl::BackgroundColor {
                value: parser::Colour::Rgb(r, g, b),
            } => {
                styles.push(StyleDecl {
                    style: Style::BgColour(Colour {
                        r: *r,
                        g: *g,
                        b: *b,
                    }),
                    importance: decl.important,
                });
            }
            parser::Decl::Height { value } => match value {
                parser::Height::Auto => (),
                parser::Height::Length(l, _) => {
                    if *l == 0.0 {
                        height_zero = true;
                    }
                }
            },
            parser::Decl::MaxHeight { value } => match value {
                parser::Height::Auto => (),
                parser::Height::Length(l, _) => {
                    if *l == 0.0 {
                        height_zero = true;
                    }
                }
            },
            parser::Decl::Overflow {
                value: parser::Overflow::Hidden,
            }
            | parser::Decl::OverflowY {
                value: parser::Overflow::Hidden,
            } => {
                overflow_hidden = true;
            }
            parser::Decl::Overflow { .. } | parser::Decl::OverflowY { .. } => {}
            parser::Decl::Display { value } => match value {
                parser::Display::None => {
                    styles.push(StyleDecl {
                        style: Style::Display(Display::None),
                        importance: decl.important,
                    });
                }
                #[cfg(feature = "css_ext")]
                parser::Display::RawDom => {
                    styles.push(StyleDecl {
                        style: Style::Display(Display::ExtRawDom),
                        importance: decl.important,
                    });
                }
                _ => (),
            },
            parser::Decl::WhiteSpace { value } => {
                styles.push(StyleDecl {
                    style: Style::WhiteSpace(*value),
                    importance: decl.important,
                });
            } /*
              _ => {
                  html_trace_quiet!("CSS: Unhandled property {:?}", decl);
              }
              */
        }
    }
    // If the height is set to zero and overflow hidden, treat as display: none
    if height_zero && overflow_hidden {
        styles.push(StyleDecl {
            style: Style::Display(Display::None),
            importance: Importance::Default,
        });
    }
    styles
}

impl StyleData {
    /// Add some CSS source to be included.  The source will be parsed
    /// and the relevant and supported features extracted.
    fn do_add_css(css: &str, rules: &mut Vec<Ruleset>) -> Result<()> {
        let (_, ss) = parser::parse_stylesheet(css).map_err(|_| crate::Error::CssParseError)?;

        for rule in ss {
            let styles = styles_from_properties(&rule.declarations);
            if !styles.is_empty() {
                for selector in rule.selectors {
                    let ruleset = Ruleset {
                        selector,
                        styles: styles.clone(),
                    };
                    html_trace_quiet!("Adding ruleset {ruleset:?}");
                    rules.push(ruleset);
                }
            }
        }
        Ok(())
    }
    /// Add some CSS source to be included as part of the user agent ("browser") CSS rules.
    pub fn add_agent_css(&mut self, css: &str) -> Result<()> {
        Self::do_add_css(css, &mut self.agent_rules)
    }

    /// Add some CSS source to be included as part of the user CSS rules.
    pub fn add_user_css(&mut self, css: &str) -> Result<()> {
        Self::do_add_css(css, &mut self.user_rules)
    }

    /// Add some CSS source to be included as part of the document/author CSS rules.
    pub fn add_author_css(&mut self, css: &str) -> Result<()> {
        Self::do_add_css(css, &mut self.author_rules)
    }

    /// Merge style data from other into this one.
    /// Data on other takes precedence.
    pub fn merge(&mut self, other: Self) {
        self.agent_rules.extend(other.agent_rules);
        self.user_rules.extend(other.user_rules);
        self.author_rules.extend(other.author_rules);
    }

    pub(crate) fn computed_style(
        &self,
        parent_style: ComputedStyle,
        handle: &Handle,
        use_doc_css: bool,
    ) -> ComputedStyle {
        let mut result = parent_style;

        for (origin, ruleset) in [
            (StyleOrigin::Agent, &self.agent_rules),
            (StyleOrigin::User, &self.user_rules),
            (StyleOrigin::Author, &self.author_rules),
        ] {
            for rule in ruleset {
                if rule.selector.matches(handle) {
                    for style in rule.styles.iter() {
                        Self::merge_computed_style(
                            &mut result,
                            style.importance == Importance::Important,
                            origin,
                            rule.selector.specificity(),
                            style,
                        );
                    }
                }
            }
        }

        if use_doc_css {
            // Now look for a style attribute
            if let Element { attrs, .. } = &handle.data {
                let borrowed = attrs.borrow();
                for attr in borrowed.iter() {
                    if &attr.name.local == "style" {
                        let rules = parse_style_attribute(&attr.value).unwrap_or_default();
                        for style in rules {
                            Self::merge_computed_style(
                                &mut result,
                                false,
                                StyleOrigin::Author,
                                Specificity::inline(),
                                &style,
                            );
                        }
                    } else if &*attr.name.local == "color" {
                        if let Ok(colour) = parser::parse_color_attribute(&attr.value) {
                            Self::merge_computed_style(
                                &mut result,
                                false,
                                StyleOrigin::Author,
                                Specificity::inline(),
                                &StyleDecl {
                                    style: Style::Colour(colour.into()),
                                    importance: Importance::Default,
                                },
                            );
                        }
                    } else if &*attr.name.local == "bgcolor" {
                        if let Ok(colour) = parser::parse_color_attribute(&attr.value) {
                            Self::merge_computed_style(
                                &mut result,
                                false,
                                StyleOrigin::Author,
                                Specificity::inline(),
                                &StyleDecl {
                                    style: Style::BgColour(colour.into()),
                                    importance: Importance::Default,
                                },
                            );
                        }
                    }
                }
            }
        }

        result
    }

    fn merge_computed_style(
        result: &mut ComputedStyle,
        important: bool,
        origin: StyleOrigin,
        specificity: Specificity,
        style: &StyleDecl,
    ) {
        // The increasing priority is:
        // * agent
        // * user
        // * author
        // * author !important
        // * user !important
        // * agent !important
        // Since we view in the order agent, user, author, we always want to
        // replace the value if we haven't yet seen an !important rule, and
        // never afterwards.
        match style.style {
            Style::Colour(col) => {
                result
                    .colour
                    .maybe_update(important, origin, specificity, col);
            }
            Style::BgColour(col) => {
                result
                    .bg_colour
                    .maybe_update(important, origin, specificity, col);
            }
            Style::Display(disp) => {
                // We don't have a "not DisplayNone" - we might need to fix this.
                result
                    .display
                    .maybe_update(important, origin, specificity, disp);
            }
            Style::WhiteSpace(ws) => {
                result
                    .white_space
                    .maybe_update(important, origin, specificity, ws);
            }
        }
    }
}

impl std::fmt::Display for StyleData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.agent_rules.is_empty() {
            writeln!(f, "Agent rules:")?;
            for ruleset in &self.agent_rules {
                ruleset.fmt(f)?;
            }
        }
        if !self.user_rules.is_empty() {
            writeln!(f, "User rules:")?;
            for ruleset in &self.user_rules {
                ruleset.fmt(f)?;
            }
        }
        if !self.author_rules.is_empty() {
            writeln!(f, "Author rules:")?;
            for ruleset in &self.author_rules {
                ruleset.fmt(f)?;
            }
        }
        Ok(())
    }
}

fn pending<F>(handle: Handle, f: F) -> TreeMapResult<'static, (), Handle, Vec<String>>
where
    F: Fn(&mut (), Vec<Vec<String>>) -> Result<Option<Vec<String>>> + 'static,
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

fn extract_style_nodes<T: Write>(
    handle: Handle,
    _err_out: &mut T,
) -> TreeMapResult<'static, (), Handle, Vec<String>> {
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
                            result += &contents.borrow();
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
pub(crate) fn dom_to_stylesheet<T: Write>(handle: Handle, err_out: &mut T) -> Result<StyleData> {
    let styles = tree_map_reduce(&mut (), handle, |_, handle| {
        Ok(extract_style_nodes(handle, err_out))
    })?;

    let mut result = StyleData::default();
    if let Some(styles) = styles {
        for css in styles {
            // Ignore CSS parse errors.
            let _ = result.add_author_css(&css);
        }
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::Specificity;

    use super::parser::parse_selector;

    #[test]
    fn test_specificity() {
        let sel_id1 = parse_selector("#foo").unwrap().1;
        assert_eq!(
            sel_id1.specificity(),
            Specificity {
                id: 1,
                ..Default::default()
            }
        );

        let sel_cl3 = parse_selector(".foo .bar .baz").unwrap().1;
        assert_eq!(
            sel_cl3.specificity(),
            Specificity {
                class: 3,
                ..Default::default()
            }
        );

        assert!(sel_id1.specificity() > sel_cl3.specificity());
    }
}
