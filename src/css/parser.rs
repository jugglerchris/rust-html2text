//! Parsing for the subset of CSS used in html2text.

use std::str::FromStr;

use nom::{IResult, branch::alt, character::complete::{self, alpha0, digit1, digit0, alpha1}, bytes::complete::{tag, take, take_until, take_while}, combinator::{map, fail, opt, all_consuming, verify, recognize}, multi::{many0, separated_list0, many1}, error::ErrorKind, sequence::tuple};

#[derive(Debug, PartialEq)]
pub enum Colour {
    Rgb(u8, u8, u8),
}

#[derive(Debug, PartialEq)]
pub enum LengthUnit {
    // Absolute units
    In,
    Cm,
    Mm,
    Pt,
    Pc,
    Px,
    // Relative units
    Em,
    Ex
}

#[derive(Debug, PartialEq)]
pub enum Height {
    #[allow(unused)]
    Auto,
    // If the length is 0, the unit will be Px
    Length(f32, LengthUnit),
}

#[derive(Debug, PartialEq)]
pub enum Overflow {
    Visible,
    Hidden,
    Scroll,
    Auto,
}

#[non_exhaustive]
#[derive(Debug, PartialEq)]
pub enum Display {
    None,
    Other
}

#[derive(Debug, PartialEq)]
pub enum Decl {
    Color {
        value: Colour,
    },
    BackgroundColor {
        value: Colour,
    },
    Height {
        value: Height,
    },
    MaxHeight {
        value: Height,
    },
    Overflow {
        value: Overflow,
    },
    OverflowY {
        value: Overflow,
    },
    Display {
        value: Display,
    },
    Unknown {
        name: PropertyName,
        value: String,
    },
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub data: Decl,
    pub important: Importance,
}

use super::{Selector, SelectorComponent};

#[derive(Debug, PartialEq)]
pub struct RuleSet {
    pub selectors: Vec<Selector>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub struct PropertyName(String);

fn match_comment(text: &str) -> IResult<&str, ()> {
    let (rest, _) = tag("/*")(text)?;
    map(take_until("*/"), |_count| ())(rest)
}

fn match_whitespace_item(text: &str) -> IResult<&str, ()> {
    alt((map(complete::one_of(" \t\r\n\x0c"), |_c| ()), match_comment))(text)
}

fn skip_optional_whitespace(text: &str) -> IResult<&str, ()> {
    map(many0(match_whitespace_item), |_res| ())(text)
}

fn nmstart_char(s: &str) -> IResult<&str, char> {
    let mut iter = s.chars();
    match iter.next() {
        Some(c) => {
            match c {
                '_' | 'a'..='z' | 'A' ..= 'Z' => {
                    Ok((iter.as_str(), c.to_ascii_lowercase()))
                }
                _ => {
                    IResult::Err(nom::Err::Error(nom::error::Error::new(s, ErrorKind::Fail)))
                }
            }
        }
        None => fail(s),
    }
}

fn nmchar_char(s: &str) -> IResult<&str, char> {
    let mut iter = s.chars();
    match iter.next() {
        Some(c) => {
            match c {
                '_' | 'a'..='z' | 'A' ..= 'Z' | '0' ..= '9' | '-' => {
                    Ok((iter.as_str(), c.to_ascii_lowercase()))
                }
                _ => {
                    IResult::Err(nom::Err::Error(nom::error::Error::new(s, ErrorKind::Fail)))
                }
            }
        }
        None => fail(s)
    }
}

fn ident_escape(s: &str) -> IResult<&str, char> {
    // We may not need escapes for property names.
    fail(s)
}

fn nmstart(text: &str) -> IResult<&str, char> {
    alt((
      nmstart_char,
      ident_escape))(text)
}

fn nmchar(text: &str) -> IResult<&str, char> {
    alt((
        nmchar_char,
        ident_escape))(text)
}

fn parse_ident(text: &str) -> IResult<&str, String> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let mut name = Vec::new();
    let (rest, dash) = opt(tag("-"))(rest)?;
    if let Some(_) = dash {
        name.push('-');
    }

    let (rest, start) = nmstart(rest)?;
    name.push(start);

    let (rest, chars) = many0(nmchar)(rest)?;
    name.extend(chars);
    Ok((rest, name.into_iter().collect()))
}

fn parse_property_name(text: &str) -> IResult<&str, PropertyName> {
    parse_ident(text)
        .map(|(r, s)| (r, PropertyName(s)))
}

fn parse_value(text: &str) -> IResult<&str, &str> {
    take_while(|c| c != ';')(text)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Importance {
    Default,
    Important
}

fn parse_important(text: &str) -> IResult<&str, Importance> {
    let (rest, (_, _ws1, id, _ws2)) = tuple((
            tag("!"),
            skip_optional_whitespace,
            parse_ident,
            skip_optional_whitespace))(text)?;
    if id != "important" {
        return fail(text);
    }
    Ok((rest, Importance::Important))
}

pub fn parse_declaration(text: &str) -> IResult<&str, Option<Declaration>> {
    let (rest, (prop, _ws1, _colon, _ws2, value, _ws3, important)) =
        tuple((
            parse_property_name,
            skip_optional_whitespace,
            tag(":"),
            skip_optional_whitespace,
            parse_value,
            skip_optional_whitespace,
            opt(parse_important)))(text)?;
    let decl = match prop.0.as_str() {
        "background-color" => {
            let (_rest, value) = all_consuming(parse_color)(&value)?;
            Decl::BackgroundColor { value }
        }
        "color" => {
            let (_rest, value) = all_consuming(parse_color)(&value)?;
            Decl::Color { value }
        }
        "height" => {
            let (_rest, value) = all_consuming(parse_height)(&value)?;
            Decl::Height { value }
        }
        "max-height" => {
            let (_rest, value) = all_consuming(parse_height)(&value)?;
            Decl::MaxHeight { value }
        }
        "overflow" => {
            let (_rest, value) = all_consuming(parse_overflow)(&value)?;
            Decl::Overflow { value }
        }
        "overflow-y" => {
            let (_rest, value) = all_consuming(parse_overflow)(&value)?;
            Decl::OverflowY { value }
        }
        "display" => {
            let (_rest, value) = all_consuming(parse_display)(&value)?;
            Decl::Display { value }
        }
        _ => Decl::Unknown {
            name: prop,
            value: value.into(),
        }
    };
    Ok((rest, Some(Declaration {
        data: decl,
        important: important.unwrap_or(Importance::Default),
    })))
}

fn hex1(text: &str) -> IResult<&str, u8> {
    let (rest, digit) = verify(take(1usize), |s: &str| -> bool { s.chars().all(|c| c.is_ascii_hexdigit())})(text)?;
    Ok((rest, u8::from_str_radix(digit, 16).unwrap()))
}

fn hex2(text: &str) -> IResult<&str, u8> {
    let (rest, digits) = verify(take(2usize), |s: &str| -> bool { s.chars().all(|c| c.is_ascii_hexdigit())})(text)?;
    Ok((rest, u8::from_str_radix(digits, 16).unwrap()))
}


fn hex_colour3(text: &str) -> IResult<&str, Colour> {
    let (rest, _) = tag("#")(text)?;
    let (rest, (r,g,b)) = tuple((hex1, hex1, hex1))(rest)?;
    let (rest, _) = skip_optional_whitespace(rest)?;
    Ok((rest, Colour::Rgb(r * 0x11, g * 0x11, b * 0x11)))
}

fn hex_colour6(text: &str) -> IResult<&str, Colour> {
    let (rest, _) = tag("#")(text)?;
    let (rest, (r,g,b)) = tuple((hex2, hex2, hex2))(rest)?;
    let (rest, _) = skip_optional_whitespace(rest)?;
    Ok((rest, Colour::Rgb(r, g, b)))
}

fn rgb_func_colour(text: &str) -> IResult<&str, Colour> {
    fail(text)
}

fn named_colour(text: &str) -> IResult<&str, Colour> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let (rest, name) = dbg!(parse_ident(rest))?;
    let colour = match name.as_str() {
        "aqua" => Colour::Rgb(0, 0xff, 0xff),
        "black" => Colour::Rgb(0, 0, 0),
        "blue" => Colour::Rgb(0, 0, 0xff),
        "fuchsia" => Colour::Rgb(0xff, 0, 0xff),
        "gray" => Colour::Rgb(0x80, 0x80, 0x80),
        "green" => Colour::Rgb(0, 0x80, 0),
        "lime" => Colour::Rgb(0, 0xff, 0),
        "maroon" => Colour::Rgb(0x80, 0, 0),
        "navy" => Colour::Rgb(0, 0, 0x80),
        "olive" => Colour::Rgb(0x80, 0x80, 0),
        "orange" => Colour::Rgb(0xff, 0xa5, 0),
        "purple" => Colour::Rgb(0x80, 0, 0x80),
        "red" => Colour::Rgb(0xff, 0, 0),
        "silver" => Colour::Rgb(0xc0, 0xc0, 0xc0),
        "teal" => Colour::Rgb(0, 0x80, 0x80),
        "white" => Colour::Rgb(0xff, 0xff, 0xff),
        "yellow" => Colour::Rgb(0xff, 0xff, 0),
        _ => {
            return fail(text);
        }
    };
    Ok((rest, colour))
}

fn parse_color(text: &str) -> IResult<&str, Colour> {
    let (rest, _) = skip_optional_whitespace(text)?;
    alt((
       named_colour,
       hex_colour6,
       hex_colour3,
       rgb_func_colour,
       ))(rest)
}

fn parse_integer(text: &str) -> IResult<&str, f32> {
    let (rest, digits) = digit1(text)?;
    Ok((rest, <f32 as FromStr>::from_str(digits).unwrap()))
}

fn parse_decimal(text: &str) -> IResult<&str, f32> {
    let (rest, valstr) = recognize(tuple((
            digit0,
            tag("."),
            digit1)))(text)?;
    Ok((rest, <f32 as FromStr>::from_str(valstr).unwrap()))
}

fn parse_number(text: &str) -> IResult<&str, f32> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let (rest, (sign, val)) =
        tuple((
            opt(alt((tag("-"), tag("+")))),
            alt((
                parse_integer,
                parse_decimal))))(rest)?;
    Ok((rest, match sign {
        Some("-") => -val,
        None | Some("+") => val,
        _ => unreachable!(),
    }))
}

fn parse_unit(text: &str) -> IResult<&str, LengthUnit> {
    let (rest, word) = alpha0(text)?;
    Ok((rest, match word {
        "in" => LengthUnit::In,
        "cm" => LengthUnit::Cm,
        "mm" => LengthUnit::Mm,
        "pt" => LengthUnit::Pt,
        "pc" => LengthUnit::Pc,
        "px" => LengthUnit::Px,
        "em" => LengthUnit::Em,
        "ex" => LengthUnit::Ex,
        _ => {
            return fail(text);
        }
    }))
}

fn parse_height(text: &str) -> IResult<&str, Height> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let (rest, num) = parse_number(rest)?;
    let (rest, unit) = opt(parse_unit)(rest)?;

    Ok((rest, match unit {
        Some(unit) => {
            Height::Length(num, unit)
        }
        None => {
            if num == 0.0 {
                Height::Length(num, LengthUnit::Px)
            } else {
                return fail(text);
            }
        }
    }))
}

fn parse_overflow(text: &str) -> IResult<&str, Overflow> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let (rest, word) = alpha1(rest)?;
    match word {
        "visible" => Ok((rest, Overflow::Visible)),
        "hidden" => Ok((rest, Overflow::Hidden)),
        "scroll" => Ok((rest, Overflow::Scroll)),
        "auto" => Ok((rest, Overflow::Auto)),
        _ => fail(text),
    }
}

fn parse_display(text: &str) -> IResult<&str, Display> {
    let (rest, _) = skip_optional_whitespace(text)?;
    if let Ok((rest, word)) = alpha1::<_, nom::error::Error<&str>>(rest) {
        match word {
            "none" => return Ok((rest, Display::None)),
            _ => (),
        }
    }
    Ok((rest, Display::Other))
}

pub fn parse_rules(text: &str) -> IResult<&str, Vec<Declaration>> {
    separated_list0(
        tuple((tag(";"), skip_optional_whitespace)),
        parse_declaration)(text)
        .map(|(rest, v)| (rest, v.into_iter()
                          .flatten()
                          .collect()))
}

pub fn parse_class(text: &str) -> IResult<&str, SelectorComponent> {
    let (rest, _) = tag(".")(text)?;
    let (rest, classname) = parse_ident(rest)?;
    Ok((rest, SelectorComponent::Class(classname)))
}

pub fn parse_hash(text: &str) -> IResult<&str, SelectorComponent> {
    fail(text)
    /*
    let (rest, _) = tag(".")(text)?;
    let (rest, id) = parse_hash(rest)?;
    Ok((rest, SelectorComponent::
    */
}

pub fn parse_simple_selector_component(text: &str) -> IResult<&str, SelectorComponent> {
    alt((
        parse_class,
        parse_hash))(text)
}

pub fn parse_selector_with_element(text: &str) -> IResult<&str, Vec<SelectorComponent>> {
    let (rest, ident) = parse_ident(text)?;
    let (rest, extras) = many0(
        parse_simple_selector_component)(rest)?;
    let mut result = vec![SelectorComponent::Element(ident.into())];
    result.extend(extras);
    Ok((rest, result))
}

pub fn parse_selector_without_element(text: &str) -> IResult<&str, Vec<SelectorComponent>> {
    many1(parse_simple_selector_component)(text)
}

pub fn parse_selector(text: &str) -> IResult<&str, Selector> {
    let (rest, components) =
        alt((
            parse_selector_with_element,
            parse_selector_without_element,
            fail))(text)?;
    Ok((rest, Selector {
        components
    }))
}

pub fn parse_ruleset(text: &str) -> IResult<&str, RuleSet> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let (rest, selectors) = dbg!(separated_list0(
        tuple((tag(","), skip_optional_whitespace)),
        parse_selector)(rest))?;
    let (rest, (_ws1, _bra, _ws2, declarations, _ws3, _optsemi, _ws4,_ket, _ws5)) =
        tuple((
            skip_optional_whitespace,
            tag("{"),
            skip_optional_whitespace,
            parse_rules,
            skip_optional_whitespace,
            opt(tag(";")),
            skip_optional_whitespace,
            tag("}"),
            skip_optional_whitespace,
            ))(rest)?;
    Ok((rest, RuleSet {
        selectors,
        declarations,
    }))
}

pub fn parse_stylesheet(text: &str) -> IResult<&str, Vec<RuleSet>> {
    many0(parse_ruleset)(text)
}

#[cfg(test)]
mod test {
    use crate::css::{parser::{Height, LengthUnit, RuleSet, Selector, Importance}, SelectorComponent};

    use super::{Decl, Declaration, PropertyName, Colour, Overflow};

    #[test]
    fn test_parse_decl() {
        assert_eq!(super::parse_declaration("foo:bar;"), Ok((";", Some(Declaration {
            data: Decl::Unknown {
                name: PropertyName("foo".into()),
                value: "bar".into()
            },
            important: Importance::Default,
        }))));
    }

    #[test]
    fn test_parse_overflow() {
        assert_eq!(
            super::parse_rules("overflow: hidden; overflow-y: scroll"),
            Ok(("",
                vec![
                Declaration {
                    data: Decl::Overflow { value: Overflow::Hidden }, 
                    important: Importance::Default,
                },
                Declaration {
                    data: Decl::OverflowY { value: Overflow::Scroll },
                    important: Importance::Default,
                },
                ])));
    }

    #[test]
    fn test_parse_color() {
        assert_eq!(
            super::parse_rules("color: #123; color: #abcdef"),
            Ok(("",
                vec![
                    Declaration {
                        data: Decl::Color {
                            value: Colour::Rgb(0x11, 0x22, 0x33)
                        },
                        important: Importance::Default,
                    },
                    Declaration {
                        data: Decl::Color {
                            value: Colour::Rgb(0xab, 0xcd, 0xef)
                        },
                        important: Importance::Default,
                    },
                ])));
    }

    #[test]
    fn test_parse_height() {
        assert_eq!(
            super::parse_rules("height: 0; max-height: 100cm"),
            Ok(("",
                vec![
                    Declaration {
                        data: Decl::Height {
                            value: Height::Length(0.0, LengthUnit::Px),
                        },
                        important: Importance::Default,
                    },
                    Declaration {
                        data: Decl::MaxHeight {
                            value: Height::Length(100.0, LengthUnit::Cm),
                        },
                        important: Importance::Default,
                    },
                ])));
    }

    #[test]
    fn test_parse_empty_ss() {
        assert_eq!(
            super::parse_stylesheet(""),
            Ok(("", vec![])));
    }

    #[test]
    fn test_parse_ss_col() {
        assert_eq!(
            super::parse_stylesheet("
            foo {
                color: #112233;
            }
            "),
            Ok(("", vec![
                RuleSet {
                    selectors: vec![
                        Selector {
                            components: vec![
                                SelectorComponent::Element("foo".into()),
                            ],
                        },
                    ],
                    declarations: vec![
                        Declaration {
                            data: Decl::Color {
                                value: Colour::Rgb(0x11, 0x22, 0x33)
                            },
                            important: Importance::Default,
                        },
                    ],
                }
            ])));
    }

    #[test]
    fn test_parse_class() {
        assert_eq!(
            super::parse_stylesheet("
            .foo {
                color: #112233;
                background-color: #332211 !important;
            }
            "),
            Ok(("", vec![
                RuleSet {
                    selectors: vec![
                        Selector {
                            components: vec![
                                SelectorComponent::Class("foo".into()),
                            ],
                        },
                    ],
                    declarations: vec![
                        Declaration {
                            data: Decl::Color {
                                value: Colour::Rgb(0x11, 0x22, 0x33)
                            },
                            important: Importance::Default,
                        },
                        Declaration {
                            data: Decl::BackgroundColor {
                                value: Colour::Rgb(0x33, 0x22, 0x11)
                            },
                            important: Importance::Important,
                        },
                    ],
                }
            ])));
    }

    #[test]
    fn test_parse_named_colour() {
        assert_eq!(super::parse_color(" white"), Ok(("", Colour::Rgb(0xff, 0xff, 0xff))));
    }
}
