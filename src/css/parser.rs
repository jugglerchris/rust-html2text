//! Parsing for the subset of CSS used in html2text.

use std::{borrow::Cow, ops::Deref, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{self, digit0, digit1},
    combinator::{fail, map, opt, recognize},
    error::ErrorKind,
    multi::{many0, many1, separated_list0},
    sequence::tuple,
    AsChar, IResult,
};

#[derive(Debug, PartialEq)]
pub(crate) enum Colour {
    Rgb(u8, u8, u8),
}

impl From<Colour> for crate::Colour {
    fn from(value: Colour) -> Self {
        match value {
            Colour::Rgb(r, g, b) => crate::Colour { r, g, b },
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum LengthUnit {
    // Absolute units
    In,
    Cm,
    Mm,
    Pt,
    Pc,
    Px,
    // Relative units
    Em,
    Ex,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Height {
    #[allow(unused)]
    Auto,
    // If the length is 0, the unit will be Px
    Length(f32, LengthUnit),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Overflow {
    Visible,
    Hidden,
    Scroll,
    Auto,
}

#[non_exhaustive]
#[derive(Debug, PartialEq)]
pub(crate) enum Display {
    None,
    Other,
    #[cfg(feature = "css_ext")]
    RawDom,
}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub(crate) enum Decl {
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
    WhiteSpace {
        value: WhiteSpace,
    },
    Content {
        text: String,
    },
    Unknown {
        name: PropertyName,
        //        value: Vec<Token>,
    },
}

// Tokens as defined in the CSS Syntax Module Level 3
#[allow(unused)]
#[derive(Debug, PartialEq)]
enum Token<'s> {
    /// Plain identifier
    Ident(Cow<'s, str>),
    /// Start of a function call: <ident>(
    Function(Cow<'s, str>),
    /// @<ident>
    AtKeyword(Cow<'s, str>),
    /// #abcd12
    Hash(Cow<'s, str>),
    /// Quoted (double or single) string
    String(Cow<'s, str>),
    /// <bad-string-token>
    BadString(Cow<'s, str>),
    /// URL
    Url(Cow<'s, str>),
    /// <bad-url-token>
    BadUrl(Cow<'s, str>),
    /// Delim character
    Delim(char),
    /// Number
    Number(Cow<'s, str>),
    /// Dimension (number, unit)
    Dimension(Cow<'s, str>, Cow<'s, str>),
    /// Percentage
    Percentage(Cow<'s, str>),
    /// Whitespace
    //Whitespace(Cow<'s, str>),
    /// CDO (<!--)
    #[allow(clippy::upper_case_acronyms)]
    CDO,
    /// CDC (-->)
    #[allow(clippy::upper_case_acronyms)]
    CDC,
    /// Colon
    Colon,
    /// Semicolon
    Semicolon,
    /// Comma
    Comma,
    /// [-token
    OpenSquare,
    /// ]-token
    CloseSquare,
    /// (-token
    OpenRound,
    /// )-token
    CloseRound,
    /// {-token
    OpenBrace,
    /// }-token
    CloseBrace,
}

// A raw, uninterpreted declaration value.
#[derive(Debug, PartialEq)]
struct RawValue<'s> {
    tokens: Vec<Token<'s>>,
    important: bool,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Declaration {
    pub data: Decl,
    pub important: Importance,
}

use crate::css::styles_from_properties;

use super::{types::Importance, PseudoElement, Selector, SelectorComponent, StyleDecl, WhiteSpace};

#[derive(Debug, PartialEq)]
pub(crate) struct RuleSet {
    pub selectors: Vec<Selector>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct PropertyName(String);

fn match_comment(text: &str) -> IResult<&str, ()> {
    let (rest, _) = tag("/*")(text)?;
    let (rest, _) = take_until("*/")(rest)?;
    map(tag("*/"), |_t| ())(rest)
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
        Some(c) => match c {
            '_' | 'a'..='z' | 'A'..='Z' => Ok((iter.as_str(), c.to_ascii_lowercase())),
            _ => IResult::Err(nom::Err::Error(nom::error::Error::new(s, ErrorKind::Fail))),
        },
        None => fail(s),
    }
}

fn is_ident_start(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '\u{0081}'..)
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn nmchar_char(s: &str) -> IResult<&str, char> {
    let mut iter = s.chars();
    match iter.next() {
        Some(c) => match c {
            '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' | '-' => {
                Ok((iter.as_str(), c.to_ascii_lowercase()))
            }
            _ => IResult::Err(nom::Err::Error(nom::error::Error::new(s, ErrorKind::Fail))),
        },
        None => fail(s),
    }
}

fn ident_escape(s: &str) -> IResult<&str, char> {
    let (rest, _) = tag("\\")(s)?;
    let mut chars = rest.char_indices();

    match chars.next() {
        None => {
            // EOF: return replacement char
            Ok((rest, '\u{fffd}'))
        }
        Some((i, c)) if c.is_hex_digit() => {
            // Option 1: up to 6 hex digits.
            let start_idx = i;
            let mut end_idx = i + 1;
            for (nexti, nextc) in chars {
                if nextc.is_hex_digit() && nexti - start_idx < 6 {
                    continue;
                } else {
                    end_idx = nexti;
                    break;
                }
            }
            let val = u32::from_str_radix(&rest[start_idx..end_idx], 16).unwrap();
            Ok((&rest[end_idx..], char::from_u32(val).unwrap_or('\u{fffd}')))
        }
        Some((_i, c)) => {
            let bytes = c.len_utf8();
            Ok((&rest[bytes..], c))
        }
    }
}

fn nmstart(text: &str) -> IResult<&str, char> {
    alt((nmstart_char, ident_escape))(text)
}

fn nmchar(text: &str) -> IResult<&str, char> {
    alt((nmchar_char, ident_escape))(text)
}

fn parse_ident(text: &str) -> IResult<&str, String> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let mut name = Vec::new();
    let (rest, dash) = opt(tag("-"))(rest)?;
    if dash.is_some() {
        name.push('-');
    }

    let (rest, start) = nmstart(rest)?;
    name.push(start);

    let (rest, chars) = many0(nmchar)(rest)?;
    name.extend(chars);
    Ok((rest, name.into_iter().collect()))
}

fn parse_identstring(text: &str) -> IResult<&str, String> {
    let (rest, _) = skip_optional_whitespace(text)?;

    let (rest, name) = many1(nmchar)(rest)?;
    Ok((rest, name.into_iter().collect()))
}

fn parse_property_name(text: &str) -> IResult<&str, PropertyName> {
    parse_ident(text).map(|(r, s)| (r, PropertyName(s)))
}

// For now ignore whitespace
fn parse_token(text: &str) -> IResult<&str, Token> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let mut chars = rest.chars();
    match chars.next() {
        None => fail(rest),
        Some('"') | Some('\'') => parse_string_token(rest),
        Some('#') => match parse_identstring(&rest[1..]) {
            Ok((rest, id)) => Ok((rest, Token::Hash(id.into()))),
            Err(_) => Ok((rest, Token::Delim('#'))),
        },
        Some(';') => Ok((&rest[1..], Token::Semicolon)),
        Some('(') => Ok((&rest[1..], Token::OpenRound)),
        Some(')') => Ok((&rest[1..], Token::CloseRound)),
        Some('+') => match parse_numeric_token(&rest[1..]) {
            Ok(result) => Ok(result),
            Err(_) => Ok((&rest[1..], Token::Delim('+'))),
        },
        Some(',') => Ok((&rest[1..], Token::Comma)),
        Some('-') => {
            if let Ok((rest_n, tok)) = parse_numeric_token(rest) {
                return Ok((rest_n, tok));
            }
            if let Some(rest_cdc) = rest.strip_prefix("-->") {
                return Ok((rest_cdc, Token::CDC));
            }
            if let Ok((rest_id, token)) = parse_ident_like(rest) {
                return Ok((rest_id, token));
            }
            Ok((&rest[1..], Token::Delim('-')))
        }
        Some('.') => {
            if let Ok((rest_n, tok)) = parse_numeric_token(rest) {
                return Ok((rest_n, tok));
            }
            Ok((&rest[1..], Token::Delim('.')))
        }
        Some(':') => Ok((&rest[1..], Token::Colon)),
        Some('<') => {
            if let Some(rest_cdo) = rest.strip_prefix("<!--") {
                return Ok((rest_cdo, Token::CDO));
            }
            Ok((&rest[1..], Token::Delim('<')))
        }
        Some('@') => {
            if let Ok((rest_id, id)) = parse_ident(rest) {
                return Ok((rest_id, Token::AtKeyword(id.into())));
            }
            Ok((&rest[1..], Token::Delim('@')))
        }
        Some('[') => Ok((&rest[1..], Token::OpenSquare)),
        Some('\\') => {
            if let Ok((rest_id, token)) = parse_ident_like(rest) {
                Ok((rest_id, token))
            } else {
                Ok((&rest[1..], Token::Delim('\\')))
            }
        }
        Some(']') => Ok((&rest[1..], Token::CloseSquare)),
        Some('{') => Ok((&rest[1..], Token::OpenBrace)),
        Some('}') => Ok((&rest[1..], Token::CloseBrace)),
        Some(c) if is_ident_start(c) => parse_ident_like(rest),
        Some(c) if is_digit(c) => parse_numeric_token(rest),
        Some('!') => Ok((&rest[1..], Token::Delim('!'))),
        Some(c) => {
            let num_bytes = c.len_utf8();
            Ok((&rest[num_bytes..], Token::Delim(c)))
        }
    }
}

fn parse_token_not_semicolon(text: &str) -> IResult<&str, Token> {
    let (rest, token) = parse_token(text)?;
    if token == Token::Semicolon {
        fail(text)
    } else {
        Ok((rest, token))
    }
}

fn parse_value(text: &str) -> IResult<&str, RawValue> {
    let (rest, mut tokens) = many0(parse_token_not_semicolon)(text)?;
    let mut important = false;
    if let [.., Token::Delim('!'), Token::Ident(x)] = &tokens[..] {
        if x == "important" {
            tokens.pop();
            tokens.pop();
            important = true;
        }
    }
    Ok((rest, RawValue { tokens, important }))
}

pub(crate) fn parse_color_attribute(
    text: &str,
) -> Result<Colour, nom::Err<nom::error::Error<&'static str>>> {
    let (_rest, value) = parse_value(text).map_err(|_| empty_fail())?;
    parse_color(&value.tokens).or_else(|e| parse_faulty_color(e, text))
}

fn parse_color_part(text: &str, index: std::ops::Range<usize>) -> Option<u8> {
    u8::from_str_radix(text.get(index)?, 16).ok()
}

// Both Firefox and Chromium accept "00aabb" as a bgcolor - I'm not sure this has ever been legal,
// but regrettably I've had e-mails which were unreadable without doing this.
fn parse_faulty_color(
    e: nom::Err<nom::error::Error<&'static str>>,
    text: &str,
) -> Result<Colour, nom::Err<nom::error::Error<&'static str>>> {
    let text = text.trim();
    let r = parse_color_part(text, 0..2);
    let g = parse_color_part(text, 2..4);
    let b = parse_color_part(text, 4..6);
    if let (Some(r), Some(g), Some(b)) = (r, g, b) {
        return Ok(Colour::Rgb(r, g, b));
    }
    Err(e)
}

pub(crate) fn parse_declaration(text: &str) -> IResult<&str, Option<Declaration>> {
    let (rest, (prop, _ws1, _colon, _ws2, value)) = tuple((
        parse_property_name,
        skip_optional_whitespace,
        tag(":"),
        skip_optional_whitespace,
        parse_value,
    ))(text)?;
    let decl = match prop.0.as_str() {
        "background-color" => {
            if let Ok(value) = parse_color(&value.tokens) {
                Decl::BackgroundColor { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "background" => match parse_background_color(&value) {
            Ok(Some(value)) => Decl::BackgroundColor { value },
            _ => Decl::Unknown { name: prop },
        },
        "color" => {
            if let Ok(value) = parse_color(&value.tokens) {
                Decl::Color { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "height" => {
            if let Ok(value) = parse_height(&value) {
                Decl::Height { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "max-height" => {
            if let Ok(value) = parse_height(&value) {
                Decl::MaxHeight { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "overflow" => {
            if let Ok(value) = parse_overflow(&value) {
                Decl::Overflow { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "overflow-y" => {
            if let Ok(value) = parse_overflow(&value) {
                Decl::OverflowY { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "display" => {
            if let Ok(value) = parse_display(&value) {
                Decl::Display { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "white-space" => {
            if let Ok(value) = parse_white_space(&value) {
                Decl::WhiteSpace { value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        "content" => {
            if let Ok(value) = parse_content(&value) {
                Decl::Content { text: value }
            } else {
                Decl::Unknown { name: prop }
            }
        }
        _ => Decl::Unknown {
            name: prop,
            //            value: /*value*/"".into(),
        },
    };
    Ok((
        rest,
        Some(Declaration {
            data: decl,
            important: if value.important {
                Importance::Important
            } else {
                Importance::Default
            },
        }),
    ))
}

fn empty_fail() -> nom::Err<nom::error::Error<&'static str>> {
    nom::Err::Error(nom::error::Error::new("", ErrorKind::Fail))
}

fn parse_color(tokens: &[Token]) -> Result<Colour, nom::Err<nom::error::Error<&'static str>>> {
    let fail_error = empty_fail();
    if tokens.is_empty() {
        return Err(fail_error);
    }
    match tokens {
        [Token::Ident(c)] => {
            let colour = match c.deref() {
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
                    return Err(empty_fail());
                }
            };
            Ok(colour)
        }
        [Token::Function(name), rgb_args @ .., Token::CloseRound] => {
            use Token::*;
            match name.deref() {
                "rgb" => match rgb_args {
                    [Number(r), Comma, Number(g), Comma, Number(b)] => {
                        let r = r.parse().map_err(|_e| empty_fail())?;
                        let g = g.parse().map_err(|_e| empty_fail())?;
                        let b = b.parse().map_err(|_e| empty_fail())?;
                        Ok(Colour::Rgb(r, g, b))
                    }
                    _ => Err(empty_fail()),
                },
                _ => Err(empty_fail()),
            }
        }
        [Token::Hash(s)] => {
            if s.len() == 3 {
                let v = u32::from_str_radix(s, 16).map_err(|_| fail_error)?;
                let r = ((v >> 8) & 0xf) as u8 * 0x11;
                let g = ((v >> 4) & 0xf) as u8 * 0x11;
                let b = (v & 0xf) as u8 * 0x11;
                Ok(Colour::Rgb(r, g, b))
            } else if s.len() == 6 {
                let v = u32::from_str_radix(s, 16).map_err(|_| fail_error)?;
                let r = ((v >> 16) & 0xff) as u8;
                let g = ((v >> 8) & 0xff) as u8;
                let b = (v & 0xff) as u8;
                Ok(Colour::Rgb(r, g, b))
            } else {
                Err(fail_error)
            }
        }
        _ => Err(fail_error),
    }
}

// Parse background: value, extracting only the colour (if present).
fn parse_background_color(
    value: &RawValue,
) -> Result<Option<Colour>, nom::Err<nom::error::Error<&'static str>>> {
    let tokens = if let Some(last) = value.tokens.rsplit(|tok| *tok == Token::Comma).next() {
        last
    } else {
        return Err(empty_fail());
    };

    match parse_color(tokens) {
        Ok(col) => Ok(Some(col)),
        Err(_) => Ok(None),
    }
}

fn parse_integer(text: &str) -> IResult<&str, f32> {
    let (rest, digits) = digit1(text)?;
    Ok((rest, <f32 as FromStr>::from_str(digits).unwrap()))
}

fn parse_decimal(text: &str) -> IResult<&str, f32> {
    let (rest, valstr) = recognize(tuple((digit0, tag("."), digit1)))(text)?;
    Ok((rest, <f32 as FromStr>::from_str(valstr).unwrap()))
}

fn parse_number(text: &str) -> IResult<&str, f32> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let (rest, (sign, val)) = tuple((
        opt(alt((tag("-"), tag("+")))),
        alt((parse_integer, parse_decimal)),
    ))(rest)?;
    Ok((
        rest,
        match sign {
            Some("-") => -val,
            None | Some("+") => val,
            _ => unreachable!(),
        },
    ))
}

fn parse_numeric_token(text: &str) -> IResult<&str, Token> {
    let (rest, num) = recognize(parse_number)(text)?;
    let match_pct: IResult<_, _> = tag("%")(rest);
    if let Ok((rest_p, _)) = match_pct {
        return Ok((rest_p, Token::Percentage(num.into())));
    }
    match parse_ident(rest) {
        Ok((rest_id, dim)) => Ok((rest_id, Token::Dimension(num.into(), dim.into()))),
        Err(_) => Ok((rest, Token::Number(num.into()))),
    }
}

fn parse_ident_like(text: &str) -> IResult<&str, Token> {
    let (rest, ident) = parse_ident(text)?;
    // If the next character is '(', then it's a function token
    let match_bracket: IResult<_, _> = tag("(")(rest);
    match match_bracket {
        Ok((rest_f, _)) => Ok((rest_f, Token::Function(ident.into()))),
        Err(_) => Ok((rest, Token::Ident(ident.into()))),
    }
}

fn parse_string_token(text: &str) -> IResult<&str, Token> {
    let mut chars = text.char_indices();
    let mut s = String::new();
    let end_char = chars.next().unwrap().1;
    debug_assert!(end_char == '"' || end_char == '\'');

    loop {
        match chars.next() {
            None => return Ok(("", Token::String(s.into()))),
            Some((i, c)) if c == end_char => {
                return Ok((&text[i + 1..], Token::String(s.into())));
            }
            Some((i, '\n')) => {
                return Ok((&text[i..], Token::BadString(s.into())));
            }
            Some((i, '\\')) => {
                match chars.next() {
                    None => {
                        // Backslash at end
                        return Ok((&text[i + 1..], Token::String(s.into())));
                    }
                    Some((_i, '\n')) => {} // Eat the newline
                    Some((_i, c)) => {
                        s.push(c);
                    }
                }
            }
            Some((_, c)) => {
                s.push(c);
            }
        }
    }
}

/*
fn parse_unit(text: &str) -> IResult<&str, LengthUnit> {
    let (rest, word) = alpha0(text)?;
    Ok((rest, match word {
        _ => {
            return fail(text);
        }
    }))
}
*/

fn parse_height(value: &RawValue) -> Result<Height, nom::Err<nom::error::Error<&'static str>>> {
    match value.tokens[..] {
        [Token::Dimension(ref n, ref unit)] => {
            let (_, num) = parse_number(n).map_err(|_e| empty_fail())?;
            let unit = match unit.deref() {
                "in" => LengthUnit::In,
                "cm" => LengthUnit::Cm,
                "mm" => LengthUnit::Mm,
                "pt" => LengthUnit::Pt,
                "pc" => LengthUnit::Pc,
                "px" => LengthUnit::Px,
                "em" => LengthUnit::Em,
                "ex" => LengthUnit::Ex,
                _ => {
                    return Err(empty_fail());
                }
            };
            Ok(Height::Length(num, unit))
        }
        [Token::Number(ref n)] => {
            let (_, n) = parse_number(n).map_err(|_e| empty_fail())?;
            if n == 0.0 {
                Ok(Height::Length(0.0, LengthUnit::Px))
            } else {
                Err(empty_fail())
            }
        }
        _ => Err(empty_fail()),
    }
}

fn parse_overflow(value: &RawValue) -> Result<Overflow, nom::Err<nom::error::Error<&'static str>>> {
    for tok in &value.tokens {
        if let Token::Ident(word) = tok {
            match word.deref() {
                "visible" => {
                    return Ok(Overflow::Visible);
                }
                "hidden" => {
                    return Ok(Overflow::Hidden);
                }
                "scroll" => {
                    return Ok(Overflow::Scroll);
                }
                "auto" => {
                    return Ok(Overflow::Auto);
                }
                _ => {}
            }
        }
    }
    Err(empty_fail())
}

fn parse_display(value: &RawValue) -> Result<Display, nom::Err<nom::error::Error<&'static str>>> {
    for tok in &value.tokens {
        if let Token::Ident(word) = tok {
            #[allow(clippy::single_match)]
            match word.deref() {
                "none" => return Ok(Display::None),
                #[cfg(feature = "css_ext")]
                "x-raw-dom" => return Ok(Display::RawDom),
                _ => (),
            }
        }
    }
    Ok(Display::Other)
}

fn parse_white_space(
    value: &RawValue,
) -> Result<WhiteSpace, nom::Err<nom::error::Error<&'static str>>> {
    for tok in &value.tokens {
        if let Token::Ident(word) = tok {
            match word.deref() {
                "normal" => return Ok(WhiteSpace::Normal),
                "pre" => return Ok(WhiteSpace::Pre),
                "pre-wrap" => return Ok(WhiteSpace::PreWrap),
                _ => (),
            }
        }
    }
    Ok(WhiteSpace::Normal)
}

// Parse content - currently only support a single string.
fn parse_content(value: &RawValue) -> Result<String, nom::Err<nom::error::Error<&'static str>>> {
    let mut result = String::new();
    for tok in &value.tokens {
        if let Token::String(word) = tok {
            result.push_str(word);
        } else {
            return Err(empty_fail());
        }
    }
    Ok(result)
}

pub(crate) fn parse_rules(text: &str) -> IResult<&str, Vec<Declaration>> {
    separated_list0(
        tuple((tag(";"), skip_optional_whitespace)),
        parse_declaration,
    )(text)
    .map(|(rest, v)| (rest, v.into_iter().flatten().collect()))
}

fn parse_class(text: &str) -> IResult<&str, SelectorComponent> {
    let (rest, _) = tag(".")(text)?;
    let (rest, classname) = parse_ident(rest)?;
    Ok((rest, SelectorComponent::Class(classname)))
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum Sign {
    Plus,
    Neg,
}

impl Sign {
    fn val(&self) -> i32 {
        match self {
            Sign::Plus => 1,
            Sign::Neg => -1,
        }
    }
}

fn opt_sign(text: &str) -> IResult<&str, Sign> {
    match text.chars().next() {
        Some('-') => Ok((&text[1..], Sign::Neg)),
        Some('+') => Ok((&text[1..], Sign::Plus)),
        _ => Ok((text, Sign::Plus)),
    }
}
fn sign(text: &str) -> IResult<&str, Sign> {
    match text.chars().next() {
        Some('-') => Ok((&text[1..], Sign::Neg)),
        Some('+') => Ok((&text[1..], Sign::Plus)),
        _ => fail(text),
    }
}

fn parse_nth_child_args(text: &str) -> IResult<&str, SelectorComponent> {
    let (rest, _) = tag("(")(text)?;
    let (rest, _) = skip_optional_whitespace(rest)?;

    let (rest, (a, b)) = alt((
        map(tag("even"), |_| (2, 0)),
        map(tag("odd"), |_| (2, 1)),
        // The case where both a and b are specified
        map(
            tuple((
                opt_sign,
                opt(digit1),
                tag("n"),
                skip_optional_whitespace,
                sign,
                digit1,
            )),
            |(a_sign, a_opt_val, _, _, b_sign, b_val)| {
                let a =
                    <i32 as FromStr>::from_str(a_opt_val.unwrap_or("1")).unwrap() * a_sign.val();
                let b = <i32 as FromStr>::from_str(b_val).unwrap() * b_sign.val();
                (a, b)
            },
        ),
        // Just a
        map(
            tuple((opt_sign, opt(digit1), tag("n"))),
            |(a_sign, a_opt_val, _)| {
                let a =
                    <i32 as FromStr>::from_str(a_opt_val.unwrap_or("1")).unwrap() * a_sign.val();
                (a, 0)
            },
        ),
        // Just b
        map(tuple((opt_sign, digit1)), |(b_sign, b_val)| {
            let b = <i32 as FromStr>::from_str(b_val).unwrap() * b_sign.val();
            (0, b)
        }),
    ))(rest)?;

    let (rest, _) = tuple((skip_optional_whitespace, tag(")")))(rest)?;

    let sel = Selector {
        components: vec![SelectorComponent::Star],
        ..Default::default()
    };
    Ok((rest, SelectorComponent::NthChild { a, b, sel }))
}

fn parse_pseudo_class(text: &str) -> IResult<&str, SelectorComponent> {
    let (rest, _) = tag(":")(text)?;
    let (rest, pseudo_classname) = parse_ident(rest)?;
    match pseudo_classname.as_str() {
        "nth-child" => {
            let (rest, component) = parse_nth_child_args(rest)?;
            Ok((rest, component))
        }
        _ => fail(text),
    }
}

fn parse_hash(text: &str) -> IResult<&str, SelectorComponent> {
    let (rest, _) = tag("#")(text)?;
    let (rest, word) = parse_identstring(rest)?;
    Ok((rest, SelectorComponent::Hash(word)))
}

// Match some (not zero) whitespace
fn parse_ws(text: &str) -> IResult<&str, ()> {
    map(many1(match_whitespace_item), |_| ())(text)
}

fn parse_simple_selector_component(text: &str) -> IResult<&str, SelectorComponent> {
    alt((
        map(
            tuple((skip_optional_whitespace, tag(">"), skip_optional_whitespace)),
            |_| SelectorComponent::CombChild,
        ),
        map(
            tuple((skip_optional_whitespace, tag("*"), skip_optional_whitespace)),
            |_| SelectorComponent::Star,
        ),
        map(parse_ws, |_| SelectorComponent::CombDescendant),
        parse_class,
        parse_hash,
        map(parse_ident, SelectorComponent::Element),
        parse_pseudo_class,
    ))(text)
}

fn parse_selector_with_element(text: &str) -> IResult<&str, Vec<SelectorComponent>> {
    let (rest, ident) = parse_ident(text)?;
    let (rest, extras) = many0(parse_simple_selector_component)(rest)?;
    let mut result = vec![SelectorComponent::Element(ident)];
    result.extend(extras);
    Ok((rest, result))
}

fn parse_selector_without_element(text: &str) -> IResult<&str, Vec<SelectorComponent>> {
    many1(parse_simple_selector_component)(text)
}

pub(crate) fn parse_pseudo_element(text: &str) -> IResult<&str, Option<PseudoElement>> {
    opt(alt((
        map(tag("::before"), |_| PseudoElement::Before),
        map(tag("::after"), |_| PseudoElement::After),
    )))(text)
}

pub(crate) fn parse_selector(text: &str) -> IResult<&str, Selector> {
    let (rest, mut components) = alt((
        parse_selector_with_element,
        parse_selector_without_element,
        fail,
    ))(text)?;
    // Reverse.  Also remove any leading/trailing CombDescendant, as leading/trailing whitespace
    // shouldn't count as a descendent combinator.
    if let Some(&SelectorComponent::CombDescendant) = components.last() {
        components.pop();
    }
    components.reverse();
    if let Some(&SelectorComponent::CombDescendant) = components.last() {
        components.pop();
    }

    let (rest, pseudo_element) = parse_pseudo_element(rest)?;
    Ok((
        rest,
        Selector {
            components,
            pseudo_element,
        },
    ))
}

fn parse_ruleset(text: &str) -> IResult<&str, RuleSet> {
    let (rest, _) = skip_optional_whitespace(text)?;
    let (rest, selectors) =
        separated_list0(tuple((tag(","), skip_optional_whitespace)), parse_selector)(rest)?;
    let (rest, (_ws1, _bra, _ws2, declarations, _ws3, _optsemi, _ws4, _ket, _ws5)) = tuple((
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
    Ok((
        rest,
        RuleSet {
            selectors,
            declarations,
        },
    ))
}

fn skip_to_end_of_statement(text: &str) -> IResult<&str, ()> {
    let mut rest = text;

    let mut bra_stack = vec![];
    loop {
        let (remain, tok) = match parse_token(rest) {
            Ok(res) => res,
            Err(_) => return Ok((rest, ())),
        };
        match &tok {
            Token::Ident(..)
            | Token::AtKeyword(_)
            | Token::Hash(_)
            | Token::String(_)
            | Token::BadString(_)
            | Token::Url(_)
            | Token::BadUrl(_)
            | Token::Delim(_)
            | Token::Number(_)
            | Token::Dimension(_, _)
            | Token::Percentage(_)
            | Token::Colon
            | Token::Comma => (),

            Token::Function(_) | Token::OpenRound => {
                bra_stack.push(Token::CloseRound);
            }
            Token::CDO => {
                bra_stack.push(Token::CDC);
            }
            Token::OpenSquare => {
                bra_stack.push(Token::CloseSquare);
            }
            Token::OpenBrace => {
                bra_stack.push(Token::CloseBrace);
            }
            Token::Semicolon => {
                if bra_stack.is_empty() {
                    return Ok((remain, ()));
                }
            }
            Token::CloseBrace if bra_stack.is_empty() => {
                // The stack is empty, so don't include the closing brace.
                return Ok((rest, ()));
            }
            // Standard closing brackets
            Token::CDC | Token::CloseSquare | Token::CloseRound | Token::CloseBrace => {
                if bra_stack.last() == Some(&tok) {
                    bra_stack.pop();

                    if tok == Token::CloseBrace && bra_stack.is_empty() {
                        // The rule lasted until the end of the next block;
                        // eat this closing brace.
                        return Ok((remain, ()));
                    }
                } else {
                    // Unbalanced brackets
                    return fail(rest);
                }
            }
        }
        rest = remain;
    }
}

fn parse_at_rule(text: &str) -> IResult<&str, ()> {
    let (rest, _) = tuple((
        skip_optional_whitespace,
        tag("@"),
        skip_optional_whitespace,
        parse_ident,
    ))(text)?;

    skip_to_end_of_statement(rest)
}

fn parse_statement(text: &str) -> IResult<&str, Option<RuleSet>> {
    alt((map(parse_ruleset, Some), map(parse_at_rule, |_| None)))(text)
}

pub(crate) fn parse_stylesheet(text: &str) -> IResult<&str, Vec<RuleSet>> {
    let (rest, items) = many0(parse_statement)(text)?;
    Ok((rest, items.into_iter().flatten().collect()))
}

pub(crate) fn parse_style_attribute(text: &str) -> crate::Result<Vec<StyleDecl>> {
    html_trace_quiet!("Parsing inline style: {text}");
    let (_rest, decls) = parse_rules(text).map_err(|_| crate::Error::CssParseError)?;

    let styles = styles_from_properties(&decls);
    html_trace_quiet!("Parsed inline style: {:?}", styles);
    Ok(styles)
}

#[cfg(test)]
mod test {
    use crate::css::{
        parser::{Height, Importance, LengthUnit, RuleSet, Selector},
        PseudoElement, SelectorComponent,
    };

    use super::{Colour, Decl, Declaration, Overflow, PropertyName};

    #[test]
    fn test_parse_decl() {
        assert_eq!(
            super::parse_declaration("foo:bar;"),
            Ok((
                ";",
                Some(Declaration {
                    data: Decl::Unknown {
                        name: PropertyName("foo".into()),
                        //                value: "bar".into()
                    },
                    important: Importance::Default,
                })
            ))
        );
    }

    #[test]
    fn test_parse_overflow() {
        assert_eq!(
            super::parse_rules("overflow: hidden; overflow-y: scroll"),
            Ok((
                "",
                vec![
                    Declaration {
                        data: Decl::Overflow {
                            value: Overflow::Hidden
                        },
                        important: Importance::Default,
                    },
                    Declaration {
                        data: Decl::OverflowY {
                            value: Overflow::Scroll
                        },
                        important: Importance::Default,
                    },
                ]
            ))
        );
    }

    #[test]
    fn test_parse_color() {
        assert_eq!(
            super::parse_rules("color: #123; color: #abcdef"),
            Ok((
                "",
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
                ]
            ))
        );
        assert_eq!(
            super::parse_rules("color: inherit"),
            Ok((
                "",
                vec![Declaration {
                    data: Decl::Unknown {
                        name: PropertyName("color".into()),
                    },
                    important: Importance::Default,
                },]
            ))
        );
    }

    #[test]
    fn test_parse_height() {
        assert_eq!(
            super::parse_rules("height: 0; max-height: 100cm"),
            Ok((
                "",
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
                ]
            ))
        );
    }

    #[test]
    fn test_parse_empty_ss() {
        assert_eq!(super::parse_stylesheet(""), Ok(("", vec![])));
    }

    #[test]
    fn test_parse_ss_col() {
        assert_eq!(
            super::parse_stylesheet(
                "
            foo {
                color: #112233;
            }
            "
            ),
            Ok((
                "",
                vec![RuleSet {
                    selectors: vec![Selector {
                        components: vec![SelectorComponent::Element("foo".into()),],
                        ..Default::default()
                    },],
                    declarations: vec![Declaration {
                        data: Decl::Color {
                            value: Colour::Rgb(0x11, 0x22, 0x33)
                        },
                        important: Importance::Default,
                    },],
                }]
            ))
        );
    }

    #[test]
    fn test_parse_class() {
        assert_eq!(
            super::parse_stylesheet(
                "
            .foo {
                color: #112233;
                background-color: #332211 !important;
            }
            "
            ),
            Ok((
                "",
                vec![RuleSet {
                    selectors: vec![Selector {
                        components: vec![SelectorComponent::Class("foo".into()),],
                        ..Default::default()
                    },],
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
                }]
            ))
        );
    }

    #[test]
    fn test_parse_at_rules() {
        assert_eq!(
            super::parse_stylesheet(
                "
            @media paper {
            }

            @blah asldfkjasfda;

            @nested { lkasjfd alkdsjfa sldkfjas ( alksjdasfd ) [ alskdjfalskdf] }

            @keyframes foo {
                0% { transform: translateY(0); }
               50% { opacity:0.8; }
              100% { }
            }


            .foo {
                color: #112233;
                background-color: #332211 !important;
            }
            "
            ),
            Ok((
                "",
                vec![RuleSet {
                    selectors: vec![Selector {
                        components: vec![SelectorComponent::Class("foo".into()),],
                        ..Default::default()
                    },],
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
                }]
            ))
        );
    }

    #[test]
    fn test_parse_named_colour() {
        assert_eq!(
            super::parse_declaration("color: white"),
            Ok((
                "",
                Some(Declaration {
                    data: Decl::Color {
                        value: Colour::Rgb(0xff, 0xff, 0xff)
                    },
                    important: Importance::Default,
                })
            ))
        );
    }

    #[test]
    fn test_parse_colour_func() {
        assert_eq!(
            super::parse_declaration("color: rgb(1, 2, 3)"),
            Ok((
                "",
                Some(Declaration {
                    data: Decl::Color {
                        value: Colour::Rgb(1, 2, 3)
                    },
                    important: Importance::Default,
                })
            ))
        );
    }

    #[test]
    fn test_parse_multi_selector() {
        assert_eq!(
            super::parse_stylesheet(
                "
.foo a         .foo-bar2 { color: inherit; }
.foo a.foo-bar .foo-bar2 { color: #112233; }
            "
            ),
            Ok((
                "",
                vec![
                    RuleSet {
                        selectors: vec![Selector {
                            components: vec![
                                SelectorComponent::Class("foo-bar2".into()),
                                SelectorComponent::CombDescendant,
                                SelectorComponent::Element("a".into()),
                                SelectorComponent::CombDescendant,
                                SelectorComponent::Class("foo".into()),
                            ],
                            ..Default::default()
                        },],
                        declarations: vec![Declaration {
                            data: Decl::Unknown {
                                name: PropertyName("color".into()),
                            },
                            important: Importance::Default,
                        },],
                    },
                    RuleSet {
                        selectors: vec![Selector {
                            components: vec![
                                SelectorComponent::Class("foo-bar2".into()),
                                SelectorComponent::CombDescendant,
                                SelectorComponent::Class("foo-bar".into()),
                                SelectorComponent::Element("a".into()),
                                SelectorComponent::CombDescendant,
                                SelectorComponent::Class("foo".into()),
                            ],
                            ..Default::default()
                        },],
                        declarations: vec![Declaration {
                            data: Decl::Color {
                                value: Colour::Rgb(0x11, 0x22, 0x33)
                            },
                            important: Importance::Default,
                        },],
                    }
                ]
            ))
        );
    }

    #[test]
    fn test_parse_comma_selector() {
        assert_eq!(
            super::parse_stylesheet(
                "
.foo a, p  { color: #112233; }
            "
            ),
            Ok((
                "",
                vec![RuleSet {
                    selectors: vec![
                        Selector {
                            components: vec![
                                SelectorComponent::Element("a".into()),
                                SelectorComponent::CombDescendant,
                                SelectorComponent::Class("foo".into()),
                            ],
                            ..Default::default()
                        },
                        Selector {
                            components: vec![SelectorComponent::Element("p".into()),],
                            ..Default::default()
                        },
                    ],
                    declarations: vec![Declaration {
                        data: Decl::Color {
                            value: Colour::Rgb(0x11, 0x22, 0x33)
                        },
                        important: Importance::Default,
                    },],
                },]
            ))
        );
    }

    #[test]
    fn test_parse_before_after() {
        assert_eq!(
            super::parse_stylesheet(
                "
.foo a::before, p::after  { color: #112233; }
            "
            ),
            Ok((
                "",
                vec![RuleSet {
                    selectors: vec![
                        Selector {
                            components: vec![
                                SelectorComponent::Element("a".into()),
                                SelectorComponent::CombDescendant,
                                SelectorComponent::Class("foo".into()),
                            ],
                            pseudo_element: Some(PseudoElement::Before),
                        },
                        Selector {
                            components: vec![SelectorComponent::Element("p".into()),],
                            pseudo_element: Some(PseudoElement::After),
                        },
                    ],
                    declarations: vec![Declaration {
                        data: Decl::Color {
                            value: Colour::Rgb(0x11, 0x22, 0x33)
                        },
                        important: Importance::Default,
                    },],
                },]
            ))
        );
    }

    #[test]
    fn test_parse_content() {
        assert_eq!(
            super::parse_stylesheet(
                r#"
.foo a::before, p::after  { content: "blah*#"; }
            "#
            ),
            Ok((
                "",
                vec![RuleSet {
                    selectors: vec![
                        Selector {
                            components: vec![
                                SelectorComponent::Element("a".into()),
                                SelectorComponent::CombDescendant,
                                SelectorComponent::Class("foo".into()),
                            ],
                            pseudo_element: Some(PseudoElement::Before),
                        },
                        Selector {
                            components: vec![SelectorComponent::Element("p".into()),],
                            pseudo_element: Some(PseudoElement::After),
                        },
                    ],
                    declarations: vec![Declaration {
                        data: Decl::Content {
                            text: "blah*#".into()
                        },
                        important: Importance::Default,
                    },],
                },]
            ))
        );
    }

    #[test]
    fn test_background() {
        assert_eq!(
            super::parse_declaration("background: white"),
            Ok((
                "",
                Some(Declaration {
                    data: Decl::BackgroundColor {
                        value: Colour::Rgb(0xff, 0xff, 0xff)
                    },
                    important: Importance::Default,
                })
            ))
        );
        assert_eq!(
            super::parse_declaration("background: url('blah'), white"),
            Ok((
                "",
                Some(Declaration {
                    data: Decl::BackgroundColor {
                        value: Colour::Rgb(0xff, 0xff, 0xff)
                    },
                    important: Importance::Default,
                })
            ))
        );
        assert_eq!(
            super::parse_declaration("background: url('blah'), foo"),
            Ok((
                "",
                Some(Declaration {
                    data: Decl::Unknown {
                        name: PropertyName("background".into()),
                    },
                    important: Importance::Default,
                })
            ))
        );
    }

    #[test]
    fn test_nth_child() {
        use SelectorComponent::NthChild;
        let (_, sel_all) = super::parse_selector("*").unwrap();
        assert_eq!(
            super::parse_selector(":nth-child(even)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 2,
                        b: 0,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(odd)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 2,
                        b: 1,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(17)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 0,
                        b: 17,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(17n)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 17,
                        b: 0,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(10n-1)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 10,
                        b: -1,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(10n+9)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 10,
                        b: 9,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(-n+3)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: -1,
                        b: 3,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(n)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 1,
                        b: 0,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(+n)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: 1,
                        b: 0,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
        assert_eq!(
            super::parse_selector(":nth-child(-n)").unwrap(),
            (
                "",
                Selector {
                    components: vec![NthChild {
                        a: -1,
                        b: 0,
                        sel: sel_all.clone()
                    }],
                    ..Default::default()
                }
            )
        );
    }
}
