//! Parsing for the subset of CSS used in html2text.

use nom::{IResult, branch::alt, character::complete, bytes::complete::{tag, take_until, take_while}, combinator::{map, fail, opt}, multi::many0, error::ErrorKind, sequence::tuple};

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Unknown {
        name: PropertyName,
        value: String,
    },
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
        None => IResult::Err(nom::Err::Incomplete(nom::Needed::Size(1.try_into().unwrap())))
    }
}

fn nmchar_char(s: &str) -> IResult<&str, char> {
    let mut iter = s.chars();
    match iter.next() {
        Some(c) => {
            match c {
                '_' | 'a'..='z' | 'A' ..= 'Z' | '0' ..= '9' => {
                    Ok((iter.as_str(), c.to_ascii_lowercase()))
                }
                _ => {
                    IResult::Err(nom::Err::Error(nom::error::Error::new(s, ErrorKind::Fail)))
                }
            }
        }
        None => IResult::Err(nom::Err::Incomplete(nom::Needed::Size(1.try_into().unwrap())))
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

fn parse_property_name(text: &str) -> IResult<&str, PropertyName> {
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
    Ok((rest, PropertyName(name.into_iter().collect())))
}

fn parse_value(text: &str) -> IResult<&str, &str> {
    take_while(|c| c != ';')(text)
}

pub fn parse_declaration(text: &str) -> IResult<&str, Option<Declaration>> {
    let (rest, (prop, _ws1, _colon, _ws2, value)) =
        tuple((
            parse_property_name,
            skip_optional_whitespace,
            tag(":"),
            skip_optional_whitespace,
            parse_value))(text)?;
    Ok((rest, Some(Declaration::Unknown {
        name: prop,
        value: value.into(),
    })))
}

#[cfg(test)]
mod test {
    use super::{Declaration, PropertyName};
    #[test]
    fn test_parse_decl() {
        assert_eq!(super::parse_declaration("foo:bar;"), Ok((";", Some(Declaration::Unknown {
            name: PropertyName("foo".into()),
            value: "bar".into()
        }))));
    }
}
