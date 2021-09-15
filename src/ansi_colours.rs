//! Convenience helper for producing coloured terminal output.
//!
//! This optional helper applies terminal colours (or other effects which
//! can be achieved using inline characters sent to the terminal such as
//! underlining in some terminals).

use crate::{parse, RichAnnotation, RichDecorator};
use std::fmt::Write;
use std::io;

/// Reads HTML from `input`, and returns text wrapped to `width` columns.
/// The text is returned as a `Vec<TaggedLine<_>>`; the annotations are vectors
/// of `RichAnnotation`.  The "outer" annotation comes first in the `Vec`.
///
/// The function `colour_map` is given a slice of `RichAnnotation` and should
/// return a pair of static strings which should be inserted before/after a text
/// span with that annotation; for example a string which sets text colour
/// and a string which sets the colour back to the default.
pub fn from_read_coloured<R, FMap>(
    input: R,
    width: usize,
    colour_map: FMap,
) -> Result<String, std::fmt::Error>
where
    R: io::Read,
    FMap: Fn(&RichAnnotation) -> (String, String),
{
    let lines = parse(input)
        .render(width, RichDecorator::new())
        .into_lines();

    let mut result = String::new();
    for line in lines {
        for ts in line.tagged_strings() {
            let mut start = String::new();
            let mut finish = String::new();
            for ann in &ts.tag {
                let (s, f) = colour_map(ann);
                start.push_str(&s);
                finish.push_str(&f);
            }
            write!(result, "{}{}{}", start, ts.s, finish)?;
        }
        result.push('\n');
    }
    Ok(result)
}
