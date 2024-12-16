//! Convenience helper for producing coloured terminal output.
//!
//! This optional helper applies terminal colours (or other effects which
//! can be achieved using inline characters sent to the terminal such as
//! underlining in some terminals).

use crate::RichAnnotation;
use std::io;

/// Reads HTML from `input`, and returns text wrapped to `width` columns.
///
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
) -> Result<String, crate::Error>
where
    R: io::Read,
    FMap: Fn(&[RichAnnotation], &str) -> String,
{
    super::config::rich().coloured(input, width, colour_map)
}
