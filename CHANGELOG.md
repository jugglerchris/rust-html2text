# Changelog

Possible log types:

- `[added]` for new features.
- `[changed]` for changes in existing functionality.
- `[deprecated]` for once-stable features removed in upcoming releases.
- `[removed]` for deprecated features removed in this release.
- `[fixed]` for any bug fixes.
- `[security]` to invite users to upgrade in case of vulnerabilities.

### 0.11.0

- [fixed] CSS: rules marked !important were ignored.
- [changed] html\_trace feature now uses the `log` crate.
- [changed] Bumped MSRV to 1.63 (matching Debian stable) due to some dependencies.

### 0.10.3

- [fixed] A panic on some unlucky text wrapping coincidences.
- [fixed] Use dep:backtrace in Cargo.toml to avoid implicit feature.

### 0.10.2

- [fixed] CSS: Ignore transparent colours.

### 0.10.1

- [fixed] `max_width` was not working with some render methods.

### 0.10.0

- [added] Simple support for `<i>`, `<ins>`, and `<del>` (thanks sgtatham)
- [added] Added background-color support
- [fixed] CSS support didn't work in some places, such as `<td>` elements.
- [added] Add support for `style` attributes.
- [added] Styles apply to table borders
- [changed] Update some dependencies
- [fixed] Fix a few places which caused excess blank lines or empty tables

### 0.9.4

- [changed] Updated the termion dev-dependency to 2.0.

### 0.9.3

- [changed] Added cargo categories and update to 2021 edition.

### 0.9.2

- [fixed] CSS didn't work inside `<ul>` or `<ol>`.
- [added] Add methods to get and use the intermediate HTML DOM and RenderTree
  from Config.
- [fixed] Removed some clones which are no longer necessary now that Box<FnOnce>
  works.

### 0.9.1

- [fixed] Various documentation issues (thanks sgtatham)
- [changed] CSS color rules now work for elements other than span.

### 0.9.0

- [changed] `Config::add_css` now returns `Result` instead of panicking on
  CSS parse errors.  Errors from parsing document CSS are ignored.
- [added] Support `<font color=...>` when CSS is enabled.
- [added] `Config::max_wrap_width()` to wrap text to a norrower width than
  the overal size available.
- [added] Add --wrap-width and --css options to html2text example.

### 0.8.0

- [added] CSS: Support more extensive selectors
- [changed] CSS handling defaults to off; use `Config::use_doc_css()`
  or `Config::add_css` to use CSS.

### 0.7.1

- [added] Now recognised CSS `display:none`
- [added] Can now add extra CSS rules via `Config::add_css`.
- [changed] StyleData::coloured is no longer public.

### 0.7.0

- [changed] Remove some noisy stderr output when encoutering control chars
  (thanks sftse)
- [added] A builder-based config API.
- [changed] Updated MSRV to 1.60
- [fixed] Fixed #88: panic when a width of zero passed in (thanks bingen13)
- [fixed] Fixed #90: Fixed a divide-by-zero panic with colspan=0 (thanks mtorromeo)
- [added] Add very basic CSS colour support (under the css feature flag)
- [changed] Removed ansi\_colours feature (from\_read\_coloured is always available)
- [changed] Overhauled error handling.  Internally (and in the lower level
  API) errors (mainly "TooNarrow") are passed around with `Result`.  Fixed
  some panics and infinite loops.  (Thanks WIZeaz for fuzzing)

### 0.6.0

- [changed] Improve layout of tables thanks to sftse:
  - Table column size estimates have been improved when the source HTML has a lot
    of unnecessary whitespace.
  - Move the URL footnotes out to the top level, also improving layout of tables
    containing links.
- [changed] Some APIs have slightly changed as part of the table improvements,
  though most users should not be affeted.

### 0.5.1

- [fixed] Some tables were rendered too wide.

### 0.5.0

- [changed] Rich Image annotations now include the src attirbute (thanks spencerwi).

### 0.4.5

- [fixed] Preserve empty lines in pre blocks (thanks kpagacz).

### 0.4.4

- [fixed] Fix some panics when enumerated lists are in tables (thanks sfts).
- [fixed] Impove table size estimation to include links.

### 0.4.3

- [changed] MSRV is now 1.56.
- [fixed] Fix some panics when very large widths are used with tables.

### 0.4.2

- [changed] Moved the rcdom module directly into src/

### 0.4.1 (unpublished)

- [changed] rcdom now vendored as a module.

### 0.4.0 (unpublished)

- [changed] Update html5ever to v0.26.
- [changed] MSRV is now 1.49.

### 0.3.1

- [changed] Update the build badges to reflect the updated CI configuration.

### 0.3.0

- [added] New experimental `from_read_coloured()` (under `ansi_colours` feature).
- [added] Add `into_tagged_strings` and `tagged_strings` methods to `TaggedLine`
  (thanks Robin Krahl)
- [added] Add `width` method to `TaggedString` (thanks Robin Krahl)
- [changed] Keep annotations in `TextRenderer::into_lines` (thanks Robin Krahl)
- [fixed] Add colon to reference style link (thanks zakaluka)
- [added] Allow text decorators to customise block prefix strings (thanks SardineFish)
- [fixed] Fixed some problems rendering some complicated tables, including a panic
  and near-infinite loops.
- [changed] Tables which are too wide to possibly render in the given width are now
  arranged vertically instead (with `///`) lines.
- [changed] A number of small table rendering improvements.
- [changed] MSRV is now 1.41.

### 0.2.1

- [added] New entry points - split HTML parsing from rendering the output,
  thanks Robin Krahl.
- [fixed] Decorators weren't being used for preformatted text.

### 0.2.0

- [added] Support `<s>` strikeout text.

### 0.1.14 (2020-08-07)

- [fixed] A table with an `id` attribute on `<tbody>` would be hidden.

### 0.1.13 (2020-07-21)

- [changed] Run cargo fmt (thanks crunchyjesus)
- [added] CHANGELOG.md
- [fixed] Some text near a fragment start (`id="foo"` attribute) could be
  lost if it needed breaking across lines.
- [added] Experimentally add dependabot configuration.
