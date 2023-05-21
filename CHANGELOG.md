# Changelog

Possible log types:

- `[added]` for new features.
- `[changed]` for changes in existing functionality.
- `[deprecated]` for once-stable features removed in upcoming releases.
- `[removed]` for deprecated features removed in this release.
- `[fixed]` for any bug fixes.
- `[security]` to invite users to upgrade in case of vulnerabilities.

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
