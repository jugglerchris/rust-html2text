# Changelog

Possible log types:

- `[added]` for new features.
- `[changed]` for changes in existing functionality.
- `[deprecated]` for once-stable features removed in upcoming releases.
- `[removed]` for deprecated features removed in this release.
- `[fixed]` for any bug fixes.
- `[security]` to invite users to upgrade in case of vulnerabilities.

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
