# Changelog

Possible log types:

- `[added]` for new features.
- `[changed]` for changes in existing functionality.
- `[deprecated]` for once-stable features removed in upcoming releases.
- `[removed]` for deprecated features removed in this release.
- `[fixed]` for any bug fixes.
- `[security]` to invite users to upgrade in case of vulnerabilities.

### Unreleased

### 0.1.13 (2020-07-21)

- [changed] Run cargo fmt (thanks crunchyjesus)
- [added] CHANGELOG.md
- [fixed] Some text near a fragment start (`id="foo"` attribute) could be
  lost if it needed breaking across lines.
- [added] Experimentally add dependabot configuration.
