//! Resolving relative links against a base URL.
//!
//! Implemented here rather than by depending on the `url` crate: the only
//! thing needed is reference resolution as specified in RFC 3986 §5.3, and
//! this crate keeps its dependency list deliberately small.

/// True when `reference` already carries a scheme (`https:`, `mailto:`, `data:`).
///
/// RFC 3986 §3.1: `scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )`.
fn has_scheme(reference: &str) -> bool {
    let mut chars = reference.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() => {}
        _ => return false,
    }
    for c in chars {
        match c {
            ':' => return true,
            c if c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.' => {}
            _ => return false,
        }
    }
    false
}

/// Split a URL into `(scheme_and_authority, path, query_and_fragment)`.
///
/// Returns `None` when `base` has no `scheme://authority`, in which case there
/// is nothing meaningful to resolve against.
fn split_base(base: &str) -> Option<(&str, &str)> {
    let scheme_end = base.find("://")? + 3;
    let after_authority = base[scheme_end..]
        .find(['/', '?', '#'])
        .map(|i| scheme_end + i)
        .unwrap_or(base.len());
    Some((&base[..after_authority], &base[after_authority..]))
}

/// Remove `.` and `..` segments, per RFC 3986 §5.2.4.
fn remove_dot_segments(path: &str) -> String {
    // The leading slash of an absolute path is held aside rather than being
    // treated as an empty first segment: otherwise a `..` too many pops the
    // root away, and "/../etc" resolves against the origin with no separator.
    // RFC 3986 §5.2.4 discards those instead.
    let (prefix, rest) = match path.strip_prefix('/') {
        Some(rest) => ("/", rest),
        None => ("", path),
    };

    let mut out: Vec<&str> = Vec::new();
    for segment in rest.split('/') {
        match segment {
            "." => {}
            ".." => {
                out.pop();
            }
            s => out.push(s),
        }
    }

    let mut joined = out.join("/");
    // A trailing "." or ".." names a directory, so it leaves a slash behind.
    if (path.ends_with("/.") || path.ends_with("/..")) && !joined.ends_with('/') {
        joined.push('/');
    }
    format!("{prefix}{joined}")
}

/// Resolve `reference` against `base`, returning the reference unchanged when
/// it is already absolute, is empty, or when `base` is not usable.
///
/// Handles the forms that appear in real documents: absolute URLs and non-HTTP
/// schemes are passed through, `//host/path` inherits the base scheme,
/// `/path` replaces the base path, `?q` and `#frag` attach to the base, and
/// relative paths are merged with dot-segment removal.
pub(crate) fn resolve(base: &str, reference: &str) -> String {
    if reference.is_empty() || has_scheme(reference) {
        return reference.to_string();
    }

    // Protocol-relative: keep the base's scheme.
    if let Some(rest) = reference.strip_prefix("//") {
        let scheme = match base.find("://") {
            Some(i) => &base[..i],
            None => return reference.to_string(),
        };
        return format!("{scheme}://{rest}");
    }

    let (origin, base_path) = match split_base(base) {
        Some(parts) => parts,
        None => return reference.to_string(),
    };

    // Query-only and fragment-only references attach to the base path, with
    // anything after the relevant marker dropped (RFC 3986 §5.3).
    if let Some(stripped) = reference.strip_prefix('#') {
        let without_fragment = base_path.split('#').next().unwrap_or("");
        return format!("{origin}{without_fragment}#{stripped}");
    }
    if reference.starts_with('?') {
        let path_only = base_path.split(['?', '#']).next().unwrap_or("");
        return format!("{origin}{path_only}{reference}");
    }

    if reference.starts_with('/') {
        return format!("{origin}{}", remove_dot_segments(reference));
    }

    // Relative path: merge with the base path's directory.
    let path_only = base_path.split(['?', '#']).next().unwrap_or("");
    let directory = match path_only.rfind('/') {
        Some(i) => &path_only[..=i],
        None => "/",
    };
    format!(
        "{origin}{}",
        remove_dot_segments(&format!("{directory}{reference}"))
    )
}

#[cfg(test)]
mod tests {
    use super::resolve;

    const BASE: &str = "https://example.com/a/b/page.html?x=1#top";

    #[test]
    fn absolute_references_are_untouched() {
        for r in [
            "https://other.example/x",
            "http://other.example/x",
            "mailto:someone@example.com",
            "data:text/plain,hello",
            "tel:+6491234567",
            "javascript:void(0)",
        ] {
            assert_eq!(resolve(BASE, r), r, "{r} should be left alone");
        }
    }

    #[test]
    fn root_relative_replaces_the_path() {
        assert_eq!(resolve(BASE, "/login"), "https://example.com/login");
        assert_eq!(resolve(BASE, "/"), "https://example.com/");
    }

    #[test]
    fn relative_paths_merge_with_the_directory() {
        assert_eq!(resolve(BASE, "c.html"), "https://example.com/a/b/c.html");
        assert_eq!(resolve(BASE, "./c.html"), "https://example.com/a/b/c.html");
        assert_eq!(resolve(BASE, "../c.html"), "https://example.com/a/c.html");
        assert_eq!(resolve(BASE, "../../c.html"), "https://example.com/c.html");
    }

    #[test]
    fn protocol_relative_inherits_the_scheme() {
        assert_eq!(
            resolve(BASE, "//cdn.example/x.js"),
            "https://cdn.example/x.js"
        );
        assert_eq!(
            resolve("http://example.com/a", "//cdn.example/x.js"),
            "http://cdn.example/x.js"
        );
    }

    #[test]
    fn fragments_and_queries_attach_to_the_base() {
        assert_eq!(
            resolve(BASE, "#section"),
            "https://example.com/a/b/page.html?x=1#section"
        );
        assert_eq!(
            resolve(BASE, "?y=2"),
            "https://example.com/a/b/page.html?y=2"
        );
    }

    #[test]
    fn an_unusable_base_leaves_the_reference_alone() {
        assert_eq!(resolve("", "/login"), "/login");
        assert_eq!(resolve("not-a-url", "/login"), "/login");
    }

    #[test]
    fn empty_reference_is_left_alone() {
        assert_eq!(resolve(BASE, ""), "");
    }

    #[test]
    fn dot_segments_cannot_escape_the_origin() {
        assert_eq!(resolve(BASE, "/../../etc"), "https://example.com/etc");
    }
}
