//! Core browser state.
//!

/// The browser context.
pub(crate) struct Browser {
    location: Option<String>,
}

impl Browser {
    /// Create a new browser open at a page.
    pub(crate) fn new_with_url(url: impl Into<String>) -> Self {
        Browser {
            location: Some(url.into()),
        }
    }

    /// Open a new browser with no current location.
    pub(crate) fn new() -> Self {
        Browser {
            location: None,
        }
    }
}
