//! Core browser state.

use std::sync::Arc;
use async_stream::stream;

use html2text::{
    render::{
        RichDecorator,
        TaggedLine,
        TextDecorator,
    },
};

pub type RenderedText = Vec<TaggedLine<Vec<<RichDecorator as TextDecorator>::Annotation>>>;

#[derive(Default)]
/// The browser context.
struct Inner {
    location: Option<String>,
    doc_string: Option<String>,
    client: reqwest::Client,
    evt_listeners: Vec<tokio::sync::mpsc::Sender<Event>>,
}

impl Inner {
    pub fn new() -> Self {
        Inner {
            location: None,
            doc_string: None,
            client: reqwest::Client::new(),
            evt_listeners: Vec::new(),
        }
    }

    async fn set_error(&mut self, err: String) {
        self.doc_string = Some(err);
        self.emit(Event::DocUpdated).await;
    }
    // Todo: content types, streaming body, etc.
    async fn set_body(&mut self, data: String) {
        self.doc_string = Some(data);
        self.emit(Event::DocUpdated).await;
    }

    async fn emit(&self, event: Event) {
        for sender in &self.evt_listeners {
            sender.send(event).await;
        }
    }
}

pub(crate) struct Browser {
    inner: Arc<tokio::sync::Mutex<Inner>>,
}

#[derive(Copy, Clone, Debug)]
pub enum Event {
    DocUpdated,
}

pub enum BrowserError {
    Html2text(html2text::Error),
}

impl Browser {
    /// Open a new browser with no current location.
    pub(crate) fn new() -> Self {
        Browser {
            inner: Arc::new(tokio::sync::Mutex::new(Inner::new())),
        }
    }

    pub(crate) async fn events(&mut self) -> impl futures::Stream<Item = Event> + use<>
    {
        let (sender, mut receiver) = tokio::sync::mpsc::channel(1);
        let mut inner = self.inner.lock().await;
        inner.evt_listeners.push(sender);

        stream! {
            while let Some(evt) = receiver.recv().await {
                yield evt;
            }
        }
    }

    /// Run the fetching task
    pub(crate) async fn navigate_to(&self, url: impl Into<String>) {
        let location = url.into();
        let client;
        {
            let mut inner = self.inner.lock().await;
            inner.location = Some(location.clone());
            inner.doc_string = None;
            client = inner.client.clone();
        }
        let inner = Arc::clone(&self.inner);
        tokio::task::spawn_local(async move {
            match client.get(location).send().await {
                Err(e) => {
                    inner.lock().await.set_error(format!("Error: {e}")).await;
                }
                Ok(res) => {
                    match res.text().await {
                        Err(e) => {
                            inner.lock().await.set_error(format!("Error on text(): {e}")).await;
                        }
                        Ok(text) => {
                            inner.lock().await.set_body(text).await;
                        }
                    }
                }
            }
        });
    }

    /// Render the HTML
    pub(crate) async fn render_body(&self, width: usize) -> Result<RenderedText, BrowserError> {
        let inner = self.inner.lock().await;
        let source = inner.doc_string.as_deref().unwrap_or("Empty document");
        html2text::config::rich()
            .lines_from_read(source.as_bytes(), width)
            .map_err(BrowserError::Html2text)
    }
}
