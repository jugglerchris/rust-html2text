//! Core browser state.

use async_stream::stream;
use std::sync::Arc;

use html2text::{
    expanded_name, local_name, ns, Element,
    render::{RichDecorator, TaggedLine, TextDecorator},
};
use tracing::info;

pub type RenderedText = Vec<TaggedLine<Vec<<RichDecorator as TextDecorator>::Annotation>>>;

#[derive(Default)]
/// The browser context.
struct Inner {
    location: Option<String>,
    doc_string: Option<String>,
    dom: Option<html2text::RcDom>,
    client: reqwest::Client,
    evt_listeners: Vec<tokio::sync::mpsc::Sender<Event>>,
    stylesheet_urls: Vec<String>,
}

impl Inner {
    pub fn new() -> Self {
        Inner {
            location: None,
            doc_string: None,
            dom: None,
            client: reqwest::Client::new(),
            evt_listeners: Vec::new(),
            stylesheet_urls: Vec::new(),
        }
    }

    async fn set_error(&mut self, err: String) {
        self.doc_string = Some(err);
        self.emit(Event::DocUpdated).await;
    }
    // Todo: content types, streaming body, etc.
    async fn set_content(&mut self, data: String) {
        let dom = html2text::config::plain()
            .parse_html(data.as_bytes())
            .ok();
        self.doc_string = Some(data);
        self.dom = dom;
        self.stylesheet_urls = vec![];

        if let Some(dom) = self.dom.as_ref() {
            let html = dom.document.get_children_by_name("html").pop();
            if let Some(html) = html {
                let head = html.get_children_by_name("head");
                if let Some(head) = head.first() {
                    self.parse_head(head);
                }
            }
        }
        self.emit(Event::DocUpdated).await;
    }

    async fn emit(&self, event: Event) {
        for sender in &self.evt_listeners {
            let _ = sender.send(event).await;
        }
    }

    fn parse_head(&mut self, head: &html2text::Handle) {
        info!("Parsing head");
        for child in head.children.borrow().iter() {
            match child.data {
                Element {
                    ref name,
                    ref attrs,
                    ..
                } => {
                    match name.expanded() {
                        expanded_name!(html "link") => {
                            info!("Found link element");
                            // Is it a stylesheet?
                            let attrs = attrs.borrow();
                            let mut attr_rel = None;
                            let mut attr_href = None;
                            for attr in attrs.iter() {
                                if &attr.name.local == "rel" {
                                    attr_rel = Some(&*attr.value);
                                } else if &attr.name.local == "href" {
                                    attr_href = Some(&*attr.value);
                                }
                            }
                            match (attr_rel, attr_href) {
                                (Some("stylesheet"), Some(href)) => {
                                    info!("Found stylesheet URL {href}");
                                    self.stylesheet_urls.push(href.into());
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
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

    pub(crate) async fn events(&mut self) -> impl futures::Stream<Item = Event> + use<> {
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
                Ok(res) => match res.text().await {
                    Err(e) => {
                        inner
                            .lock()
                            .await
                            .set_error(format!("Error on text(): {e}"))
                            .await;
                    }
                    Ok(text) => {
                        inner.lock().await.set_content(text).await;
                    }
                },
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
