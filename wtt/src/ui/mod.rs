//! The UI widgets.

use std::error::Error;
use crossterm::{
    event::{
        EventStream,
    }
};
use futures::{
    StreamExt,
};
use html2text::render::{
    TaggedLineElement,
};
use ratatui::{
    buffer::Buffer,
    layout::{
        Rect,
    },
    style::{
        Modifier,
        Style,
    },
    widgets::{
        StatefulWidget,
    },
};

use crate::{
    browser,
    Browser,
    Term,
};

enum HtmlState {
    Empty,
    Loading,
    Rendered,
}

struct HtmlView {
    state: HtmlState,
    body: browser::RenderedText,
}

struct HtmlWidget { }

impl StatefulWidget for HtmlWidget {
    type State = HtmlView;
    fn render(self, area: Rect, buf: &mut Buffer, view: &mut Self::State) {
        match view.state {
            HtmlState::Empty => {
                buf.set_string(area.left(), area.top(), "No document", Style::default());
            }
            HtmlState::Loading => {
                buf.set_string(area.left(), area.top(), "Loading...", Style::default());
            }
            HtmlState::Rendered => {
                let mut y = area.top();
                for line in &view.body {
                    if y > area.bottom() {
                        break;
                    }
                    let mut x = area.left();

                    for tle in line.iter() {
                        use TaggedLineElement::*;
                        match tle {
                            Str(ts) => {
                                let mut style = Style::default();
                                for ann in &ts.tag {
                                    use html2text::render::RichAnnotation::*;
                                    match ann {
                                        Default => {}
                                        Link(..) => {}
                                        Image(..) => {}
                                        Emphasis => {}
                                        Strong => {
                                            style = style.add_modifier(Modifier::BOLD);
                                        }
                                        Strikeout => {}
                                        Code => {}
                                        Preformat(..) => {}
                                        Colour(..) => {}
                                        BgColour(..) => {}
                                        _ => {}
                                    }
                                }
                                buf.set_string(x, y, &ts.s, style);
                                x += ts.width() as u16;
                            }
                            FragmentStart(..) => {}
                        }
                    }
                    y += 1;
                }
            }
        }
    }
}

struct UrlBar {
}

#[derive(Debug)]
/// All kinds of event which can happen.
enum Event {
    /// Terminal event (e.g. key press)
    Term(crossterm::event::Event),
    /// Browser events
    Browser(crate::browser::Event),
}

/// Overall UI state
struct UI {
    browser: Browser,
    main_view: HtmlView,
}

enum EventEffect {
    /// No change to UI
    Nothing,
    /// Display needs updating
    Update,
    /// Quit the application
    Quit,
}

impl UI {
    fn new(browser: Browser) -> UI {
        UI {
            browser,
            main_view: HtmlView {
                state: HtmlState::Empty,
                body: Default::default(),
            },
        }
    }

    fn handle_key(&mut self, kevt: crossterm::event::KeyEvent) -> EventEffect {
        use crossterm::event::KeyCode;
        if kevt.code == KeyCode::Char('q') {
            EventEffect::Quit
        } else {
            EventEffect::Update
        }
    }

    async fn handle_browser_event(&mut self, bevt: browser::Event) -> EventEffect {
        use browser::Event::*;
        match bevt {
            DocUpdated => {
                let body = self.browser
                    .render_body(80)
                    .await
                    .unwrap_or_else(|_| vec![]);
                self.main_view.body = body;
                self.main_view.state = HtmlState::Rendered;
                EventEffect::Update
            }
        }
    }

    async fn set_location(&mut self, url: String) {
        self.browser.navigate_to(url).await;
        self.main_view.state = HtmlState::Loading;
    }
}

/// Run the terminal browser UI
pub async fn run_browser(terminal: &mut Term, browser: Browser, url: Option<String>) -> Result<(), Box<dyn Error>> {
    let mut term_events = EventStream::new();
    let mut ui = UI::new(browser);
    let (evt_sender, mut evt_recv) = tokio::sync::mpsc::channel(20);

    {
        let sender = evt_sender.clone();
        let mut stream = Box::pin(ui.browser.events().await);

        tokio::task::spawn_local(async move {
            while let Some(evt) = stream.next().await { 
                sender.send(Event::Browser(evt)).await.unwrap();
            }
        });
    }

    tokio::task::spawn_local(async move {
        while let Some(r_evt) = term_events.next().await {
            match r_evt {
                Ok(evt) => {
                    let _ = evt_sender.send(Event::Term(evt)).await;
                },
                Err(e) => panic!("Error: {e}"),
            }
        }
    });

    if let Some(url) = url {
        ui.set_location(url).await;
    }

    let mut needs_draw = true;
    loop {
        if needs_draw {
            terminal.terminal.draw(|frame| {
                let view_area = Rect::new(0, 0, frame.area().width, frame.area().height - 1);
                frame.render_stateful_widget(HtmlWidget{}, view_area, &mut ui.main_view);
            })?;
            needs_draw = false;
        }
        match evt_recv.recv().await {
            None => {
                // Event stream closed
                break;
            }
            Some(Event::Term(evt)) => {
                use crossterm::event::Event::*;
                match evt {
                    Key(kevt) => match ui.handle_key(kevt) {
                        EventEffect::Nothing => (),
                        EventEffect::Update => {
                            needs_draw = true;
                        }
                        EventEffect::Quit => {
                            break;
                        }
                    },
                    _ => ()
                }
            }
            Some(Event::Browser(b_evt)) => {
                match ui.handle_browser_event(b_evt).await {
                    EventEffect::Nothing => (),
                    EventEffect::Update => {
                        needs_draw = true;
                    }
                    EventEffect::Quit => {
                        break;
                    }
                }
            }
        }
    }
    Ok(())
}

