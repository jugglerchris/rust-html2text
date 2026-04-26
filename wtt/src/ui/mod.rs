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
use ratatui::{
    buffer::Buffer,
    layout::{
        Rect,
    },
    widgets::{
        Clear,
        StatefulWidget,
    },
};

use crate::{
    Browser,
    Term,
};

struct HtmlView {
}

struct HtmlWidget { }

impl StatefulWidget for HtmlWidget {
    type State = HtmlView;
    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        buf.set_string(area.left(), area.top(), "hello world", ratatui::style::Style::default());
    }
}

struct UrlBar {
}

/// All kinds of event which can happen.
enum Event {
    /// Terminal event (e.g. key press)
    Term(crossterm::event::Event),
}

/// Overall UI state
struct UI {
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
    fn new() -> UI {
        UI {
            main_view: HtmlView {},
        }
    }

    /// Returns true if
    fn handle_key(&mut self, kevt: crossterm::event::KeyEvent) -> EventEffect {
        use crossterm::event::KeyCode;
        if kevt.code == KeyCode::Char('q') {
            EventEffect::Quit
        } else {
            EventEffect::Update
        }
    }
}

/// Run the terminal browser UI
pub async fn run_browser(terminal: &mut Term, browser: &mut Browser) -> Result<(), Box<dyn Error>> {
    let mut term_events = EventStream::new();
    let mut ui = UI::new();

    let (evt_sender, mut evt_recv) = tokio::sync::mpsc::channel(20);
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
        }
    }
    Ok(())
}

