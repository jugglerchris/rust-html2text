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

use crate::{
    Browser,
    Term,
};

struct HtmlView {
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

