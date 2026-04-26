mod args;
mod browser;
mod ui;

use std::error::Error;
use clap::Parser;
use crossterm::{
    execute,
    terminal::{
        disable_raw_mode,
        enable_raw_mode,
        EnterAlternateScreen,
        LeaveAlternateScreen,
    },
};
use ratatui::prelude::{
    Backend,
    CrosstermBackend,
    Terminal,
};

use args::Cli;
use browser::Browser;
use ui::run_browser;

/// Manage the terminal including restoring it.
struct Term {
    pub terminal: Terminal<CrosstermBackend<std::io::Stdout>>,
}

impl Term {
    pub fn new() -> Result<Term, Box<dyn Error>> {
        enable_raw_mode()?;
        let mut stdout = std::io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;
        terminal.clear()?;
        Ok(Term {
            terminal,
        })
    }
}

impl Drop for Term {
    fn drop(&mut self) {
        let _ = execute!(std::io::stdout(), LeaveAlternateScreen);
        disable_raw_mode().unwrap();
    }
}

#[tokio::main(flavor = "local")]
async fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    let mut browser = if let Some(url) = cli.url {
        Browser::new_with_url(url)
    } else {
        Browser::new()
    };

    let mut terminal = Term::new()?;

    run_browser(&mut terminal, browser).await
}


