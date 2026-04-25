//! Argument parsing.

use clap::Parser;

#[derive(Parser)]
#[command(version)]
pub struct Cli {
    /// URL to load
    pub url: Option<String>,
}
