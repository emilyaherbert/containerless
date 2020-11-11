//! An implementation of a logger that sends logged messages over the network
//! to the log-echo-agent.

use std::collections::HashMap;
use tokio::task;
use futures::channel::mpsc;
use futures::prelude::*;
use log::{Level, Log, Metadata, Record};
// The Client type has a generated method to post logs. The rest of the types below are boilerplate
// that we need to use Client.
use log_openapi::{ApiNoContext, Client, ContextWrapperExt};
use swagger::{AuthData, EmptyContext, Push, XSpanIdString};

struct Logger {
    /// We display all logs at this level or lower. Note that `Level::Error`
    /// is the lowest level, and `Level::Trace` is the highest level.
    enabled_level: Level,
    /// We send log messages on this channel to a background task that then
    /// sends them to the server asynchronously.
    send_string: mpsc::Sender<String>,
}

impl Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        return metadata.level() <= self.enabled_level;
    }

    fn log(&self, record: &Record) {
        let log_str = format!("{}", record.args());
        let mut send_string = self.send_string.clone();
        task::spawn(async move {
            if let Err(_err) = send_string.send(log_str).await {
                eprintln!("Failed to send log message to logging task");
            }
        });
    }

    fn flush(&self) {}
}

/// Creates a logger that sends log messages to http://localhost/controller-logger.
/// It uses the environment variable `RUST_LOG` to determine the log level.
pub fn init(max_reported_errors: usize) {
    // We use a single task (below) to send log messages, but multiple tasks
    // may concurrently send log messages to the logger.
    let (send_string, mut recv_string) = mpsc::channel(1);

    // Task that sends log messages in a loop.
    task::spawn(async move {
        let client = Client::try_new_http("http://localhost/controller-logger")
            .expect("error creating HTTP client for logging")
            .with_context(swagger::make_context!(
                ContextBuilder,
                EmptyContext,
                None as Option<AuthData>,
                XSpanIdString::default()
            ));

        // Counts the number of log messages that we have failed to send in a row.
        let mut err_count = 0;

        while let Some(log_msg) = recv_string.next().await {
            if let Err(err) = client.log_post(log_msg).await {
                if err_count < max_reported_errors {
                    eprintln!("Failed to send log message. Send error was {}", err);
                }
                err_count += 1;
            } else if err_count > 0 {
                eprintln!(
                    "Successfully delivered a log message ({} failures occurred, only {} were shown here)",
                    err_count,
                    max_reported_errors);
                err_count = 0;
            }
        }
    });

    let env: HashMap<String, String> = std::env::vars().collect();

    // Try to parse RUST_LOG. If not set, use Info. Panic if set and parsing fails.
    let enabled_level: Level = {
        if let Some(level_str) = env.get("RUST_LOG") {
            level_str
                .parse()
                .expect("RUST_LOG environment variable set to bogus value")
        } else {
            eprintln!("RUST_LOG environment variable not set. Using INFO to log.");
            Level::Info
        }
    };

    // NOTE(arjun) Reading the code of env_logger, I realized that this line is necessary, even
    // though the Log trait implementation above filters by level. It appears that the level filter
    // is applied twice, once with log::set_max_level and again in the Log::enabled method above.
    log::set_max_level(enabled_level.to_level_filter());
    let logger = Logger {
        enabled_level,
        send_string,
    };
    log::set_boxed_logger(Box::new(logger)).expect("failed to set logger");
}
