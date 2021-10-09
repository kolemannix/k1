use std::fmt;
use std::fmt::Formatter;

use LogLevel::*;

#[derive(Clone, Copy)]
pub enum LogLevel {
    Error,
    Normal,
    Verbose,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Error =>   "error  ",
            Normal =>  "normal ",
            Verbose => "verbose"
        };
        f.write_str(s)
    }
}

pub fn normal(message: &str) -> () {
    log(message, LogLevel::Normal);
}

pub fn verbose(message: &str) -> () {
    log(message, LogLevel::Verbose);
}

pub fn error(message: &str) -> () {
    log(message, LogLevel::Error);
}

pub fn log(message: &str, level: LogLevel) -> () {
    println!("[{}] {}", level, message);
}