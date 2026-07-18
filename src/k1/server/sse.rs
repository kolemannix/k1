//! The datastar SSE events; payloads write directly into a caller-owned
//! output buffer.

use std::fmt;

pub const SSE_HEADERS: &str =
    "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\n\r\n";

pub fn patch_elements(out: &mut String, elements: impl fmt::Display) {
    event(out, "datastar-patch-elements", "elements", elements);
}

pub fn patch_signals(out: &mut String, signals: impl fmt::Display) {
    event(out, "datastar-patch-signals", "signals", signals);
}

/// Appends one complete SSE event, framing each line of `payload` as
/// `data: {data_prefix} {line}`
pub fn event(out: &mut String, event_name: &str, data_prefix: &str, payload: impl fmt::Display) {
    use fmt::Write;
    out.push_str("event: ");
    out.push_str(event_name);
    out.push('\n');
    let mut framer = DataFramer { out, prefix: data_prefix, at_line_start: true };
    write!(framer, "{payload}").expect("writing to a String cannot fail");
    let needs_newline = !framer.at_line_start;
    if needs_newline {
        out.push('\n');
    }
    // Blank line terminates the event
    out.push('\n');
}

/// A fmt sink that begins every line it forwards with `data: {prefix} `
struct DataFramer<'a> {
    out: &'a mut String,
    prefix: &'a str,
    at_line_start: bool,
}

impl fmt::Write for DataFramer<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for chunk in s.split_inclusive('\n') {
            if self.at_line_start && !chunk.is_empty() {
                self.out.push_str("data: ");
                self.out.push_str(self.prefix);
                self.out.push(' ');
            }
            self.out.push_str(chunk);
            self.at_line_start = chunk.ends_with('\n');
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frames_multiline_elements() {
        let mut out = String::new();
        patch_elements(&mut out, "<div id=\"x\">\n  hi\n</div>");
        assert_eq!(
            out,
            "event: datastar-patch-elements\n\
             data: elements <div id=\"x\">\n\
             data: elements   hi\n\
             data: elements </div>\n\
             \n"
        );
    }

    #[test]
    fn frames_signals_object() {
        let mut out = String::new();
        patch_signals(&mut out, "{\"cells_0_tab\": \"output\"}");
        assert_eq!(
            out,
            "event: datastar-patch-signals\n\
             data: signals {\"cells_0_tab\": \"output\"}\n\
             \n"
        );
    }

    #[test]
    fn payload_formatted_in_fragments_still_frames_correctly() {
        // framing must depend on newlines, not write_str chunk boundaries
        let mut out = String::new();
        event(
            &mut out,
            "e",
            "p",
            format_args!("a{}\n{}c", "b", ""),
        );
        assert_eq!(out, "event: e\ndata: p ab\ndata: p c\n\n");
    }

    #[test]
    fn trailing_newline_payload_gets_no_empty_data_line() {
        let mut out = String::new();
        event(&mut out, "e", "p", "line\n");
        assert_eq!(out, "event: e\ndata: p line\n\n");
    }
}
