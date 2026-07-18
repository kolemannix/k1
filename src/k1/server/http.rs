//! TCP transport: connection threads, request parsing, response writing, and
//! the /events connection.

use super::{bus, sse, SharedProgram};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use std::sync::Arc;
use std::time::Duration;

pub enum Response {
    Html(String),
    Text(String),
    /// The request was a command; its effects were published on the event bus
    NoContent,
    BadRequest,
    NotFound,
    /// No compiled program behind the server (yet)
    Unavailable,
}

/// One thread per connection; the compiler is locked per request, so the
/// long-lived /events connections never block it.
pub fn serve(k1: SharedProgram) {
    let listener = match TcpListener::bind("127.0.0.1:8080") {
        Ok(listener) => listener,
        Err(e) => {
            eprintln!("megarepl server could not bind 127.0.0.1:8080: {e}");
            return;
        }
    };
    eprintln!("k1 server running on http://127.0.0.1:8080");
    let bus = Arc::new(bus::EventBus::new());
    {
        let bus = bus.clone();
        std::thread::spawn(move || super::css_watch_publisher(&bus));
    }
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let k1 = k1.clone();
                let bus = bus.clone();
                std::thread::spawn(move || handle_client(&k1, &bus, stream));
            }
            Err(e) => eprintln!("Connection failed: {}", e),
        }
    }
}

fn handle_client(k1: &SharedProgram, bus: &bus::EventBus, mut stream: TcpStream) {
    let mut buffer = Vec::new();
    let mut chunk = [0u8; 4096];

    loop {
        // Parse one request from the front of the buffer, reading as needed
        let (method, path, header_len, content_length) = loop {
            let mut headers = [httparse::EMPTY_HEADER; 64];
            let mut req = httparse::Request::new(&mut headers);
            match req.parse(&buffer) {
                Ok(httparse::Status::Complete(header_len)) => {
                    let content_length = req
                        .headers
                        .iter()
                        .find(|h| h.name.eq_ignore_ascii_case("content-length"))
                        .and_then(|h| std::str::from_utf8(h.value).ok())
                        .and_then(|v| v.trim().parse::<usize>().ok())
                        .unwrap_or(0);
                    break (
                        req.method.unwrap().to_string(),
                        req.path.unwrap().to_string(),
                        header_len,
                        content_length,
                    );
                }
                Ok(httparse::Status::Partial) => {} // fall through and read more
                Err(e) => {
                    eprintln!("bad request: {e}");
                    return;
                }
            }
            let Ok(n) = stream.read(&mut chunk) else {
                return;
            };
            if n == 0 {
                return; // client closed the connection
            }
            buffer.extend_from_slice(&chunk[..n]);
        };

        // Headers are complete; make sure the whole body has arrived too
        while buffer.len() < header_len + content_length {
            let Ok(n) = stream.read(&mut chunk) else {
                return;
            };
            if n == 0 {
                return;
            }
            buffer.extend_from_slice(&chunk[..n]);
        }
        let body = &buffer[header_len..header_len + content_length];

        // SSE routes take over the connection and never take the compiler lock
        if path.split('?').next().unwrap() == "/events" {
            return events_stream(bus.subscribe(), stream);
        }

        let response = {
            // A panicked request poisons the lock; keep serving anyway — losing
            // the repl session to one bad cell is worse than any inconsistency
            let mut guard = k1.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
            match guard.as_deref_mut() {
                Some(k1) => super::handle_request(k1, bus, &method, &path, body),
                None => Response::Unavailable,
            }
        };
        write_response(&mut stream, &response);

        // Consume this request so the next one starts at the front
        buffer.drain(..header_len + content_length);
    }
}

fn write_response(stream: &mut TcpStream, response: &Response) {
    let (status, content_type, body) = match response {
        Response::Html(body) => ("200 OK", "text/html", body.as_str()),
        Response::Text(body) => ("200 OK", "text/plain", body.as_str()),
        Response::NoContent => ("204 NO CONTENT", "text/plain", ""),
        Response::BadRequest => ("400 BAD REQUEST", "text/plain", ""),
        Response::NotFound => ("404 NOT FOUND", "text/plain", ""),
        Response::Unavailable => (
            "503 SERVICE UNAVAILABLE",
            "text/plain",
            "no compiled program yet; reload after the next successful compile",
        ),
    };
    let result = write!(
        stream,
        "HTTP/1.1 {status}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\n\r\n{body}",
        body.len()
    );
    if let Err(e) = result {
        eprintln!("tcp write error: {e}")
    }
}

/// The per-tab event stream: everything published on the bus goes down this
/// connection until the client hangs up.
fn events_stream(events: Receiver<Arc<String>>, mut stream: TcpStream) {
    if stream.write_all(sse::SSE_HEADERS.as_bytes()).is_err() {
        return;
    }
    // SSE clients send nothing, so a read only ever times out (still alive)
    // or returns 0 (hung up); probe when idle to reap dead connections
    stream.set_read_timeout(Some(Duration::from_millis(1))).unwrap();
    let mut probe = [0u8; 64];
    loop {
        match events.recv_timeout(Duration::from_millis(300)) {
            Ok(event) => {
                if stream.write_all(event.as_bytes()).is_err() {
                    return;
                }
            }
            Err(RecvTimeoutError::Timeout) => match stream.read(&mut probe) {
                Ok(0) => return, // client closed the connection
                Ok(_) => {}
                Err(e)
                    if matches!(
                        e.kind(),
                        std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                    ) => {}
                Err(_) => return,
            },
            Err(RecvTimeoutError::Disconnected) => return,
        }
    }
}
