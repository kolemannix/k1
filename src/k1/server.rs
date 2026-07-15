use itertools::Itertools;

use crate::codegen_llvm::Cg;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;

fn full_page(styles: &str) -> maud::Markup {
    // Convert the above to maud
    use maud::{DOCTYPE, PreEscaped};
    maud::html! {
        (DOCTYPE)
        html {
            head {
                meta charset="UTF-8";
                meta name="viewport" content="width=device-width, initial-scale=1.0";
                script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js" {}
                title { "k1 megarepl" }
                style { (PreEscaped(styles)) }
            }
            body {
                div data-json-signals {}
                h1 { "Hello, World" }
                input .code type="text" data-bind="code-input" data-on:change="@post('/code')";
                p #output {}
            }
        }
    }
}

const NOT_FOUND: &str =
    "HTTP/1.1 404 NOT FOUND\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n";
const BAD_REQUEST: &str =
    "HTTP/1.1 400 BAD REQUEST\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n";

pub fn handle_client(cg: &mut Cg, mut stream: TcpStream) {
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
            let n = stream.read(&mut chunk).unwrap();
            if n == 0 {
                return; // client closed the connection
            }
            buffer.extend_from_slice(&chunk[..n]);
        };

        // Headers are complete; make sure the whole body has arrived too
        while buffer.len() < header_len + content_length {
            let n = stream.read(&mut chunk).unwrap();
            if n == 0 {
                return;
            }
            buffer.extend_from_slice(&chunk[..n]);
        }
        let body = &buffer[header_len..header_len + content_length];

        eprintln!("handling {} {}", method, path);

        let mut write_response = |stream: &mut TcpStream, body: &str| {
            write!(
                stream,
                "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
                body.len(),
                body
            )
            .unwrap();
        };

        let styles = "";
        match path.as_str() {
            "/" => write_response(&mut stream, &full_page(styles).0),
            "/code" => {
                let Ok(serde_json::Value::Object(signals)) =
                    serde_json::from_slice::<serde_json::Value>(body)
                else {
                    stream.write_all(BAD_REQUEST.as_bytes()).unwrap();
                    return;
                };
                let code = signals.get("code-input").unwrap().as_str().unwrap();
                let s = cg.k1.megarepl_submit(code.to_string());
                write_response(&mut stream, &format!("<div id=output>{}</div>", s));
            }
            "/functions" => {
                let f = cg
                    .k1
                    .function_iter()
                    .map(|(_, tf)| cg.k1.function_signature_to_string(&tf.signature()))
                    .join("\n");
                write_response(&mut stream, &f);
            }
            _ => {
                stream.write_all(NOT_FOUND.as_bytes()).unwrap();
            }
        }

        // Consume this request so the next one starts at the front
        buffer.drain(..header_len + content_length);
    }
}
