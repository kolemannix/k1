use itertools::Itertools;
use maud::html;

use crate::codegen_llvm::Cg;
use crate::ir;
use crate::typer::CellResult;
use crate::typer::TypedProgram;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;


fn code_input_id(cell_id: u32) -> String {
    format!("code-input-{cell_id}")
}
fn ir_card_id(cell_id: u32) -> String {
    format!("cell-ir-{cell_id}")
}

//nocommit TODO watcher exprs, one visualizer, and 'new' working
fn render_ir_card(cell_id: u32, ir: &str) -> maud::Markup {
    html! {
        article .result-card {
            h2 .result-label { "ir" }
            pre #(ir_card_id(cell_id)) .result-output { (ir) }
        }
    }
}
fn render_cell(k1: &TypedProgram, cell_id: u32) -> maud::Markup {
    let cell = k1.megarepl_get_cell(cell_id);
    let cell_id_text = format!("cell-{}", cell_id);
    let post = format!("@post('/cell/{}')", cell_id);
    let source = k1.ast.sources.get(cell.source_id);
    let signal_name = code_input_id(cell_id);
    let is_error = matches!(cell.last_result, CellResult::Error { .. });
    html! {
        div #(cell_id_text) {
        p { (source.filename) }
        textarea
            .code-input type="text"
            autofocus
            placeholder="k1 code, then we go"
            data-bind=(signal_name)
            data-on:change=(post)
            { (source.content) }
        section .results {
            article .result-card {
                h2 .result-label { "value" }
                
                @let output = match &cell.last_result {
                    CellResult::Expr { value } => k1.static_value_to_string(*value),
                    CellResult::Error { k1_message } => k1_message.message.clone()
                };
                pre .result-output .error[is_error] { (output) }
            }
            (render_ir_card(cell_id, ""))
        }
        }
    }
}
fn full_page(k1: &TypedProgram, styles: &str) -> maud::Markup {
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
                header .site-header {
                    h1 { "k1 megarepl" }
                    code .signals-debug data-json-signals {}
                }
                main .repl {
                    @for cell in &k1.megarepl.as_ref().unwrap().cells {
                        (render_cell(k1, cell.id))
                    }
                    button data-on-click="@post('/new-cell')" { "New" }
                }
            }
        }
    }
}

const NOT_FOUND: &str =
    "HTTP/1.1 404 NOT FOUND\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n";
const BAD_REQUEST: &str =
    "HTTP/1.1 400 BAD REQUEST\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n";

fn write_response(stream: &mut TcpStream, body: &str) {
    if let Err(e) = write!(
        stream,
        "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
        body.len(),
        body
    ) {
        eprintln!("tcp write error: {e}")
    }
}

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
            let n = stream.read(&mut chunk).unwrap();
            if n == 0 {
                return;
            }
            buffer.extend_from_slice(&chunk[..n]);
        }
        let body = &buffer[header_len..header_len + content_length];

        let styles = include_str!("megarepl.css");
        let mut segments: Vec<_> = path[1..].split('/').collect();
        eprintln!("handling {} {:?}", method, segments);
        match *segments.first().unwrap() {
            "" => {
                match cg.k1.megarepl {
                    None => {
                        let cell_id = cg.k1.megarepl_submit(None, "".to_string());
                    }
                    Some(_) => {}
                }
                let page = full_page(cg.k1, styles);
                write_response(&mut stream, &page.0)
            }
            "cell" => {
                let Some(cell_id) = segments.get(1) else {
                    stream.write_all(BAD_REQUEST.as_bytes()).unwrap();
                    return;
                };
                let Ok(cell_id) = cell_id.parse::<u32>() else {
                    stream.write_all(BAD_REQUEST.as_bytes()).unwrap();
                    return;
                };
                let Ok(serde_json::Value::Object(signals)) =
                    serde_json::from_slice::<serde_json::Value>(body)
                else {
                    stream.write_all(BAD_REQUEST.as_bytes()).unwrap();
                    return;
                };
                let code = signals.get(&code_input_id(cell_id)).unwrap().as_str().unwrap();
                let cell_id = cg.k1.megarepl_submit(Some(cell_id), code.to_string());
                let output = render_cell(cg.k1, cell_id);
                write_response(&mut stream, &output.0);
            }
            "functions" => {
                let f = cg
                    .k1
                    .function_iter()
                    .map(|(_, tf)| cg.k1.function_signature_to_string(&tf.signature()))
                    .join("\n");
                write_response(&mut stream, &f);
            }
            "cell-ir" => {
                let Some(cell_id) = segments.get(1) else {
                    stream.write_all(BAD_REQUEST.as_bytes()).unwrap();
                    return;
                };
                let Ok(cell_id) = cell_id.parse::<u32>() else {
                    stream.write_all(BAD_REQUEST.as_bytes()).unwrap();
                    return;
                };

                let cell = cg.k1.megarepl_get_cell(cell_id);
                let ir_string = match cell.expr_id {
                    None => "".to_string(),
                    Some(expr_id) => ir::unit_to_string(cg.k1, ir::IrUnitId::Expr(expr_id), true),
                };
                let output = render_ir_card(cell_id, &ir_string);
                write_response(&mut stream, &output.0);
            }
            "new-cell" => {
                let cell_id = cg.k1.megarepl_submit(None, "".to_string());
                let output = render_cell(cg.k1, cell_id);
                write_response(&mut stream, &output.0);
            }
            _ => {
                stream.write_all(NOT_FOUND.as_bytes()).unwrap();
            }
        }

        // Consume this request so the next one starts at the front
        buffer.drain(..header_len + content_length);
    }
}
