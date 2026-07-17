mod tabs;

use itertools::Itertools;
use maud::html;
use tabs::Tabs;

use crate::ir;
use crate::typer::megarepl::{CellResult, MegareplCell};
use crate::typer::TypedProgram;
use std::borrow::Cow;
use std::fmt;
use std::io::Read;
use std::io::Write;
use std::net::{TcpListener, TcpStream};
use std::sync::Mutex;
use std::time::{Duration, SystemTime};

enum Response {
    Ok(String),
    BadRequest,
    NotFound,
}

/// Wraps a closure as `Display` so render helpers can return cheap values that
/// maud splices (and `format!` args) write straight into the output buffer,
/// instead of building intermediate `String`s.
struct DisplayFn<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result>(F);
impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Display for DisplayFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

const TAB_OUTPUT: &str = "output";
const TAB_IR: &str = "ir";

/// All of a cell's signals live under the `cells_{id}_` prefix, so requests can
/// filter to exactly one cell's state.
fn cell_signal(cell_id: u32, field: &'static str) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "cells_{cell_id}_{field}"))
}

/// Datastar request option restricting the signals an action sends to those
/// matching the wrapped include regex
struct SignalFilter<R: fmt::Display>(R);

impl<R: fmt::Display> fmt::Display for SignalFilter<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{filterSignals: {{include: /{}/}}}}", self.0)
    }
}

/// Submits the cell's code for recompilation/re-execution. Sends the cell's
/// own signals plus every cell's tab signal, since watchers re-render in the
/// response and their IR panes need to be kept fresh too.
fn cell_post_action(cell_id: u32) -> impl fmt::Display {
    DisplayFn(move |f| {
        write!(
            f,
            "@post('/cell/{cell_id}', {})",
            SignalFilter(format_args!("^cells_{cell_id}_|_tab$"))
        )
    })
}

/// Toggles whether the cell's output is pinned to the canvas (which also
/// makes it a watcher: a displayed tile is a live one)
fn pin_toggle_action(cell_id: u32) -> impl fmt::Display {
    DisplayFn(move |f| {
        write!(
            f,
            "@post('/cell-pin/{cell_id}', {})",
            SignalFilter(format_args!("^cells_{cell_id}_tab$"))
        )
    })
}

fn cell_dom_id(cell_id: u32) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "cell-{cell_id}"))
}
fn ir_output_id(cell_id: u32) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "cell-ir-{cell_id}"))
}
fn chip_dom_id(cell_id: u32) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "chip-{cell_id}"))
}
fn tile_dom_id(cell_id: u32) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "tile-{cell_id}"))
}

fn exec_time_display(exec_time: std::time::Duration) -> impl fmt::Display {
    DisplayFn(move |f| {
        let ms = exec_time.as_secs_f64() * 1000.0;
        if ms < 1.0 {
            write!(f, "{:.0} µs", ms * 1000.0)
        } else {
            write!(f, "{ms:.2} ms")
        }
    })
}

fn cell_tabs(cell_id: u32) -> Tabs {
    let ir_get = format!("@get('/cell-ir/{cell_id}', {})", SignalFilter("^$"));
    Tabs::new(cell_signal(cell_id, "tab").to_string())
        .tab(TAB_OUTPUT, "Output")
        .tab_with_action(TAB_IR, "IR", ir_get)
}

//nocommit TODO at least one render function
//nocommit TODO a cell kind that can update its data (not a cell, because not code - a 'control' ?)
//nocommit TODO 'connect' this server to the lsp. We should be able to re-compile underneath and
//              keep all the cell sources; submit a cell from your editor via Conjure and see it in
//              the server page
//              Render UI for exploring your program: namespaces, types, abilities (their impls), functions
fn megarepl_cells(k1: &TypedProgram) -> &[MegareplCell] {
    &k1.megarepl.as_ref().unwrap().cells
}

fn render_ir_output(cell_id: u32, ir: &str) -> maud::Markup {
    html! {
        pre #(ir_output_id(cell_id))
            .result-output
            data-show=(cell_tabs(cell_id).selected(TAB_IR))
            { (ir) }
    }
}
fn cell_ir_string(k1: &TypedProgram, cell_id: u32) -> String {
    match k1.megarepl_get_cell(cell_id).expr_id {
        None => String::new(),
        Some(expr_id) => ir::unit_to_string(k1, ir::IrUnitId::Expr(expr_id), true),
    }
}

fn cell_output<'k>(k1: &TypedProgram, cell: &'k MegareplCell) -> (Cow<'k, str>, bool) {
    match &cell.last_result {
        CellResult::Expr { value } => (Cow::Owned(k1.static_value_to_string(*value)), false),
        CellResult::Error { k1_message } => (Cow::Borrowed(k1_message.message.as_str()), true),
    }
}

/// Compact cell representation for the collapsed rail; the dot carries the
/// cell's status (error / watcher / plain)
fn render_chip(cell: &MegareplCell) -> maud::Markup {
    let is_error = matches!(cell.last_result, CellResult::Error { .. });
    let dot = if is_error {
        "err"
    } else if cell.is_pinned {
        "pinned"
    } else {
        "ok"
    };
    html! {
        button #(chip_dom_id(cell.id)) .chip
            title="Expand cells"
            data-on:click="$_rail_collapsed = false" {
            (cell.id)
            span class={ "chip-dot " (dot) } {}
        }
    }
}

/// A pinned cell's output, placed on the canvas
fn render_tile(k1: &TypedProgram, cell_id: u32) -> maud::Markup {
    let cell = k1.megarepl_get_cell(cell_id);
    let (output, is_error) = cell_output(k1, cell);
    html! {
        article #(tile_dom_id(cell_id)) .tile {
            div .tile-label .error[is_error] {
                span  { "cell " (cell_id) }
                @if let Some(exec_time) = cell.last_exec_time {
                    span { (exec_time_display(exec_time)) }
                }
            }
            pre .result-output .tile-value { (output) }
        }
    }
}
/// `include_ir`: the IR pane is lazy, so it renders empty unless the client is
/// currently viewing it (its tab signal says so) and needs it kept fresh.
fn render_cell(k1: &TypedProgram, cell_id: u32, include_ir: bool) -> maud::Markup {
    let cell = k1.megarepl_get_cell(cell_id);
    let source = k1.ast.sources.get(cell.source_id);
    let post = cell_post_action(cell_id);
    let tabs = cell_tabs(cell_id);
    let is_error = matches!(cell.last_result, CellResult::Error { .. });
    let (output, _) = cell_output(k1, cell);
    let ir = if include_ir { cell_ir_string(k1, cell_id) } else { String::new() };
    html! {
        div #(cell_dom_id(cell_id)) {
        header .cell-header {
            button .run-button data-on:click=(post) { "Run" }
            label .pin-toggle title="Show this cell's output on the canvas, re-run after every run" {
                input type="checkbox"
                    checked[cell.is_pinned]
                    data-on:change=(pin_toggle_action(cell_id));
                "pin"
            }
            @if let Some(exec_time) = cell.last_exec_time {
                span .exec-time { (exec_time_display(exec_time)) }
            }
        }
        textarea
            .code-input type="text"
            autofocus
            placeholder="k1 code, then we go"
            data-bind=(cell_signal(cell_id, "code"))
            data-on:change=(post)
            { (source.content) }
        section .results {
            article .result-card {
                (tabs.nav())
                pre .result-output .error[is_error]
                    data-show=(tabs.selected(TAB_OUTPUT))
                    { (output) }
                (render_ir_output(cell_id, &ir))
            }
        }
        }
    }
}
/// Cells live in the rail: full cards when expanded, status chips at 44px when
/// collapsed. Collapse is client state ($_rail_collapsed; the underscore keeps
/// it out of every request).
fn render_rail(k1: &TypedProgram, signals: &Signals) -> maud::Markup {
    html! {
        aside #rail .cell-rail {
            div .rail-cells {
                @for cell in megarepl_cells(k1) {
                    (render_cell(k1, cell.id, CellSignals::extract(signals, cell.id).viewing_ir))
                }
                // Sends every cell's tab signal so re-rendering keeps viewed IR fresh
                button .rail-new data-on:click=(format!("@post('/new-cell', {})", SignalFilter("_tab$")))
                    { "+ new cell" }
            }
            div .rail-chips {
                @for cell in megarepl_cells(k1) {
                    (render_chip(cell))
                }
            }
        }
    }
}

fn render_canvas(k1: &TypedProgram) -> maud::Markup {
    let any_pinned = megarepl_cells(k1).iter().any(|c| c.is_pinned);
    html! {
        section #canvas .canvas {
            @if any_pinned {
                div .tiles {
                    @for cell in megarepl_cells(k1) {
                        @if cell.is_pinned { (render_tile(k1, cell.id)) }
                    }
                }
            } @else {
                div .canvas-hint { "pin a cell's output to place it here" }
            }
        }
    }
}

fn render_workspace(k1: &TypedProgram, signals: &Signals) -> maud::Markup {
    html! {
        main #workspace .workspace data-class:collapsed="$_rail_collapsed" {
            (render_rail(k1, signals))
            (render_canvas(k1))
        }
    }
}
/// Styles are read from the source tree at runtime so /css-watch can
/// live-reload edits; the compiled-in copy is the fallback.
const CSS_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/k1/megarepl.css");

fn read_styles() -> String {
    std::fs::read_to_string(CSS_PATH).unwrap_or_else(|_| include_str!("megarepl.css").to_string())
}

fn css_mtime() -> Option<SystemTime> {
    std::fs::metadata(CSS_PATH).ok().and_then(|m| m.modified().ok())
}

fn render_full_page(k1: &TypedProgram) -> maud::Markup {
    use maud::{DOCTYPE, PreEscaped};
    maud::html! {
        (DOCTYPE)
        html {
            head {
                meta charset="UTF-8";
                meta name="viewport" content="width=device-width, initial-scale=1.0";
                script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js" {}
                title { "k1 megarepl" }
                style #page-styles { (PreEscaped(read_styles())) }
            }
            body data-init=(format!("@get('/css-watch', {})", SignalFilter("^$"))) {
                header .site-header {
                    button .rail-toggle
                        title="Collapse or expand the cell rail"
                        data-on:click="$_rail_collapsed = !$_rail_collapsed"
                        data-text="$_rail_collapsed ? '»' : '«'"
                        { "«" }
                    h1 { "k1 megarepl" }
                }
                (render_workspace(k1, &Signals::new()))
                footer .debug-footer {
                    code .signals-debug data-json-signals {}
                }
            }
        }
    }
}

const NOT_FOUND: &str =
    "HTTP/1.1 404 NOT FOUND\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n";
const BAD_REQUEST: &str =
    "HTTP/1.1 400 BAD REQUEST\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n";

fn write_response(stream: &mut TcpStream, response: &Response) {
    let result = match response {
        Response::Ok(body) => write!(
            stream,
            "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
            body.len(),
            body
        ),
        Response::BadRequest => stream.write_all(BAD_REQUEST.as_bytes()),
        Response::NotFound => stream.write_all(NOT_FOUND.as_bytes()),
    };
    if let Err(e) = result {
        eprintln!("tcp write error: {e}")
    }
}

type Signals = serde_json::Map<String, serde_json::Value>;

fn parse_signals(body: &[u8]) -> Option<Signals> {
    match serde_json::from_slice(body).ok()? {
        serde_json::Value::Object(signals) => Some(signals),
        _ => None,
    }
}

/// One cell's signals, plucked out of the dynamically-named signals object
struct CellSignals<'a> {
    code: Option<&'a str>,
    viewing_ir: bool,
}

impl<'a> CellSignals<'a> {
    fn extract(signals: &'a Signals, cell_id: u32) -> CellSignals<'a> {
        let get = |field: &'static str| {
            signals.get(&cell_signal(cell_id, field).to_string()).and_then(|v| v.as_str())
        };
        CellSignals { code: get("code"), viewing_ir: get("tab") == Some(TAB_IR) }
    }
}

/// Everything in the page that changes when a cell's result changes: its rail
/// card, its status chip, and (when pinned) its canvas tile. Plain HTML
/// responses patch each top-level element by id.
fn render_cell_patch(k1: &TypedProgram, cell_id: u32, signals: &Signals) -> String {
    let viewing_ir = CellSignals::extract(signals, cell_id).viewing_ir;
    let cell = k1.megarepl_get_cell(cell_id);
    let mut html = render_cell(k1, cell_id, viewing_ir).0;
    html.push_str(&render_chip(cell).0);
    if cell.is_pinned {
        html.push_str(&render_tile(k1, cell_id).0);
    }
    html
}

/// Parses the `/<route>/{cell_id}` path segment and validates the cell exists.
fn parse_cell_id(k1: &TypedProgram, segments: &[&str]) -> Option<u32> {
    let cell_id: u32 = segments.get(1)?.parse().ok()?;
    let cell_count = k1.megarepl.as_ref()?.cells.len();
    if (cell_id as usize) < cell_count { Some(cell_id) } else { None }
}

fn handle_request(k1: &mut TypedProgram, method: &str, path: &str, body: &[u8]) -> Response {
    let path = path.split('?').next().unwrap();
    let segments: Vec<&str> = path[1..].split('/').collect();
    eprintln!("handling {} {:?}", method, segments);
    match *segments.first().unwrap() {
        "" => {
            if k1.megarepl.is_none() {
                k1.megarepl_submit(None, String::new());
            }
            Response::Ok(render_full_page(k1).0)
        }
        "new-cell" => {
            let signals = parse_signals(body).unwrap_or_default();
            k1.megarepl_submit(None, String::new());
            Response::Ok(render_workspace(k1, &signals).0)
        }
        "cell" => {
            let Some(cell_id) = parse_cell_id(k1, &segments) else {
                return Response::BadRequest;
            };
            let Some(signals) = parse_signals(body) else {
                return Response::BadRequest;
            };
            let Some(code) = CellSignals::extract(&signals, cell_id).code else {
                return Response::BadRequest;
            };
            let cell_id = k1.megarepl_submit(Some(cell_id), code.to_string());
            // Watchers re-ran too; patch them along with the submitted cell
            let mut html = render_cell_patch(k1, cell_id, &signals);
            let watcher_ids: Vec<u32> = megarepl_cells(k1)
                .iter()
                .filter(|c| c.is_watcher && c.id != cell_id)
                .map(|c| c.id)
                .collect();
            for watcher_id in watcher_ids {
                html.push_str(&render_cell_patch(k1, watcher_id, &signals));
            }
            Response::Ok(html)
        }
        "cell-pin" => {
            let Some(cell_id) = parse_cell_id(k1, &segments) else {
                return Response::BadRequest;
            };
            let signals = parse_signals(body).unwrap_or_default();
            k1.megarepl_toggle_pin(cell_id);
            // The tile count changed, so replace the whole canvas
            let viewing_ir = CellSignals::extract(&signals, cell_id).viewing_ir;
            let mut html = render_cell(k1, cell_id, viewing_ir).0;
            html.push_str(&render_canvas(k1).0);
            Response::Ok(html)
        }
        "cell-ir" => {
            let Some(cell_id) = parse_cell_id(k1, &segments) else {
                return Response::BadRequest;
            };
            let ir_string = cell_ir_string(k1, cell_id);
            Response::Ok(render_ir_output(cell_id, &ir_string).0)
        }
        "functions" => {
            let f = k1
                .function_iter()
                .map(|(_, tf)| k1.function_signature_to_string(&tf.signature()))
                .join("\n");
            Response::Ok(f)
        }
        _ => Response::NotFound,
    }
}

/// One thread per connection; the compiler is locked per request, so routes
/// that never touch it (the SSE ones) can hold their connection open forever.
pub fn serve(k1: TypedProgram) {
    let listener = TcpListener::bind("127.0.0.1:8080").expect("Could not bind to port");
    println!("k1 server running on http://127.0.0.1:8080");
    let k1 = Mutex::new(k1);
    std::thread::scope(|scope| {
        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    // Compiling cells recurses; give connections compiler-sized stacks
                    std::thread::Builder::new()
                        .stack_size(crate::STACK_SIZE)
                        .spawn_scoped(scope, || handle_client(&k1, stream))
                        .expect("failed to spawn connection thread");
                }
                Err(e) => eprintln!("Connection failed: {}", e),
            }
        }
    });
}

fn handle_client(k1: &Mutex<TypedProgram>, mut stream: TcpStream) {
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
        if path.split('?').next().unwrap() == "/css-watch" {
            return css_watch(stream);
        }

        let response = {
            let mut k1 = k1.lock().unwrap();
            handle_request(&mut k1, &method, &path, body)
        };
        write_response(&mut stream, &response);

        // Consume this request so the next one starts at the front
        buffer.drain(..header_len + content_length);
    }
}

/// Holds the connection open and pushes a patched `<style>` element whenever
/// megarepl.css changes on disk.
fn css_watch(mut stream: TcpStream) {
    const SSE_HEADERS: &str =
        "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\n\r\n";
    if stream.write_all(SSE_HEADERS.as_bytes()).is_err() {
        return;
    }
    // The blocking read doubles as the poll interval and disconnect detection:
    // SSE clients send nothing, so reads only ever time out or return 0 on close
    stream.set_read_timeout(Some(Duration::from_millis(300))).unwrap();
    let mut probe = [0u8; 64];
    let mut last_seen = css_mtime();
    loop {
        match stream.read(&mut probe) {
            Ok(0) => return, // client closed the connection
            Ok(_) => {}
            Err(e) if matches!(
                e.kind(),
                std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
            ) => {}
            Err(_) => return,
        }
        let mtime = css_mtime();
        if mtime == last_seen {
            continue;
        }
        last_seen = mtime;
        eprintln!("css changed; patching");
        let mut event = String::from(
            "event: datastar-patch-elements\ndata: elements <style id=\"page-styles\">\n",
        );
        for line in read_styles().lines() {
            event.push_str("data: elements ");
            event.push_str(line);
            event.push('\n');
        }
        event.push_str("data: elements </style>\n\n");
        if stream.write_all(event.as_bytes()).is_err() {
            return;
        }
    }
}
