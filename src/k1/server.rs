mod browse;
mod bus;
mod http;
pub mod sse;

use http::Response;
pub use http::serve;
use maud::html;

use crate::typer::TypedProgram;
use crate::typer::megarepl::{
    CellId, CellResult, MegareplCell, MegareplGlobal, MegareplWidget, WidgetDataSource, WidgetKind,
};
use std::fmt;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};

/// The program the server serves, shared with whoever compiles it: the CLI
/// hands over sole ownership, while the LSP keeps a handle and swaps in a
/// whole new program on every recompile.
pub type SharedProgram = Arc<Mutex<Option<Box<TypedProgram>>>>;

/// Wraps a closure as `Display` so render helpers can return cheap values that
/// maud splices (and `format!` args) write straight into the output buffer,
/// instead of building intermediate `String`s.
struct DisplayFn<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result>(F);
impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Display for DisplayFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

/// All of a cell's signals live under the `cells_{id}_` prefix, so requests can
/// filter to exactly one cell's state.
fn cell_signal(cell_id: CellId, field: &'static str) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "cells_{cell_id}_{field}"))
}

/// Client-only UI state for a cell (collapse/disclosure toggles); the leading
/// underscore keeps these signals out of request payloads
fn cell_ui_signal(cell_id: CellId, field: &'static str) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "_cells_{cell_id}_{field}"))
}

/// Datastar request option restricting the signals an action sends to those
/// matching the wrapped include regex
struct SignalFilter<R: fmt::Display>(R);

impl<R: fmt::Display> fmt::Display for SignalFilter<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{filterSignals: {{include: /{}/}}}}", self.0)
    }
}

/// Submits the cell's code for recompilation/re-execution
fn cell_post_action(cell_id: CellId) -> impl fmt::Display {
    DisplayFn(move |f| {
        write!(
            f,
            "@post('/cell/{cell_id}', {})",
            SignalFilter(format_args!("^cells_{cell_id}_code$"))
        )
    })
}

/// Toggles whether the cell re-runs after every run, showing its result in
/// Data
fn watch_toggle_action(cell_id: CellId) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "@post('/cell-watch/{cell_id}', {})", SignalFilter("^$")))
}

fn cell_dom_id(cell_id: CellId) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "cell-{cell_id}"))
}
fn chip_dom_id(cell_id: CellId) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "chip-{cell_id}"))
}
fn widget_dom_id(widget_id: u32) -> impl fmt::Display {
    DisplayFn(move |f| write!(f, "widget-{widget_id}"))
}

fn exec_time_display(exec_time: std::time::Duration) -> impl fmt::Display {
    DisplayFn(move |f| {
        let ms = exec_time.as_secs_f64() * 1000.0;
        if ms < 1.0 { write!(f, "{:.0} µs", ms * 1000.0) } else { write!(f, "{ms:.2} ms") }
    })
}

//nocommit TODO widget kinds, in rough order:
//              [ ] Pretty: generic html rendering by type shape (table for list-of-struct, ...)
//              [ ] read/write presets, closed set per type (slider for ints, checkbox for bool,
//                  select for no-payload enums); write path: decode scalar -> write VM global ->
//                  re-run watchers -> publish
//              [ ] custom k1 render fn (sketched in helloworld's `ns megarepl`)
//              [ ] arbitrary JS / web-component: takes input, emits output events; needs
//                  static_value_to_js_value written once, and js_value_to_static_value for the
//                  emitted data
//nocommit TODO Data panel as an instantiation of a list widget; tabled until it emerges — once we
//              can implement Data with a widget, it's time
//nocommit TODO [x] 'connect' this server to the lsp. We should be able to re-compile underneath and
//              keep all the cell sources; submit a cell from your editor via Conjure and see it in
//              the server page
//              x Render UI for exploring your program: namespaces, types, abilities (their impls), functions
//nocommit TODO stop going through static value and instead work with the raw memory layout more and
//              more (StaticValue::Raw(...)) is an option
//
fn megarepl_cells(k1: &TypedProgram) -> &[MegareplCell] {
    &k1.megarepl.as_ref().unwrap().cells
}
fn megarepl_globals(k1: &TypedProgram) -> &[MegareplGlobal] {
    &k1.megarepl.as_ref().unwrap().globals
}

fn render_output_panes(k1: &TypedProgram, cell: &MegareplCell) -> maud::Markup {
    let (values, is_error, stdout, stderr) = match &cell.last_result {
        CellResult::Expr { outputs, value, stdout, stderr } => {
            let mut values: Vec<String> =
                outputs.iter().map(|v| k1.megarepl_static_value_to_string(*v)).collect();
            if *value != k1.static_values.empty_id() {
                values.push(k1.megarepl_static_value_to_string(*value));
            }
            (values, false, stdout.as_str(), stderr.as_str())
        }
        CellResult::Error { k1_message, stdout, stderr } => {
            (vec![k1_message.message.clone()], true, stdout.as_str(), stderr.as_str())
        }
    };
    html! {
        @if !values.is_empty() {
            pre .result-output .output-values .error[is_error] { (values.join("\n")) }
        }
        @if !stdout.is_empty() {
            pre .result-output .output-stdout { (stdout) }
        }
        @if !stderr.is_empty() {
            pre .result-output .output-stderr { (stderr) }
        }
    }
}

/// Compact cell representation for the collapsed rail; the dot carries the
/// cell's status (error / watched / plain)
fn render_chip(cell: &MegareplCell) -> maud::Markup {
    let dot = if cell.is_error() {
        "err"
    } else if cell.is_watcher {
        "watch"
    } else {
        "ok"
    };
    html! {
        button #(chip_dom_id(cell.id)) .chip
            title="Expand"
            data-on:click="$_rail_collapsed = false" {
            (cell.id + 1)
            span class={ "chip-dot " (dot) } {}
        }
    }
}

fn render_widget(k1: &TypedProgram, widget: &MegareplWidget) -> maud::Markup {
    enum Body {
        Value(String),
        Error,
        Unbound,
    }
    let (label, body) = match widget.data {
        WidgetDataSource::Binding(name) => {
            let body = match k1.megarepl_resolve_binding(name).and_then(|g| g.last_value) {
                Some(value) => Body::Value(k1.megarepl_static_value_to_string(value)),
                None => Body::Unbound,
            };
            (k1.ident_str(name).to_string(), body)
        }
        WidgetDataSource::Watcher(cell_id) => {
            let body = match k1.megarepl_get_cell_opt(cell_id).map(|c| &c.last_result) {
                Some(CellResult::Expr { value, .. }) => {
                    Body::Value(k1.megarepl_static_value_to_string(*value))
                }
                Some(CellResult::Error { .. }) => Body::Error,
                None => Body::Unbound,
            };
            (format!("⟳ {}", cell_id + 1), body)
        }
    };
    let delete = format!("@post('/widget/delete/{}', {})", widget.id, SignalFilter("^$"));
    html! {
        article #(widget_dom_id(widget.id)) .card .widget {
            div .card-label .error[matches!(body, Body::Error)] {
                span { (label) }
                button .widget-close title="Remove" data-on:click=(delete) { "×" }
            }
            @match &body {
                // WidgetKind::Plain is the only kind so far
                Body::Value(text) => { pre .result-output { (text) } }
                Body::Error => { p .widget-hint .error { "error" } }
                Body::Unbound => { p .widget-hint { "unbound" } }
            }
        }
    }
}

fn render_cell(k1: &TypedProgram, cell: &MegareplCell) -> maud::Markup {
    let source = k1.ast.sources.get(cell.source_id);
    let post = cell_post_action(cell.id);
    let closed = cell_ui_signal(cell.id, "closed");
    let show_out = cell_ui_signal(cell.id, "out");
    let show_ir = cell_ui_signal(cell.id, "ir");
    let summary = source.content.lines().next().unwrap_or("");
    html! {
        div #(cell_dom_id(cell.id)) .cell data-class:closed={ "$" (closed) } {
        header .cell-header {
            button .run-button title="Run" data-on:click=(post) { "▶" }
            span .cell-label { (cell.id + 1) }
            span .cell-summary .error[cell.is_error()] { (summary) }
            @if let Some(exec_time) = cell.last_exec_time {
                span .exec-time { (exec_time_display(exec_time)) }
            }
            label .watch-toggle title="Re-run after every run; the result shows in Data" {
                input type="checkbox"
                    checked[cell.is_watcher]
                    data-on:change=(watch_toggle_action(cell.id));
                "watch"
            }
            button .cell-toggle
                title="Collapse or expand"
                data-on:click={ "$" (closed) " = !$" (closed) }
                data-text={ "$" (closed) " ? '▸' : '▾'" }
                { "▾" }
        }
        div .cell-body {
            textarea
                .code-input type="text"
                autofocus
                placeholder="k1 code, then we go"
                data-bind=(cell_signal(cell.id, "code"))
                data-on:change=(post)
                { (source.content) }
            div .cell-disclosures {
                button .disclosure data-class:on={ "$" (show_out) }
                    data-on:click={ "$" (show_out) " = !$" (show_out) } { "output" }
                button .disclosure data-class:on={ "$" (show_ir) }
                    data-on:click={ "$" (show_ir) " = !$" (show_ir) } { "ir" }
            }
            // Errors demand attention; ordinary output waits behind its toggle
            @if cell.is_error() {
                (render_output_panes(k1, cell))
            } @else {
                div .cell-output data-show={ "$" (show_out) } {
                    (render_output_panes(k1, cell))
                }
            }
            @if !cell.last_ir.is_empty() {
                pre .result-output .cell-ir data-show={ "$" (show_ir) } { (cell.last_ir) }
            }
        }
        }
    }
}

/// Places a Plain widget for the Data row on the canvas
fn widget_plus_button(data_path: fmt::Arguments) -> maud::Markup {
    html! {
        button .data-plus title="Show on the canvas"
            data-on:click=(format_args!("@post('/widget/new/{data_path}', {})", SignalFilter("^$")))
            { "+" }
    }
}

/// The rail's "Data" panel: every live repl global with its type and the
/// value it held after the last run, plus each watcher's latest result —
/// a value that re-computes on every run is data too
fn render_data_section(k1: &TypedProgram) -> maud::Markup {
    let live: Vec<&MegareplGlobal> =
        megarepl_globals(k1).iter().filter(|g| k1.megarepl_global_is_live(g)).collect();
    let watchers: Vec<&MegareplCell> = megarepl_cells(k1).iter().filter(|c| c.is_watcher).collect();
    html! {
        section #data .card .data-section {
            div .card-label { span { "Data" } }
            @if live.is_empty() && watchers.is_empty() {
                p .data-hint { "let-bound values appear here" }
            } @else {
                table .data-table {
                    @for mr_global in live {
                        @let global = k1.globals.get(mr_global.global_id);
                        @let variable = k1.variables.get(global.variable_id);
                        @let name = k1.ident_str(variable.name);
                        tr {
                            td .data-name { (name) }
                            td .data-type { (k1.type_id_to_string(global.type_id)) }
                            td .data-value {
                                @match mr_global.last_value {
                                    Some(value) => {
                                        div .data-clamp {
                                            (k1.megarepl_static_value_to_string(value))
                                        }
                                    }
                                    None => { "—" }
                                }
                            }
                            td { (widget_plus_button(format_args!("let/{name}"))) }
                        }
                    }
                    @for cell in &watchers {
                        tr {
                            td .data-name .data-watch { "⟳ " (cell.id + 1) }
                            @match (&cell.last_result, cell.expr_id) {
                                (CellResult::Expr { value, .. }, Some(expr_id)) => {
                                    td .data-type {
                                        (k1.type_id_to_string(k1.exprs.get_type(expr_id)))
                                    }
                                    td .data-value {
                                        @if *value == k1.static_values.empty_id() { "—" }
                                        @else {
                                            div .data-clamp {
                                                (k1.megarepl_static_value_to_string(*value))
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    td .data-type {}
                                    td .data-value .error { "error" }
                                }
                            }
                            td { (widget_plus_button(format_args!("watcher/{}", cell.id))) }
                        }
                    }
                }
            }
        }
    }
}

fn render_rail(k1: &TypedProgram) -> maud::Markup {
    html! {
        aside #rail .cell-rail {
            div .rail-cells {
                // Data leads and stays visible; cells scroll beneath it
                (render_data_section(k1))
                div .cell-list {
                    @for cell in megarepl_cells(k1) {
                        (render_cell(k1, cell))
                    }
                    button .rail-new data-on:click=(format!("@post('/new-cell', {})", SignalFilter("^$")))
                        { "+ new code" }
                }
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
    let widgets = &k1.megarepl.as_ref().unwrap().widgets;
    html! {
        section #canvas .canvas {
            @if widgets.is_empty() {
                div .canvas-hint { "widgets appear here; add them from Data with +" }
            } @else {
                div .tiles {
                    @for widget in widgets {
                        (render_widget(k1, widget))
                    }
                }
            }
        }
    }
}

fn render_workspace(k1: &TypedProgram) -> maud::Markup {
    html! {
        main #workspace .workspace data-class:collapsed="$_rail_collapsed" {
            (render_rail(k1))
            (render_canvas(k1))
        }
    }
}

/// Styles are read from the source tree at runtime so edits can be
/// live-reloaded; the compiled-in copy is the fallback.
const CSS_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/k1/megarepl.css");

fn read_styles() -> String {
    std::fs::read_to_string(CSS_PATH)
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| include_str!("megarepl.css").to_string())
}

fn css_mtime() -> Option<SystemTime> {
    std::fs::metadata(CSS_PATH).ok().and_then(|m| m.modified().ok())
}

/// Polls the stylesheet's mtime and publishes the patched `<style>` element
fn css_watch_publisher(bus: &bus::EventBus) {
    let mut last_seen = css_mtime();
    loop {
        std::thread::sleep(Duration::from_millis(300));
        let mtime = css_mtime();
        if mtime == last_seen {
            continue;
        }
        last_seen = mtime;
        eprintln!("css changed; patching");
        let mut event = String::new();
        sse::patch_elements(
            &mut event,
            format_args!("<style id=\"page-styles\">\n{}\n</style>", read_styles()),
        );
        bus.publish(event);
    }
}

fn events_init_action() -> String {
    format!("@get('/events', {})", SignalFilter("^$"))
}

fn render_head() -> maud::Markup {
    use maud::PreEscaped;
    html! {
        head {
            meta charset="UTF-8";
            meta name="viewport" content="width=device-width, initial-scale=1.0";
            script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js" {}
            title { "k1 megarepl" }
            style #page-styles { (PreEscaped(read_styles())) }
        }
    }
}

fn render_site_header(active: &str) -> maud::Markup {
    html! {
        header .site-header {
            @if active == "repl" {
                button .rail-toggle
                    title="Collapse or expand the rail"
                    data-on:click="$_rail_collapsed = !$_rail_collapsed"
                    data-text="$_rail_collapsed ? '»' : '«'"
                    { "«" }
            }
            h1 { "k1 megarepl" }
            nav .site-nav {
                a .active[active == "repl"] href="/" { "repl" }
                a .active[active == "browse"] href="/ns" { "browse" }
            }
        }
    }
}

fn render_full_page(k1: &TypedProgram) -> maud::Markup {
    use maud::DOCTYPE;
    maud::html! {
        (DOCTYPE)
        html {
            (render_head())
            body data-init=(events_init_action()) {
                (render_site_header("repl"))
                (render_workspace(k1))
                footer .debug-footer {
                    code .signals-debug data-json-signals {}
                }
            }
        }
    }
}

/// The submitted code for the cell, from the request's signals payload
fn submitted_code(body: &[u8], cell_id: CellId) -> Option<String> {
    let signals: serde_json::Value = serde_json::from_slice(body).ok()?;
    let code = signals.get(cell_signal(cell_id, "code").to_string())?.as_str()?;
    Some(code.to_string())
}

/// Commands respond with no content; their effects go out on the event bus
fn publish_elements(bus: &bus::EventBus, elements: String) -> Response {
    let mut event = String::new();
    sse::patch_elements(&mut event, elements);
    bus.publish(event);
    Response::NoContent
}

/// Parses the `/<route>/{cell_id}` path segment and validates the cell exists.
fn parse_cell_id(k1: &TypedProgram, segment: Option<&str>) -> Option<CellId> {
    let cell_id: CellId = segment?.parse().ok()?;
    let exists = k1.megarepl_get_cell_opt(cell_id).is_some();
    if exists { Some(cell_id) } else { None }
}

fn handle_request(
    k1: &mut TypedProgram,
    bus: &bus::EventBus,
    method: &str,
    path: &str,
    body: &[u8],
) -> Response {
    let path = path.split('?').next().unwrap();
    let segments: Vec<&str> = path[1..].split('/').collect();
    eprintln!("handling {} {:?}", method, segments);
    let route = *segments.first().unwrap();
    if matches!(route, "cell" | "cell-watch" | "new-cell" | "widget") && method != "POST" {
        return Response::BadRequest;
    }
    match route {
        "" => {
            if k1.megarepl.is_none() {
                k1.megarepl_submit(None, String::new());
            }
            Response::Html(render_full_page(k1).0)
        }
        "new-cell" => {
            k1.megarepl_submit(None, String::new());
            publish_elements(bus, render_workspace(k1).0)
        }
        "cell" => {
            // /cell/{cell_id}
            let Some(cell_id) = parse_cell_id(k1, segments.get(1).copied()) else {
                return Response::BadRequest;
            };
            let Some(code) = submitted_code(body, cell_id) else {
                return Response::BadRequest;
            };
            k1.megarepl_submit(Some(cell_id), code);
            publish_elements(bus, render_workspace(k1).0)
        }
        "cell-watch" => {
            // /cell-watch/{cell_id}
            let Some(cell_id) = parse_cell_id(k1, segments.get(1).copied()) else {
                return Response::BadRequest;
            };
            k1.megarepl_toggle_watcher(cell_id);
            publish_elements(bus, render_workspace(k1).0)
        }
        "widget" => match segments.get(1).copied() {
            // /widget/new/let/{name} | /widget/new/watcher/{cell_id}
            Some("new") => {
                let data = match (segments.get(2).copied(), segments.get(3).copied()) {
                    (Some("let"), Some(name)) => match k1.ast.idents.lookup(name) {
                        Some(name_id) => WidgetDataSource::Binding(name_id),
                        None => return Response::BadRequest,
                    },
                    (Some("watcher"), Some(id)) => match parse_cell_id(k1, Some(id)) {
                        Some(cell_id) => WidgetDataSource::Watcher(cell_id),
                        None => return Response::BadRequest,
                    },
                    _ => return Response::BadRequest,
                };
                k1.megarepl_add_widget(data, WidgetKind::Plain);
                publish_elements(bus, render_workspace(k1).0)
            }
            // /widget/delete/{widget_id}
            Some("delete") => {
                let Some(widget_id) = segments.get(2).and_then(|s| s.parse().ok()) else {
                    return Response::BadRequest;
                };
                k1.megarepl_remove_widget(widget_id);
                publish_elements(bus, render_workspace(k1).0)
            }
            _ => Response::BadRequest,
        },
        "ns" => {
            let ns_path: Vec<&str> =
                segments[1..].iter().copied().filter(|s| !s.is_empty()).collect();
            match browse::render_page(k1, &ns_path) {
                Some(markup) => Response::Html(markup.0),
                None => Response::NotFound,
            }
        }
        "functions" => {
            let functions: Vec<String> = k1
                .function_iter()
                .map(|(_, tf)| k1.function_signature_to_string(&tf.signature()))
                .collect();
            Response::Text(functions.join("\n"))
        }
        _ => Response::NotFound,
    }
}
