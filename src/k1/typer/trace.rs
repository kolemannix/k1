//! The stack of what the compiler is currently compiling, separate from the
//! call stack, recorded as a flat trace of frames for profiling.

use crate::bc::OPCODE_COUNT;
use crate::clock::Clock;
use crate::ir::IrUnitId;
use crate::typer::{EvalExprContext, TypedProgram};
use crate::vpool::VPool;
use crate::{SV8, nz_u32_id, static_assert_size};

nz_u32_id!(FrameId);

pub const FRAME_FLAG_SPECULATIVE: u8 = 1;
pub const FRAME_FLAG_EXPR_UNIT: u8 = 2;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TraceKind {
    ModuleDiscover,
    ModuleRead,
    Lex,
    Parse,
    SetupFn,
    ModuleCompile,
    TyperPass,
    SnapRestore,
    SnapRestoreSection,
    SnapRoundtrip,
    SnapStore,
    FunctionTypecheck,
    GlobalEval,
    FunctionSpecialize,
    TypeInstantiate,
    TypeInfer,
    StaticExec,
    Metaprogram,
    MacroCall,
    VmValueFerry,
    IrLower,
    IrOptimize,
    IrInline,
    IrSimplify,
    IrCfgCompute,
    Bcgen,
    VmRun,
    CodegenPrepare,
    Codegen,
    LlvmPasses,
    Thinlto,
    Link,
    Archive,
    ReloadDylib,
}

impl TraceKind {
    pub const ALL: [TraceKind; 34] = [
        TraceKind::ModuleDiscover,
        TraceKind::ModuleRead,
        TraceKind::Lex,
        TraceKind::Parse,
        TraceKind::SetupFn,
        TraceKind::ModuleCompile,
        TraceKind::TyperPass,
        TraceKind::SnapRestore,
        TraceKind::SnapRestoreSection,
        TraceKind::SnapRoundtrip,
        TraceKind::SnapStore,
        TraceKind::FunctionTypecheck,
        TraceKind::GlobalEval,
        TraceKind::FunctionSpecialize,
        TraceKind::TypeInstantiate,
        TraceKind::TypeInfer,
        TraceKind::StaticExec,
        TraceKind::Metaprogram,
        TraceKind::MacroCall,
        TraceKind::VmValueFerry,
        TraceKind::IrLower,
        TraceKind::IrOptimize,
        TraceKind::IrInline,
        TraceKind::IrSimplify,
        TraceKind::IrCfgCompute,
        TraceKind::Bcgen,
        TraceKind::VmRun,
        TraceKind::CodegenPrepare,
        TraceKind::Codegen,
        TraceKind::LlvmPasses,
        TraceKind::Thinlto,
        TraceKind::Link,
        TraceKind::Archive,
        TraceKind::ReloadDylib,
    ];

    pub fn name(self) -> &'static str {
        match self {
            TraceKind::ModuleDiscover => "discover",
            TraceKind::ModuleRead => "read",
            TraceKind::Lex => "lex",
            TraceKind::Parse => "parse",
            TraceKind::SetupFn => "setup",
            TraceKind::ModuleCompile => "module",
            TraceKind::TyperPass => "typer pass",
            TraceKind::SnapRestore => "restore",
            TraceKind::SnapRestoreSection => "restore section",
            TraceKind::SnapRoundtrip => "snapshot roundtrip",
            TraceKind::SnapStore => "snapshot store",
            TraceKind::FunctionTypecheck => "typecheck",
            TraceKind::GlobalEval => "global",
            TraceKind::FunctionSpecialize => "specialize",
            TraceKind::TypeInstantiate => "instantiate",
            TraceKind::TypeInfer => "infer",
            TraceKind::StaticExec => "static exec",
            TraceKind::Metaprogram => "meta",
            TraceKind::MacroCall => "macro",
            TraceKind::VmValueFerry => "ferry",
            TraceKind::IrLower => "lower",
            TraceKind::IrOptimize => "optimize",
            TraceKind::IrInline => "inline",
            TraceKind::IrSimplify => "simplify",
            TraceKind::IrCfgCompute => "cfg",
            TraceKind::Bcgen => "bcgen",
            TraceKind::VmRun => "run",
            TraceKind::CodegenPrepare => "prepare",
            TraceKind::Codegen => "codegen",
            TraceKind::LlvmPasses => "passes",
            TraceKind::Thinlto => "thinlto",
            TraceKind::Link => "link",
            TraceKind::Archive => "archive",
            TraceKind::ReloadDylib => "reload",
        }
    }

    /// Essential kinds are pushed on every compile because their open frames
    /// guard re-entry through `on_stack`/`stack_keys`; the rest exist only for
    /// the trace and are skipped when not recording
    pub fn is_essential(self) -> bool {
        match self {
            TraceKind::ModuleDiscover
            | TraceKind::GlobalEval
            | TraceKind::IrLower
            | TraceKind::Bcgen => true,
            TraceKind::ModuleRead
            | TraceKind::Lex
            | TraceKind::Parse
            | TraceKind::SetupFn
            | TraceKind::ModuleCompile
            | TraceKind::TyperPass
            | TraceKind::SnapRestore
            | TraceKind::SnapRestoreSection
            | TraceKind::SnapRoundtrip
            | TraceKind::SnapStore
            | TraceKind::FunctionTypecheck
            | TraceKind::FunctionSpecialize
            | TraceKind::TypeInstantiate
            | TraceKind::TypeInfer
            | TraceKind::StaticExec
            | TraceKind::Metaprogram
            | TraceKind::MacroCall
            | TraceKind::VmValueFerry
            | TraceKind::IrOptimize
            | TraceKind::IrInline
            | TraceKind::IrSimplify
            | TraceKind::IrCfgCompute
            | TraceKind::VmRun
            | TraceKind::CodegenPrepare
            | TraceKind::Codegen
            | TraceKind::LlvmPasses
            | TraceKind::Thinlto
            | TraceKind::Link
            | TraceKind::Archive
            | TraceKind::ReloadDylib => false,
        }
    }

    /// Kinds worth a line in the live stack: coarse enough not to flicker
    pub fn is_coarse(self) -> bool {
        matches!(
            self,
            TraceKind::ModuleDiscover
                | TraceKind::ModuleRead
                | TraceKind::Parse
                | TraceKind::SetupFn
                | TraceKind::ModuleCompile
                | TraceKind::TyperPass
                | TraceKind::SnapRestore
                | TraceKind::SnapRestoreSection
                | TraceKind::SnapRoundtrip
                | TraceKind::SnapStore
                | TraceKind::FunctionTypecheck
                | TraceKind::GlobalEval
                | TraceKind::FunctionSpecialize
                | TraceKind::StaticExec
                | TraceKind::Metaprogram
                | TraceKind::MacroCall
                | TraceKind::IrLower
                | TraceKind::IrOptimize
                | TraceKind::VmRun
                | TraceKind::CodegenPrepare
                | TraceKind::Codegen
                | TraceKind::LlvmPasses
                | TraceKind::Thinlto
                | TraceKind::Link
                | TraceKind::Archive
                | TraceKind::ReloadDylib
        )
    }

    /// What `data_count` counts for this kind, if anything
    pub fn data_label(self) -> Option<&'static str> {
        match self {
            TraceKind::VmRun => Some("instrs"),
            TraceKind::IrOptimize => Some("insts created"),
            TraceKind::IrInline => Some("inlines"),
            TraceKind::IrSimplify => Some("passes"),
            TraceKind::CodegenPrepare => Some("reachable fns"),
            TraceKind::Codegen => Some("fns"),
            _ => None,
        }
    }
}

pub const RESTORE_SECTIONS: [&str; 11] = [
    "load",
    "ast",
    "mem",
    "modules",
    "functions",
    "types",
    "globals",
    "exprs",
    "scopes",
    "namespaces",
    "ir",
];

pub const PASS_NAMES: [&str; 5] = ["uses", "namespaces", "types", "declarations", "bodies"];

pub fn restore_section(name: &str) -> u32 {
    RESTORE_SECTIONS.iter().position(|s| *s == name).unwrap() as u32
}

#[derive(Clone, Copy)]
pub struct TraceFrame {
    pub clock_start: u64,
    pub clock_end: u64,
    pub child_ticks: u64,
    pub data_count: u64,
    pub key: u32,
    pub parent_frame: Option<FrameId>,
    pub requester_frame: Option<FrameId>,
    pub kind: TraceKind,
    pub flags: u8,
}
static_assert_size!(TraceFrame, 48);

impl TraceFrame {
    pub fn ticks(&self) -> u64 {
        self.clock_end.saturating_sub(self.clock_start)
    }

    pub fn exclusive_ticks(&self) -> u64 {
        self.ticks().saturating_sub(self.child_ticks)
    }

    pub fn is_speculative(&self) -> bool {
        self.flags & FRAME_FLAG_SPECULATIVE != 0
    }

    pub fn is_expr_unit(&self) -> bool {
        self.flags & FRAME_FLAG_EXPR_UNIT != 0
    }
}

pub struct Trace {
    pub clock: Clock,
    pub clock_start: u64,
    pub frames: VPool<TraceFrame, FrameId>,
    stack: Vec<FrameId>,
    /// Off: no clock reads, and only essential kinds are pushed
    pub recording: bool,
    pub opcode_counts: [i64; OPCODE_COUNT as usize],
    /// Redraw the stack on stderr as it changes (chatty on a tty)
    pub live: bool,
    next_draw: u64,
    draw_interval: u64,
}

const LIVE_DRAW_INTERVAL_NANOS: u64 = 60_000_000;

fn unit_key(unit: IrUnitId) -> (u32, u8) {
    match unit {
        IrUnitId::Function(function_id) => (function_id.as_u32(), 0),
        IrUnitId::Expr(expr_id) => (expr_id.as_u32(), FRAME_FLAG_EXPR_UNIT),
    }
}

impl Trace {
    pub fn make(clock: Clock, live: bool, recording: bool) -> Trace {
        Trace {
            clock,
            clock_start: clock.raw(),
            frames: VPool::make_with_hint("trace_frames", 16 * 1024),
            stack: Vec::with_capacity(64),
            recording,
            opcode_counts: [0; OPCODE_COUNT as usize],
            live,
            next_draw: 0,
            draw_interval: clock.nanos_to_ticks(LIVE_DRAW_INTERVAL_NANOS),
        }
    }

    pub fn stack(&self) -> &[FrameId] {
        &self.stack
    }

    /// True at most once per draw interval, and never when not live
    pub fn draw_due(&mut self) -> bool {
        if !self.live {
            return false;
        }
        let now = self.clock.raw();
        if now < self.next_draw {
            return false;
        }
        self.next_draw = now + self.draw_interval;
        true
    }

    pub fn push(&mut self, kind: TraceKind, key: u32, flags: u8) -> Option<FrameId> {
        if !self.recording && !kind.is_essential() {
            return None;
        }
        let parent_frame = self.top();
        let inherited = match parent_frame {
            Some(parent) => self.frames.get(parent).flags & FRAME_FLAG_SPECULATIVE,
            None => 0,
        };
        let clock_start = if self.recording { self.clock.raw() } else { 0 };
        let id = self.frames.add(TraceFrame {
            clock_start,
            clock_end: 0,
            child_ticks: 0,
            data_count: 0,
            key,
            parent_frame,
            requester_frame: None,
            kind,
            flags: flags | inherited,
        });
        self.stack.push(id);
        Some(id)
    }

    pub fn push_unit(
        &mut self,
        kind: TraceKind,
        unit: IrUnitId,
        requester_frame: Option<FrameId>,
    ) -> Option<FrameId> {
        let (key, flags) = unit_key(unit);
        let id = self.push(kind, key, flags)?;
        self.frames.get_mut(id).requester_frame = requester_frame;
        Some(id)
    }

    pub fn pop(&mut self, frame: Option<FrameId>) {
        let Some(frame) = frame else { return };
        let popped = self.stack.pop();
        debug_assert_eq!(popped, Some(frame));
        if !self.recording {
            return;
        }
        let clock_end = self.clock.raw();
        let f = self.frames.get_mut(frame);
        f.clock_end = clock_end;
        let ticks = f.ticks();
        if let Some(parent) = f.parent_frame {
            self.frames.get_mut(parent).child_ticks += ticks;
        }
    }

    pub fn record(
        &mut self,
        kind: TraceKind,
        key: u32,
        parent_frame: Option<FrameId>,
        clock_start: u64,
        clock_end: u64,
        data_count: u64,
        flags: u8,
    ) -> FrameId {
        let frame = TraceFrame {
            clock_start,
            clock_end,
            child_ticks: 0,
            data_count,
            key,
            parent_frame,
            requester_frame: None,
            kind,
            flags,
        };
        if let Some(parent) = parent_frame {
            self.frames.get_mut(parent).child_ticks += frame.ticks();
        }
        self.frames.add(frame)
    }

    pub fn top(&self) -> Option<FrameId> {
        self.stack.last().copied()
    }

    pub fn set_top_count(&mut self, n: u64) {
        if let Some(top) = self.top() {
            self.frames.get_mut(top).data_count = n;
        }
    }

    pub fn on_stack(&self, kind: TraceKind, key: u32) -> bool {
        for id in self.stack.iter().rev() {
            let frame = self.frames.get(*id);
            if frame.kind == kind && frame.key == key {
                return true;
            }
        }
        false
    }

    /// Keys of the open frames of `kind`, outermost first
    pub fn stack_keys(&self, kind: TraceKind) -> SV8<u32> {
        let mut keys = SV8::new();
        for id in self.stack.iter() {
            let frame = self.frames.get(*id);
            if frame.kind == kind {
                keys.push(frame.key);
            }
        }
        keys
    }

    pub fn wall_ticks(&self) -> u64 {
        self.clock.raw().saturating_sub(self.clock_start)
    }

    pub fn nanos(&self, ticks: u64) -> u64 {
        self.clock.ticks_to_nanos(ticks)
    }
}

#[cfg(unix)]
fn terminal_columns() -> usize {
    let mut size: libc::winsize = unsafe { std::mem::zeroed() };
    let ok = unsafe { libc::ioctl(libc::STDERR_FILENO, libc::TIOCGWINSZ, &mut size) } == 0;
    if ok && size.ws_col > 0 { size.ws_col as usize } else { 120 }
}

#[cfg(not(unix))]
fn terminal_columns() -> usize {
    120
}

impl TypedProgram {
    pub fn traced<T>(
        &mut self,
        kind: TraceKind,
        key: u32,
        flags: u8,
        f: impl FnOnce(&mut TypedProgram) -> T,
    ) -> T {
        let frame = self.trace_push(kind, key, flags);
        let result = f(self);
        self.trace_pop(frame);
        result
    }

    pub fn trace_push(&mut self, kind: TraceKind, key: u32, flags: u8) -> Option<FrameId> {
        let frame = self.trace.push(kind, key, flags);
        if self.trace.draw_due() {
            self.trace_draw();
        }
        frame
    }

    pub fn trace_push_unit(
        &mut self,
        kind: TraceKind,
        unit: IrUnitId,
        requester_frame: Option<FrameId>,
    ) -> Option<FrameId> {
        let frame = self.trace.push_unit(kind, unit, requester_frame);
        if self.trace.draw_due() {
            self.trace_draw();
        }
        frame
    }

    pub fn trace_pop(&mut self, frame: Option<FrameId>) {
        self.trace.pop(frame);
        if self.trace.draw_due() {
            self.trace_draw();
        }
    }

    /// One line, coarse frames only, outermost first, cut to the terminal width
    fn trace_draw(&self) {
        use std::io::Write;
        let columns = terminal_columns();
        let mut line = String::with_capacity(columns);
        for id in self.trace.stack() {
            let frame = self.trace.frames.get(*id);
            if !frame.kind.is_coarse() {
                continue;
            }
            if !line.is_empty() {
                line.push_str(" › ");
            }
            line.push_str(&self.frame_title(frame));
        }
        if line.chars().count() >= columns {
            let mut cut = String::with_capacity(columns);
            for c in line.chars().take(columns.saturating_sub(2)) {
                cut.push(c);
            }
            cut.push('…');
            line = cut;
        }
        let mut stderr = std::io::stderr().lock();
        let _ = write!(stderr, "\r\x1b[2K{line}");
        let _ = stderr.flush();
    }

    /// Clear the live line before other stderr output
    pub fn trace_live_clear(&self) {
        use std::io::Write;
        if self.trace.live {
            let mut stderr = std::io::stderr().lock();
            let _ = write!(stderr, "\r\x1b[2K");
            let _ = stderr.flush();
        }
    }
}

impl EvalExprContext {
    pub fn trace_flags(&self) -> u8 {
        if self.is_inference() || self.is_test_compile() { FRAME_FLAG_SPECULATIVE } else { 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn frame(trace: &Trace, id: FrameId) -> TraceFrame {
        *trace.frames.get(id)
    }

    #[test]
    fn exclusive_is_inclusive_minus_children() {
        let mut trace = Trace::make(Clock::new(), false, true);
        let outer = trace.push(TraceKind::FunctionTypecheck, 1, 0);
        let inner = trace.push(TraceKind::TypeInfer, 2, 0);
        std::thread::sleep(std::time::Duration::from_millis(2));
        trace.pop(inner);
        std::thread::sleep(std::time::Duration::from_millis(2));
        trace.pop(outer);
        let outer = frame(&trace, outer.unwrap());
        let inner = frame(&trace, inner.unwrap());
        assert_eq!(outer.child_ticks, inner.ticks());
        assert_eq!(outer.exclusive_ticks(), outer.ticks() - inner.ticks());
        assert!(trace.nanos(outer.exclusive_ticks()) >= 1_500_000);
    }

    #[test]
    fn recorded_children_charge_parent_and_saturate() {
        let mut trace = Trace::make(Clock::new(), false, true);
        let parent = trace.push(TraceKind::Codegen, 0, 0);
        let start = trace.clock.raw();
        std::thread::sleep(std::time::Duration::from_millis(1));
        let end = trace.clock.raw();
        trace.record(TraceKind::Codegen, 1, parent, start, end, 3, 0);
        trace.record(TraceKind::Codegen, 2, parent, start, end, 4, 0);
        trace.pop(parent);
        let parent = frame(&trace, parent.unwrap());
        assert_eq!(parent.child_ticks, 2 * (end - start));
        assert_eq!(parent.exclusive_ticks(), 0);
    }

    #[test]
    fn speculative_flag_inherits_and_stack_scans() {
        let mut trace = Trace::make(Clock::new(), false, true);
        let a = trace.push(TraceKind::StaticExec, 7, FRAME_FLAG_SPECULATIVE);
        let b = trace.push(TraceKind::IrLower, 9, 0);
        assert!(frame(&trace, b.unwrap()).is_speculative());
        assert!(trace.on_stack(TraceKind::IrLower, 9));
        assert!(!trace.on_stack(TraceKind::IrLower, 7));
        assert_eq!(trace.stack_keys(TraceKind::StaticExec).as_slice(), &[7]);
        trace.set_top_count(5);
        assert_eq!(frame(&trace, b.unwrap()).data_count, 5);
        trace.pop(b);
        trace.pop(a);
        assert_eq!(trace.top(), None);
    }

    #[test]
    fn not_recording_keeps_essential_frames_without_clocks() {
        let mut trace = Trace::make(Clock::new(), false, false);
        let typecheck = trace.push(TraceKind::FunctionTypecheck, 1, 0);
        let lower = trace.push(TraceKind::IrLower, 9, 0);
        assert_eq!(typecheck, None);
        assert!(lower.is_some());
        assert!(trace.on_stack(TraceKind::IrLower, 9));
        assert_eq!(trace.frames.len(), 1);
        trace.pop(lower);
        trace.pop(typecheck);
        assert_eq!(trace.top(), None);
        assert_eq!(frame(&trace, lower.unwrap()).ticks(), 0);
    }

    #[test]
    #[should_panic]
    #[cfg(debug_assertions)]
    fn pop_of_non_top_frame_panics() {
        let mut trace = Trace::make(Clock::new(), false, true);
        let a = trace.push(TraceKind::Parse, 1, 0);
        let _b = trace.push(TraceKind::Parse, 2, 0);
        trace.pop(a);
    }
}
