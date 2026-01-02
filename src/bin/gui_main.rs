// Copyright (c) 2025 knix
// All rights reserved.

use std::{
    path::Path,
    sync::{Arc, RwLock, mpsc},
    thread::{self, JoinHandle},
};

use clap::Parser;
use k1::{
    compiler,
    typer::{FunctionId, TypedProgram},
};
use log::info;
use raylib::prelude::*;

pub struct Gui {
    pub rl: raylib::RaylibHandle,
    pub rl_thread: raylib::RaylibThread,
    jb_mono: Font,
    module_handle: Arc<RwLock<Option<TypedProgram>>>,
    compile_sender: mpsc::SyncSender<()>,
    run_sender: mpsc::SyncSender<()>,
    state: GuiState,
}

#[derive(Default, Debug)]
pub struct GuiState {
    selected_function: i32,
    function_list_scroll_index: i32,
}

impl Gui {
    pub fn init(
        module_handle: Arc<RwLock<Option<TypedProgram>>>,
        compile_sender: mpsc::SyncSender<()>,
        run_sender: mpsc::SyncSender<()>,
    ) -> Self {
        let (mut rl, rl_thread) = raylib::init().size(800, 600).resizable().title("K1").build();
        rl.set_target_fps(60);
        rl.gui_enable();

        let jb_mono: Font = rl
            .load_font_ex(
                &rl_thread,
                "/Users/knix/Library/Fonts/JetBrainsMono-Regular.ttf",
                96,
                None,
            )
            .expect("font load");
        rl.gui_set_font(&jb_mono);
        rl.gui_set_style(GuiControl::DEFAULT, GuiDefaultProperty::TEXT_SIZE, 24);
        // Default TEXT_SPACING: 12
        dbg!(rl.gui_get_style(GuiControl::DEFAULT, GuiDefaultProperty::TEXT_SPACING));
        rl.gui_set_style(GuiControl::DEFAULT, GuiDefaultProperty::TEXT_SPACING, 1);
        // let arial: Font =
        // gui.rl.load_font(&thread, "/Users/knix/Library/Fonts/Arial.ttf").expect("font load");
        // rl.gui_set_font(&jb_mono);

        Self {
            rl,
            rl_thread,
            jb_mono,
            module_handle,
            compile_sender,
            run_sender,
            state: GuiState::default(),
        }
    }

    pub fn run_loop(&mut self) {
        let rl = &mut self.rl;
        let mut focus = -1;
        let mut active = -1;
        while !rl.window_should_close() {
            let frame_time = rl.get_frame_time();
            let mut d = rl.begin_drawing(&self.rl_thread);
            d.gui_set_font(&self.jb_mono);

            d.clear_background(raylib::color::Color::WHITE);
            d.draw_fps(d.get_screen_width() - 25, 10);

            let compile_clicked = d.gui_label_button(
                Rectangle { x: 30.0, y: 50.0, width: 50.0, height: 30.0 },
                "Compile",
            );
            if compile_clicked {
                self.compile_sender.send(()).unwrap();
            }
            let run_clicked = d
                .gui_label_button(Rectangle { x: 30.0, y: 80.0, width: 50.0, height: 30.0 }, "Run");
            if run_clicked {
                self.run_sender.send(()).unwrap();
            }

            let lock = self.module_handle.try_read();
            let Ok(module_option_guard) = lock else {
                continue;
            };
            let Some(module) = module_option_guard.as_ref() else {
                println!("waiting on module... {}", frame_time);
                continue;
            };

            let name = module.program_name();

            d.draw_text_ex(
                &self.jb_mono,
                &format!("Module: {}", name),
                Vector2 { x: 12.0, y: 6.0 },
                48.0,
                1.0,
                raylib::color::Color::DIMGRAY,
            );

            let function_names: Vec<_> = module
                .function_iter()
                .map(|(_, f)| module.ast.idents.get_name(f.name).to_string())
                .collect();
            gui_list_view_fixed(
                &mut d,
                Rectangle { x: 100.0, y: 100.0, width: 300.0, height: 500.0 },
                &function_names,
                &mut focus,
                &mut self.state.function_list_scroll_index,
                &mut active,
            );
            self.state.selected_function = active;
            if self.state.selected_function >= 0 {
                let fun = module.get_function(
                    FunctionId::from_u32(self.state.selected_function as u32).unwrap(),
                );
                //let mut buf = [u8; 1024];
                // todo: text box
                d.gui_label(
                    Rectangle { x: 450.0, y: 100.0, width: 300.0, height: 300.0 },
                    &module.function_to_string(fun, true),
                );
            }
        }
    }
}

fn gui_list_view_fixed(
    d: &mut RaylibDrawHandle,
    bounds: impl Into<ffi::Rectangle>,
    text: &[impl AsRef<str>],
    focus: &mut i32,
    scroll_index: &mut i32,
    active: &mut i32,
) -> i32 {
    // Actual order of the ffi call is scroll_index, active, focus
    d.gui_list_view_ex(bounds, text.iter(), scroll_index, active, focus)
}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let args = k1::compiler::Args::try_parse().unwrap();
    info!("{:#?}", args);

    info!("k1 Compiler v0.1.0");

    let out_dir = Path::new(".k1-out");

    // If gui mode:
    // - Create a new thread to compile the module
    // - Run gui loop from this thread
    // - On frame or channel push, get module snapshot and render it
    // - Put module inside a RwLock, just try to read it from the gui thread

    let module_handle: Arc<RwLock<Option<TypedProgram>>> = Arc::new(RwLock::new(None));

    // Some thoughts: instead of a RwLock, we could just use a channel to send the module to the gui thread
    // Or we could just spawn a thread whenever we need to compile a module, and then send the module back to the main thread
    // Lots of better ways to share the data but this is working for now
    let shared_module_clone = module_handle.clone();
    let args_clone = args.clone();
    let (compile_sender, compile_receiver) = std::sync::mpsc::sync_channel::<()>(16);
    compile_sender.send(()).unwrap();
    let _compile_thread: JoinHandle<()> = thread::Builder::new()
        .name("compile".to_string())
        .spawn(move || {
            while let Ok(()) = compile_receiver.recv() {
                let Ok(module) = compiler::compile_program(&args_clone, out_dir) else {
                    return;
                };
                shared_module_clone.write().unwrap().replace(module);

                let mut module_lock = shared_module_clone.write().unwrap();
                let module = module_lock.as_mut().unwrap();
                let llvm_ctx = inkwell::context::Context::create();
                let _codegen =
                    match compiler::codegen_module(&args_clone, &llvm_ctx, module, out_dir, true) {
                        Ok(codegen) => codegen,
                        Err(err) => {
                            eprintln!("Codegen error: {}", err);
                            return;
                        }
                    };
            }
        })
        .unwrap();

    let (run_sender, run_receiver) = std::sync::mpsc::sync_channel::<()>(16);
    let run_module_handle = module_handle.clone();
    let _run_thread: JoinHandle<()> = thread::Builder::new()
        .name("run".to_string())
        .spawn(move || {
            while let Ok(()) = run_receiver.recv() {
                let module_read = run_module_handle.read().unwrap();
                let Some(module) = module_read.as_ref() else {
                    println!("Cannot run; no module");
                    continue;
                };
                compiler::run_compiled_program(out_dir, module.program_name());
            }
        })
        .unwrap();

    let mut gui = Gui::init(module_handle.clone(), compile_sender, run_sender);
    gui.run_loop();
    Ok(())
}
