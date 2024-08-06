use std::{
    ffi::{CStr, CString},
    sync::{mpsc, Arc, RwLock},
};

use crate::typer::{FunctionId, TypedModule};
use raylib::prelude::*;

pub struct Gui {
    pub rl: raylib::RaylibHandle,
    pub rl_thread: raylib::RaylibThread,
    jb_mono: Font,
    module_handle: Arc<RwLock<Option<TypedModule>>>,
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
        module_handle: Arc<RwLock<Option<TypedModule>>>,
        compile_sender: mpsc::SyncSender<()>,
        run_sender: mpsc::SyncSender<()>,
    ) -> Self {
        let (mut rl, rl_thread) = raylib::init().size(800, 600).resizable().title("BFL").build();
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
        rl.gui_set_style(GuiControl::DEFAULT, GuiDefaultProperty::TEXT_SIZE as i32, 24);
        // Default TEXT_SPACING: 12
        dbg!(rl.gui_get_style(GuiControl::DEFAULT, GuiDefaultProperty::TEXT_SPACING as i32));
        rl.gui_set_style(GuiControl::DEFAULT, GuiDefaultProperty::TEXT_SPACING as i32, 1);
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
                Some(rstr!("Compile")),
            );
            if compile_clicked {
                self.compile_sender.send(()).unwrap();
            }
            let run_clicked = d.gui_label_button(
                Rectangle { x: 30.0, y: 80.0, width: 50.0, height: 30.0 },
                Some(rstr!("Run")),
            );
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

            let name = module.name();

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
                .map(|(_, f)| {
                    CString::new(module.ast.identifiers.get_name(f.name).to_string()).unwrap()
                })
                .collect();
            let mut function_name_cstrs = Vec::with_capacity(function_names.len());
            for name in &function_names {
                function_name_cstrs.push(name.as_c_str())
            }
            gui_list_view_fixed(
                &mut d,
                Rectangle { x: 100.0, y: 100.0, width: 300.0, height: 500.0 },
                &function_name_cstrs,
                &mut focus,
                &mut self.state.function_list_scroll_index,
                &mut active,
            );
            self.state.selected_function = active;
            if self.state.selected_function >= 0 {
                let fun = module.get_function(FunctionId(self.state.selected_function as u32));
                //let mut buf = [u8; 1024];
                // todo: text box
                d.gui_label(
                    Rectangle { x: 450.0, y: 100.0, width: 300.0, height: 300.0 },
                    Some(CString::new(module.function_to_string(fun, true)).unwrap().as_c_str()),
                );
            }
        }
    }
}

fn gui_list_view_fixed(
    d: &mut RaylibDrawHandle,
    bounds: impl Into<ffi::Rectangle>,
    text: &[&CStr],
    focus: &mut i32,
    scroll_index: &mut i32,
    active: &mut i32,
) -> i32 {
    // Actual order of the ffi call is scroll_index, active, focus
    d.gui_list_view_ex(bounds, text, scroll_index, active, focus)
}
