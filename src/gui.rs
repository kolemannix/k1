use std::ffi::CString;

use crate::typer::TypedModule;
use raylib::prelude::*;

#[derive(Debug)]
pub struct Gui {
    pub enabled: bool,
    pub rl: raylib::RaylibHandle,
    pub rl_thread: raylib::RaylibThread,
}

impl Gui {
    pub fn init() -> Self {
        let (mut rl, rl_thread) = raylib::init().size(800, 600).title("BFL").build();
        rl.set_target_fps(60);
        rl.gui_enable();
        Self { enabled: true, rl, rl_thread }
    }

    pub fn render(&mut self, module: &TypedModule) {
        let mut d = self.rl.begin_drawing(&self.rl_thread);
        d.clear_background(Color::WHITE);
        d.draw_text(module.name(), 12, 12, 20, Color::BLACK);

        //let c = d.gui_list_view(0.0, text, scroll_index, active);
        for f in module.function_iter() {
            let name = CString::new(module.ast.identifiers.get_name(f.1.name)).unwrap();
            d.gui_label(Rectangle::new(0.0, 0.0, 100.0, 50.0), Some(name.as_c_str()));
        }
    }
}
