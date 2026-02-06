pub enum Clock {
    Quanta(quanta::Clock),
    System,
}

impl Clock {
    pub fn new_counter() -> Self {
        Clock::Quanta(quanta::Clock::new())
    }
    pub fn new_system() -> Self {
        Clock::System
    }

    pub fn raw(&self) -> u64 {
        match self {
            Clock::Quanta(c) => c.raw(),
            Clock::System => {
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .expect("Time went backwards");
                now.as_nanos() as u64
            }
        }
    }

    pub fn elapsed_nanos(&self, since: u64) -> u64 {
        let now = self.raw();
        now.saturating_sub(since)
    }

    pub fn elapsed_ms(&self, since: u64) -> u64 {
        self.elapsed_nanos(since) / 1_000_000
    }

}
