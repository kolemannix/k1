//! Broadcast channel from mutation handlers to every connected /events
//! stream. Publishing is a non-blocking channel send, so it is safe to do
//! while holding the compiler lock; slow or dead subscribers never stall a
//! request.

use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};

pub struct EventBus {
    subscribers: Mutex<Vec<Sender<Arc<String>>>>,
}

impl EventBus {
    pub fn new() -> EventBus {
        EventBus { subscribers: Mutex::new(Vec::new()) }
    }

    pub fn subscribe(&self) -> Receiver<Arc<String>> {
        let (sender, receiver) = channel();
        self.lock_subscribers().push(sender);
        receiver
    }

    /// Sends `event` to every subscriber, dropping the ones that are gone
    pub fn publish(&self, event: String) {
        let event = Arc::new(event);
        self.lock_subscribers().retain(|sender| sender.send(event.clone()).is_ok());
    }

    fn lock_subscribers(&self) -> std::sync::MutexGuard<'_, Vec<Sender<Arc<String>>>> {
        self.subscribers.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}
