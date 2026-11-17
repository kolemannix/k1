use serde::Serialize;

trait Kind {
    const KIND: &'static str;
}
macro_rules! kinds {
    ($($t:ty => $k:literal),*) => { $(impl Kind for $t { const KIND: &'static str = $k; })* };
}
kinds!(i8 => "int", i32 => "int", i64 => "int", u8 => "int", u16 => "int", u32 => "int", bool => "bool", &'static str => "str");

trait Schema {
    const FINGERPRINT: u32;
}

const fn mix(mut h: u32, s: &str) -> u32 {
    let b = s.as_bytes();
    let mut i = 0;
    while i < b.len() {
        h = (h ^ b[i] as u32).wrapping_mul(16777619);
        i += 1;
    }
    h
}

macro_rules! record {
    ($name:ident { $($f:ident: $t:ty),* }) => {
        #[derive(Serialize)]
        struct $name { $($f: $t),* }
        impl Schema for $name {
            const FINGERPRINT: u32 = {
                let mut h = 2166136261u32;
                $( h = mix(mix(mix(mix(h, stringify!($f)), ":"), <$t as Kind>::KIND), ","); )*
                h
            };
        }
    };
}

fn json_len<T: Serialize + Schema>(value: &T) -> usize {
    let body = serde_json::to_string(value).unwrap();
    format!("{{\"schema\":{},{}", T::FINGERPRINT, &body[1..]).len()
}

fn main() {
    println!("{}", serialize_all());
}
