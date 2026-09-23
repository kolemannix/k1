use serde::Serialize;

fn json_len<T: Serialize>(value: &T) -> usize {
    serde_json::to_string(value).unwrap().len()
}

fn main() {
    println!("{}", serialize_all());
}
