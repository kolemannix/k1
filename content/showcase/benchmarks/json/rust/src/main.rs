use serde::Deserialize;
use serde_json::Value;
use std::time::Instant;

#[derive(Deserialize)]
struct User { id: u64, screen_name: String, followers_count: u32, friends_count: u32, verified: bool }
#[derive(Deserialize)]
struct Status { id: u64, text: String, user: User, retweet_count: u32, favorite_count: u32, lang: String }
#[derive(Deserialize)]
struct Twitter { statuses: Vec<Status> }
#[derive(Deserialize)]
struct Geometry { coordinates: Vec<Vec<[f64; 2]>> }
#[derive(Deserialize)]
struct Feature { geometry: Geometry }
#[derive(Deserialize)]
struct Canada { features: Vec<Feature> }

fn best_seconds(iters: usize, mut f: impl FnMut()) -> f64 {
    let mut best = f64::MAX;
    for _ in 0..iters {
        let t0 = Instant::now();
        f();
        let s = t0.elapsed().as_secs_f64();
        if s < best { best = s; }
    }
    best
}

fn report(name: &str, bytes: usize, seconds: f64) {
    println!("rust  {name}: {:.3} GB/s", bytes as f64 / seconds / 1e9);
}

fn main() {
    let dir = std::env::args().nth(1).expect("usage: bench <corpus dir>");
    let mut sink = 0usize;
    for name in ["twitter", "canada", "citm_catalog"] {
        let text = std::fs::read_to_string(format!("{dir}/{name}.json")).unwrap();
        report(&format!("{name} parse"), text.len(), best_seconds(20, || {
            let v: Value = serde_json::from_str(&text).unwrap();
            sink += v.is_object() as usize;
        }));
        if name == "twitter" {
            report("twitter typed", text.len(), best_seconds(20, || {
                let t: Twitter = serde_json::from_str(&text).unwrap();
                sink += t.statuses.len();
            }));
        }
        if name == "canada" {
            report("canada typed", text.len(), best_seconds(20, || {
                let c: Canada = serde_json::from_str(&text).unwrap();
                sink += c.features.len();
            }));
        }
        let doc: Value = serde_json::from_str(&text).unwrap();
        let out_len = serde_json::to_string(&doc).unwrap().len();
        report(&format!("{name} write"), out_len, best_seconds(20, || {
            sink += serde_json::to_string(&doc).unwrap().len();
        }));
    }
    std::process::exit((sink == 1) as i32);
}
