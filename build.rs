use std::process::Command;

fn git(args: &[&str]) -> Option<String> {
    let out = Command::new("git").args(args).output().ok()?;
    if !out.status.success() {
        return None;
    }
    Some(String::from_utf8(out.stdout).ok()?.trim().to_string())
}

fn main() {
    let rev = git(&["rev-parse", "--short=12", "HEAD"]);
    let dirty = match git(&["status", "--porcelain", "--untracked-files=no"]) {
        Some(s) => !s.is_empty(),
        None => true,
    };

    let build_id = match (&rev, dirty) {
        (Some(rev), false) => rev.clone(),
        (Some(rev), true) => format!("{rev}-dirty"),
        (None, _) => "unknown-dirty".to_string(),
    };
    println!("cargo:rustc-env=K1_BUILD_ID={build_id}");
    println!("cargo:rustc-env=K1_BUILD_DIRTY={}", if dirty { "1" } else { "0" });

    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/index");
    if let Some(files) = git(&["ls-files"]) {
        for f in files.lines() {
            println!("cargo:rerun-if-changed={f}");
        }
    }
}
