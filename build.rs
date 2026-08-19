use std::path::Path;
use std::process::Command;

fn git(args: &[&str]) -> Option<String> {
    let out = Command::new("git").args(args).output().ok()?;
    if !out.status.success() {
        return None;
    }
    Some(String::from_utf8(out.stdout).ok()?.trim().to_string())
}

const FNV_OFFSET: u64 = 0xcbf29ce484222325;

fn fnv1a64(mut h: u64, bytes: &[u8]) -> u64 {
    for b in bytes {
        h ^= *b as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

const BINARY_INPUTS: &[&str] = &["src", "build.rs", "Cargo.toml", "Cargo.lock"];

fn with_binary_inputs<'a>(args: &[&'a str]) -> Vec<&'a str> {
    let mut v = args.to_vec();
    v.push("--");
    v.extend_from_slice(BINARY_INPUTS);
    v
}

/// keep k1 free of C++ runtime dependency.
fn link_static_cpp_runtime() {
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let out = Command::new(&cc)
        .arg("-print-file-name=libstdc++.a")
        .output()
        .unwrap_or_else(|e| panic!("Failed to ask {cc} where libstdc++.a lives: {e}"));
    let path = std::path::PathBuf::from(String::from_utf8(out.stdout).unwrap().trim());
    let Some(dir) = path.parent().filter(|_| path.is_file()) else {
        panic!("{cc} could not find libstdc++.a; install the GCC C++ runtime's static library")
    };
    println!("cargo:rustc-link-search=native={}", dir.display());
    println!("cargo:rustc-link-lib=static=stdc++");
}

fn main() {
    let rev = git(&["rev-parse", "--short=12", "HEAD"]);
    let tracked_dirty =
        match git(&with_binary_inputs(&["status", "--porcelain", "--untracked-files=no"])) {
            Some(s) => !s.is_empty(),
            None => true,
        };
    let untracked_src = git(&["ls-files", "--others", "--exclude-standard", "src"])
        .map(|s| s.lines().map(str::to_string).collect::<Vec<_>>())
        .unwrap_or_default();
    let dirty = tracked_dirty || !untracked_src.is_empty();

    let build_id = match (&rev, dirty) {
        (Some(rev), false) => rev.clone(),
        (Some(rev), true) => {
            // For dirty builds, incorporate the untracked sources
            let diff = git(&with_binary_inputs(&["diff", "HEAD"])).unwrap_or_default();
            let mut h = fnv1a64(FNV_OFFSET, diff.as_bytes());
            for f in &untracked_src {
                h = fnv1a64(h, f.as_bytes());
                h = fnv1a64(h, &std::fs::read(f).unwrap_or_default());
            }
            format!("{rev}-dirty-{h:016x}")
        }
        (None, _) => "unknown-dirty".to_string(),
    };
    println!("cargo:rustc-env=K1_BUILD_ID={build_id}");

    println!("cargo:rerun-if-changed=.git/HEAD");
    if let Some(head_ref) = git(&["symbolic-ref", "-q", "HEAD"]) {
        let loose = format!(".git/{head_ref}");
        if Path::new(&loose).exists() {
            println!("cargo:rerun-if-changed={loose}");
        } else if Path::new(".git/packed-refs").exists() {
            println!("cargo:rerun-if-changed=.git/packed-refs");
        }
    }
    if let Some(files) = git(&with_binary_inputs(&["ls-files"])) {
        for f in files.lines() {
            println!("cargo:rerun-if-changed={f}");
        }
    }
    for f in &untracked_src {
        println!("cargo:rerun-if-changed={f}");
    }

    let llvm_prefix = std::env::var("LLVM_SYS_211_PREFIX")
        .expect("LLVM_SYS_211_PREFIX must be set");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_211_PREFIX");
    let mut lld_shim = cc::Build::new();
    lld_shim
        .cpp(true)
        .std("c++17")
        .file("src/lld_shim.cpp")
        .include(format!("{llvm_prefix}/include"))
        .flag_if_supported("-fno-rtti")
        .flag_if_supported("-fno-exceptions");
    let is_linux = std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("linux");
    if is_linux {
        // cc would emit a dynamic -lstdc++ of its own; we supply a static one
        lld_shim.cpp_link_stdlib(None);
    }
    lld_shim.compile("k1_lld_shim");
    if is_linux {
        link_static_cpp_runtime();
    }
    println!("cargo:rustc-link-search=native={llvm_prefix}/lib");
    println!("cargo:rustc-link-lib=static=lldWasm");
    println!("cargo:rustc-link-lib=static=lldCommon");
}
