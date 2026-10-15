---
title: Install
description: Install k1 with one line, or build it from source.
---

```bash
curl -fsSL https://k1-lang.org/install.sh | sh
```

The script downloads the latest release for your machine (macOS on Apple silicon, or Linux on x86-64), installs the compiler, language server, and module library under `~/.k1`, and tells you what to add to your PATH. Set `K1_VERSION` to a release tag, like `v2026.09.01`, to install that release instead of the latest.

```bash
k1 run examples/refchess
```

## From source

You need Rust, `just`, and a C toolchain.

```bash
git clone https://github.com/kolemannix/k1.git
cd k1
llvm/get_llvm.sh
just install
```

`get_llvm.sh` fetches the pinned LLVM build the compiler links against. `just install` builds the release compiler and language server, bundles them with the module library, and installs everything under `~/.k1`. Put `~/.k1/bin` on your PATH.
