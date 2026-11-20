---
title: Install
description: Install k1 with one line, or build it from source.
---

```bash
curl -fsSL https://k1lang.org/install.sh | sh
```

The script downloads the latest release for your machine.

Installs the compiler, language server, and module library under `~/.k1`, and tells you what to add to your PATH. Set `K1_VERSION` to a release tag, like `v2026.09.01`, to install that release instead of the latest.

As of `v0.1.0`, here is approximately what you get:

```sh
├── bin
│   ├── k1
│   ├── k1_test
│   ├── k1bindgen
│   └── k1lsp
├── lib
│   └── clang
├── modules
│   ├── brotli
│   ├── core
│   ├── freetype
│   ├── harfbuzz
│   ├── http
│   ├── sdl3
│   ├── sqlite
│   ├── stb
│   ├── std
│   └── uv
```

```bash
k1 run examples/refchess
```

## From source (not recommended for users)

You'll need Rust and `just`, plus podman on Linux, or cmake and clang on macOS.

```bash
git clone https://github.com/kolemannix/k1.git
cd k1
(cd llvm && ./get_llvm.sh)   # macOS only
just install
```

On macOS, `get_llvm.sh` builds the pinned, patched LLVM the compiler links against. On Linux, `just install` builds inside a glibc 2.34 container that makes its own LLVM build. `just install` builds the release compiler and language server, bundles them with the module library, and installs everything under `~/.k1`. Put `~/.k1/bin` on your PATH.
