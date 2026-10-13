---
title: Install
description: Build k1 from source.
---

k1 builds from source today. You need Rust, `just`, and a C toolchain.

```bash
git clone https://github.com/kolemannix/k1.git
cd k1
llvm/get_llvm.sh
just install
```

`get_llvm.sh` fetches the pinned LLVM build the compiler links against. `just install` builds the release compiler and language server, bundles them with the module library, and installs everything under `~/.k1`. Put `~/.k1/bin` on your PATH.

```bash
k1 run examples/refchess
```
