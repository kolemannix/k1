# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

K1 is an experimental systems programming language that combines C-like performance with modern features like type classes (abilities), algebraic data types, pattern matching, and generics. The project is implemented in Rust with an LLVM backend and includes a self-hosted standard library written in K1.

**Important**: K1 is a hobby project for learning and exploration, not production-ready.

## Development Commands

### Main Build & Test Commands
```bash
./check.sh [file]    # Build compiler and run validation (skips codegen/linking if file specified)
./test.sh            # Run comprehensive test suite + build test_suite and compiler binaries
./run.sh <file>      # Compile and run K1 programs
./build_lsp.sh       # Build language server
```

### Individual Build Commands
```bash
# Build compiler (main binary)
cargo build --features=llvm-sys/prefer-dynamic --bin compiler

# Build test runner
cargo build --features=llvm-sys/prefer-dynamic --bin test_suite

# Build language server
cargo build --features=lsp --bin lsp

# Build GUI development environment
cargo build --features=gui --bin gui

# Build REPL
cargo build --bin repl

# Run Rust tests
cargo test
```

### Running Tests
```bash
# Run all K1 language tests
target/debug/test_suite

# Run specific test
target/debug/test_suite <test_name>

# Check for lint violations (no "nocommit" messages)
rg --type-add 'k1:*.k1' -c 'nocommit' -t rust -t c -t k1 .
```

### Environment Variables
```bash
export RUST_BACKTRACE=1     # For debugging Rust panics
export RUST_LOG=info        # For detailed logging
export MallocScribble=1     # For memory debugging (used in test.sh)
```

## Architecture Overview

### Compiler Implementation (Rust)
- **Location**: `/src/k1/`
- **Pipeline**: Lexer → Parser → Type Checker → LLVM Codegen
- **Key modules**:
  - `lex.rs` - Tokenization and lexical analysis
  - `parse.rs` - AST generation with error recovery  
  - `typer.rs` - Type inference, checking, and ability resolution
  - `codegen_llvm.rs` - LLVM IR generation and optimization
  - `vm.rs` - Compile-time execution engine for static evaluation
  - `compiler.rs` - Main compilation orchestration

### Multiple Binary Targets
- **`compiler`** (default) - Main K1 compiler
- **`test_suite`** - Comprehensive test runner
- **`lsp`** - Language Server Protocol implementation
- **`gui`** - Development GUI (raylib-based)
- **`repl`** - Interactive interpreter

### K1 Standard Library
- **Core Library**: `/k1lib/core/` - Memory management, collections, strings, metaprogramming
- **Standard Library**: `/k1lib/std/` - Higher-level utilities like HashMap
- **Self-hosted**: Entire standard library is written in K1

### Test Suite Organization
- **Location**: `/test_src/suite1/`
- **Coverage**: 79 test files covering language features, core library, error handling
- **Types**: Basic features, advanced features (generics, pattern matching), expected compilation failures

## Key Language Features

### Type System
- Strong static typing with limited inference
- Algebraic data types via `either` (sum types) and `struct` (product types)  
- Generics with constraints through abilities (trait system)
- Anonymous structs/enums for lightweight data modeling
- Reference types with distinct pointer/reference semantics

### Modern Programming Features  
- Pattern matching with exhaustiveness checking
- Closures with automatic capture analysis
- String interpolation using `{...}` syntax with full expression support: `{a.b.c()}`
- Backtick strings (`` `string` ``) for easier handling of double-quotes, especially useful for compile-time codegen
- Iterator protocol with `for` expressions supporting `yield`
- Method syntax with namespaced scoping
- Pipeline operator (`|`) for functional composition
- Abilities (traits) for type-class abstraction

### Systems Programming & Metaprogramming
- Direct memory management with pointer operations
- Foreign Function Interface (C interop)
- Fixed-size integer types (u8/i8 through u64/i64)
- Compile-time execution via VM for metaprogramming
- **`#meta` directive** for compile-time code generation and metaprogramming

## Development Workflow

### File Structure
- `/src/k1/` - Rust compiler implementation
- `/k1lib/` - K1 standard library (self-hosted)
- `/test_src/suite1/` - Language test suite
- `/dogfood/` - Real-world K1 programs (Advent of Code, chess engine, etc.)

### Cross-Platform Support
- Primary development on macOS
- Linux x86-64 via Docker cross-compilation
- Docker containerization with `Cross.toml` configuration

### Performance Considerations
- **Development builds**: Use `--features=llvm-sys/prefer-dynamic` for faster iteration
- **Release builds**: Static linking with LTO optimization enabled
- **Memory debugging**: Use `MallocScribble=1` environment variable during testing

## Integration & Tooling

### Language Server Protocol
- Full LSP implementation available via `lsp` binary
- IDE integration support for syntax highlighting, completion, diagnostics

### LLVM Backend
- Uses LLVM 18 via `inkwell` bindings
- Generates optimized machine code with debug information
- Supports compile-time execution through custom VM

### Dependencies Management
- **Core**: `anyhow`, `clap`, `log`, `itertools`, `smallvec`
- **Performance**: `mimalloc`, `ahash`, `fxhash`, `string-interner`
- **LLVM**: `inkwell`, `llvm-sys`, `libffi`
- **Optional features**: LSP (`tower-lsp`, `tokio`), GUI (`raylib`)