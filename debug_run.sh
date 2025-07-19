#!/bin/bash
# Debug build and run non-interactively with sanitizers
# Show output and commands
set -x
# Exit on errors
set -e
FILENAME=${@: -1}
filename=$(basename $FILENAME .k1)
export RUST_BACKTRACE=1

# Build with debug info and sanitizers
RUST_LOG=debug cargo run --features=llvm-sys/prefer-dynamic -- --debug --no-llvm-opt build $@

echo "Running with sanitizers..."
# Run non-interactively with lldb to catch any crashes
lldb --batch -o run .k1-out/$filename

echo "Direct execution with sanitizers..."
# Also run directly to see sanitizer output
./.k1-out/$filename