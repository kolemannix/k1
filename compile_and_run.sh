#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e 
rm a.out || true
export RUST_BACKTRACE=1
cargo run -- resources/test_src/$1
clang++ -g artifacts/$1.o -o artifacts/$1.out
./artifacts/$1.out
echo "Exit Code: $?"
