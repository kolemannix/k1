#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e 
rm a.out || true
export RUST_BACKTRACE=1
NAME=$(basename $1)
cargo run -- $1
clang++ -g artifacts/$NAME.o -o artifacts/$NAME.out
./artifacts/$NAME.out
echo "Exit Code: $?"
