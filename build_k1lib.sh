#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1

clang -g -shared -Wall -o k1lib/libk1lib.a k1lib/k1lib.c
