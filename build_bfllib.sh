#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1

clang -g -shared -Wall -o bfllib/libbfllib.a bfllib/bfllib.c
