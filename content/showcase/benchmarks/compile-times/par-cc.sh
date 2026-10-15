#!/bin/sh
REPO=$(cd "$(dirname "$0")/../../../.." && pwd)
case "$1" in
  -fsyntax-only) emit="" ;;
  *) emit="-o /dev/null" ;;
esac
printf '%s\n' compress_fragment.c compress_fragment_two_pass.c encode.c fast_log.c \
  | sed "s|^|$REPO/modules/brotli/vendor/|" \
  | xargs -P 4 -n 1 clang "$@" -I"$REPO/examples/brotli_parity/deps/cbrotli/vendor/enc" -I"$REPO/examples/brotli_parity/deps/cbrotli/vendor/include" $emit
