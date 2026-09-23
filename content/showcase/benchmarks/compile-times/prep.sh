#!/bin/sh
set -e
nonce() {
  n=$(date +%s)$$
  sed -i '' -e "s/nonce [0-9]*/nonce $n/" -e "s/NONCE: u64 = [0-9]*/NONCE: u64 = $n/" "$1"
}
case "$1" in
  nonce) nonce "$2" ;;
  go) nonce "$2"; rm -f "$3" ;;
  rm) shift; rm -rf "$@" ;;
  *) echo "prep.sh: unknown step $1" >&2; exit 1 ;;
esac
