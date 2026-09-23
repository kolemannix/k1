#!/bin/bash
cd "$(dirname "$0")"
exec python3 bench.py "$@"
