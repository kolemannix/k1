#!/bin/sh
set -eu

main() {
  os=$(uname -s)
  arch=$(uname -m)
  case "$os-$arch" in
    Darwin-arm64) bundle=k1-macos ;;
    Linux-x86_64) bundle=k1-linux-x86 ;;
    *)
      echo "k1: no prebuilt bundle for $os $arch. Build from source: https://k1lang.org/install/" >&2
      exit 1 ;;
  esac

  if [ -n "${K1_VERSION:-}" ]; then
    url="https://github.com/kolemannix/k1/releases/download/$K1_VERSION/$bundle.tar.gz"
  else
    url="https://github.com/kolemannix/k1/releases/latest/download/$bundle.tar.gz"
  fi

  tmp=$(mktemp -d)
  trap 'rm -rf "$tmp"' EXIT
  echo "Downloading $url"
  curl -fL --progress-bar -o "$tmp/$bundle.tar.gz" "$url"
  tar -xzf "$tmp/$bundle.tar.gz" -C "$tmp"
  cd "$tmp/$bundle"
  sh install.sh
}

main
