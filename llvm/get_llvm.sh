#!/usr/bin/env bash
set -x
set -euo pipefail

# === Configuration ===
LLVM_REPO="https://github.com/llvm/llvm-project.git"
LLVM_TAG="llvmorg-21.1.7"      # pick the LLVM release tag you want
BUILD_DIR="$(pwd)/build-llvm"
INSTALL_DIR="$(pwd)/install-llvm"
SRC_DIR="$(pwd)/llvm-project"

# Choose which subprojects to build
LLVM_ENABLE_PROJECTS="clang;lld;compiler-rt"

# Targets: only build x86_64 and AArch64 backends
LLVM_TARGETS="X86;AArch64"

# CMake generator
GENERATOR="Ninja"

# CMake build type (Release, RelWithDebInfo, etc.)
BUILD_TYPE="Release"

# === Clone LLVM ===
if [ ! -d "${SRC_DIR}" ]; then
  git clone --depth 1 --branch "${LLVM_TAG}" "${LLVM_REPO}" "${SRC_DIR}"
fi

mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# === Configure ===
cmake -G "${GENERATOR}" "${SRC_DIR}"/llvm \
  -DCMAKE_BUILD_TYPE="${BUILD_TYPE}" \
  -DCMAKE_INSTALL_PREFIX="${INSTALL_DIR}" \
  -DLLVM_BUILD_LLVM_DYLIB=OFF \
  -DLLVM_LINK_LLVM_DYLIB=OFF \
  -DLLVM_ENABLE_RTTI=OFF \
  -DLLVM_ENABLE_EH=OFF \
  -DLLVM_ENABLE_ZLIB=OFF \
  -DLLVM_ENABLE_LIBXML2=OFF \
  -DLLVM_ENABLE_TERMINFO=OFF \
  -DLLVM_ENABLE_PROJECTS="${LLVM_ENABLE_PROJECTS}" \
  -DLLVM_TARGETS_TO_BUILD="${LLVM_TARGETS}" \
  -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=OFF

# === Build and Install ===
cmake --build . -- -j$(getconf _NPROCESSORS_ONLN)
cmake --install .

echo "LLVM built and installed under: ${INSTALL_DIR}"
