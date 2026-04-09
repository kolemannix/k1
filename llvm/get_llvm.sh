#!/usr/bin/env bash
set -x
set -euo pipefail

# === Configuration ===
LLVM_REPO="https://github.com/llvm/llvm-project.git"
LLVM_TAG="llvmorg-21.1.7"
BUILD_DIR="$(pwd)/build-llvm"
INSTALL_DIR="$(pwd)/install-llvm"
SRC_DIR="$(pwd)/llvm-project"

LLVM_ENABLE_PROJECTS="clang;lld"

LLVM_TARGETS="X86;AArch64"

# CMake generator
GENERATOR="Unix Makefiles"

if [ ! -d "${SRC_DIR}" ]; then
  git clone --depth 1 --branch "${LLVM_TAG}" "${LLVM_REPO}" "${SRC_DIR}"
fi

mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# === Configure ===
# Note on -DLLVM_STATIC_LINK_CXX_STDLIB=OFF: we wish we could statically link libc++
# But Rust's llvm-sys is hardcoded to demand libc++ anyway (https://docs.rs/crate/llvm-sys/221.0.0/source/build.rs#383)
# So I'll request libc++ (clang) over libstdc++ (gnu), but will not statically link it yet
# Until we move to pure rustc perhaps, or selfhost or something
cmake -G "${GENERATOR}" "${SRC_DIR}"/llvm \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="${INSTALL_DIR}" \
  -DLLVM_BUILD_LLVM_DYLIB=OFF \
  -DLLVM_LINK_LLVM_DYLIB=OFF \
  -DLLVM_ENABLE_RTTI=OFF \
  -DLLVM_ENABLE_EH=OFF \
  -DLLVM_ENABLE_ZLIB=OFF \
  -DLLVM_ENABLE_LIBXML2=OFF \
  -DLLVM_ENABLE_FFI=OFF \
  -DLLVM_ENABLE_PROJECTS="${LLVM_ENABLE_PROJECTS}" \
  -DLLVM_TARGETS_TO_BUILD="${LLVM_TARGETS}" \
  -DLLVM_INCLUDE_TESTS=OFF \
  -DLLVM_STATIC_LINK_CXX_STDLIB=OFF \
  -DLLVM_ENABLE_LIBCXX=ON \
  -DLLVM_ENABLE_RUNTIMES=compiler-rt

# === Build and Install ===
# cmake --build . -- -j$(getconf _NPROCESSORS_ONLN)
cmake --build . -- -j2
cmake --install .

echo "LLVM built and installed under: ${INSTALL_DIR}"
