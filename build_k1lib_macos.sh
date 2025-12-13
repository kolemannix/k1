set -e
set -x
cd k1lib/core/libs

$LLVM_SYS_211_PREFIX/bin/clang -mmacosx-version-min=15.0 --sysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk \
  -flto -c k1rt.c
ar rcs libk1rt.a k1rt.o

$LLVM_SYS_211_PREFIX/bin/clang -mmacosx-version-min=15.0 --sysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk \
  -fPIC -shared k1rt.c -o libk1rt.dylib
