set -e
set -x
cd libs
clang -c foo.c -o foo.o
ar rcs libfoo.a foo.o

case "$(uname -s)" in
  Darwin) SHLIB_EXT="dylib" ;;
  *)      SHLIB_EXT="so"    ;;
esac

clang -fPIC -shared foo.c -o libfoo.$SHLIB_EXT
