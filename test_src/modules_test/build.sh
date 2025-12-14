set -e
set -x
cd libs
clang -c foo.c -o foo.o
ar rcs libfoo.a foo.o

clang -fPIC -shared foo.c -o libfoo.dylib
