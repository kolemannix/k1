set -e
set -x
cd libs
clang -c mul.c -o mul.o
ar rcs libmul.a mul.o

clang -fPIC -shared mul.c -o libmul.dylib
