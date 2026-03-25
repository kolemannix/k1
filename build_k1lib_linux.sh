set -e
set -x
cd k1lib/core/libs
cc -c k1rt.c -o k1rt.o
ar rcs libk1rt.a k1rt.o
cc -fPIC -shared k1rt.c -o libk1rt.dylib
