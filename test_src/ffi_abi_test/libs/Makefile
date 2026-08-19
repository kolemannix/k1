OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)
# Keep in sync with MAC_SDK_VERSION in src/k1/compiler.rs
MINVER := $(if $(filter Darwin, $(OS)),-mmacosx-version-min=15.0.0,)

build:
	clang $(MINVER) -c foo.c -o foo.o
	ar rcs libfoo.a foo.o
	clang $(MINVER) -fPIC -shared foo.c -o libfoo.$(DYLIB_EXT)

clean:
	rm -f foo.o libfoo.a libfoo.so libfoo.dylib foo-filc.o libfoo-filc.a

filc:
	$(K1_FILC)/build/bin/clang -O2 -c foo.c -o foo-filc.o
	ar rcs libfoo-filc.a foo-filc.o

