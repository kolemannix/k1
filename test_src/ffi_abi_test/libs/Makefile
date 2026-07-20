OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)

build: 
	clang -c foo.c -o foo.o
	ar rcs libfoo.a foo.o
	clang -fPIC -shared foo.c -o libfoo.$(DYLIB_EXT)

clean:
	rm -f foo.o libfoo.a libfoo.so libfoo.dylib foo-filc.o libfoo-filc.a

filc:
	$(K1_FILC)/build/bin/clang -O2 -c foo.c -o foo-filc.o
	ar rcs libfoo-filc.a foo-filc.o

