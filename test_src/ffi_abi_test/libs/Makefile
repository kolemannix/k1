OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)

build: 
	clang -c foo.c -o foo.o
	ar rcs libfoo.a foo.o
	clang -fPIC -shared foo.c -o libfoo.$(DYLIB_EXT)

clean:
	rm -f foo.o libfoo.a libfoo.so libfoo.dylib

