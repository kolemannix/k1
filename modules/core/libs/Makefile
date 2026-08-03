OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)

SOURCES := k1rt.c stb_sprintf.h ffc.h

.PHONY: all

build: k1rt.o libk1rt.a libk1rt.$(DYLIB_EXT)

clean:
	rm -f k1rt.o libk1rt.a libk1rt.$(DYLIB_EXT) k1rt-filc.o libk1rt-filc.a

k1rt.o: $(SOURCES)
	$(CC) -c k1rt.c -o k1rt.o

libk1rt.a: k1rt.o
	ar rcs libk1rt.a k1rt.o

libk1rt.$(DYLIB_EXT): $(SOURCES)
	$(CC) -fPIC -shared k1rt.c -o libk1rt.$(DYLIB_EXT)

# Fil-C variant for `k1 --filc` builds; requires K1_FILC pointing at a Fil-C
# installation (Linux/x86_64 only)
filc: libk1rt-filc.a

libk1rt-filc.a: $(SOURCES)
	$(K1_FILC)/build/bin/clang -O2 -c k1rt.c -o k1rt-filc.o
	ar rcs libk1rt-filc.a k1rt-filc.o

