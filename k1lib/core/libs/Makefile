OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)

SOURCES := k1rt.c stb_sprintf.h ffc.h

.PHONY: all

all: k1rt.o libk1rt.a libk1rt.$(DYLIB_EXT)

clean:
	rm -f k1rt.o libk1rt.a libk1rt.$(DYLIB_EXT)

k1rt.o: $(SOURCES)
	$(CC) -c k1rt.c -o k1rt.o

libk1rt.a: k1rt.o
	ar rcs libk1rt.a k1rt.o

libk1rt.$(DYLIB_EXT): $(SOURCES)
	$(CC) -fPIC -shared k1rt.c -o libk1rt.$(DYLIB_EXT)

