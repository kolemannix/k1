OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)
LIBS := $(if $(filter Linux, $(OS)),-lm -lpthread,)
OUT := ../.k1-out

build:
	clang consumer.c $(OUT)/libklib.a $(LIBS) -o consumer_static.a
	clang consumer.c -L$(OUT) -lklib -Wl,-rpath,$(abspath $(OUT)) -o consumer_dylib.$(DYLIB_EXT)

run: build
	./consumer_static.a > out_static.txt
	./consumer_dylib.$(DYLIB_EXT) > out_dylib.txt
	diff out_static.txt out_dylib.txt
	diff out_static.txt expected.txt

clean:
	rm -f consumer_static.a consumer_dylib.$(DYLIB_EXT) out_static.txt out_dylib.txt
