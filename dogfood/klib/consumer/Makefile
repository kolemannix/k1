OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)
LIBS := $(if $(filter Linux, $(OS)),-lm -lpthread,)
OUT := ../.k1-out

build:
	clang consumer.c $(OUT)/libklib.a $(LIBS) -o consumer_static
	clang consumer.c -L$(OUT) -lklib -Wl,-rpath,$(abspath $(OUT)) -o consumer_dylib

run: build
	./consumer_static > out_static.txt
	./consumer_dylib > out_dylib.txt
	diff out_static.txt out_dylib.txt
	diff out_static.txt expected.txt

clean:
	rm -f consumer_static consumer_dylib out_static.txt out_dylib.txt
