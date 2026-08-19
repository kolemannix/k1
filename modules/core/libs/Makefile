OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)
# Keep in sync with MAC_SDK_VERSION in src/k1/compiler.rs
MINVER := $(if $(filter Darwin, $(OS)),-mmacosx-version-min=15.0.0,)

HEADERS := stb_sprintf.h ffc.h

BT_DIR := vendor/libbacktrace
BT_SRCS := atomic.c backtrace.c dwarf.c elf.c fileline.c mmap.c mmapio.c posix.c print.c simple.c sort.c state.c
ifneq ($(OS),Darwin)
BT_OBJS := $(BT_SRCS:%.c=bt-%.o)
endif

LLVM_BIN := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))../../../llvm/install-llvm/bin)

.PHONY: all build clean filc wasm nocrt

build: libk1rt.a libk1rt.$(DYLIB_EXT) libk1rt-freestanding.a

clean:
	rm -f *.o libk1rt.a libk1rt.so libk1rt.dylib libk1rt-freestanding.a libk1rt-filc.a libk1rt-wasm.a libk1rt-nocrt.a

k1rt.o: k1rt.c $(HEADERS)
	$(CC) $(MINVER) -fPIC -c k1rt.c -o k1rt.o

k1rt_hosted.o: k1rt_hosted.c
	$(CC) $(MINVER) -fPIC -c k1rt_hosted.c -o k1rt_hosted.o

bt-%.o: $(BT_DIR)/%.c
	$(CC) -O2 -fPIC -I$(BT_DIR) -c $< -o $@

libk1rt.a: k1rt.o k1rt_hosted.o $(BT_OBJS)
	ar rcs libk1rt.a k1rt.o k1rt_hosted.o $(BT_OBJS)

libk1rt.$(DYLIB_EXT): k1rt.c k1rt_hosted.c $(HEADERS) $(BT_OBJS)
	$(CC) $(MINVER) -fPIC -shared k1rt.c k1rt_hosted.c $(BT_OBJS) -o libk1rt.$(DYLIB_EXT)

k1rt-freestanding.o: k1rt.c $(HEADERS)
	$(CC) $(MINVER) -ffreestanding -fno-stack-protector -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -c k1rt.c -o k1rt-freestanding.o

libk1rt-freestanding.a: k1rt-freestanding.o
	ar rcs libk1rt-freestanding.a k1rt-freestanding.o

# wasm64 variant for `k1 --target wasm64` builds; needs the repo LLVM's clang
wasm: libk1rt-wasm.a

k1rt-wasm.o: k1rt.c $(HEADERS)
	$(LLVM_BIN)/clang --target=wasm64 -O2 -ffreestanding -nostdinc \
	  -isystem $(LLVM_BIN)/../lib/clang/21/include -isystem freestanding-include \
	  -fno-stack-protector -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 \
	  -mbulk-memory -msign-ext -mnontrapping-fptoint -c k1rt.c -o k1rt-wasm.o

libk1rt-wasm.a: k1rt-wasm.o
	$(LLVM_BIN)/llvm-ar rcs libk1rt-wasm.a k1rt-wasm.o

# OS-free compute runtime (float fmt, ffc, mem shims) for freestanding
# x86_64-linux consumers of k1 libraries; needs the repo LLVM's clang
nocrt: libk1rt-nocrt.a

NOCRT_CFLAGS := --target=x86_64-unknown-linux-gnu -O2 -ffreestanding -nostdinc \
  -isystem $(LLVM_BIN)/../lib/clang/21/include -isystem freestanding-include \
  -fno-stack-protector -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -DK1_NOCRT \
  -fno-asynchronous-unwind-tables

k1rt-core-nocrt.o: k1rt.c $(HEADERS)
	$(LLVM_BIN)/clang $(NOCRT_CFLAGS) -c k1rt.c -o k1rt-core-nocrt.o

libk1rt-nocrt.a: k1rt-core-nocrt.o
	$(LLVM_BIN)/llvm-ar rcs libk1rt-nocrt.a k1rt-core-nocrt.o

# Fil-C variant for `k1 --filc` builds; requires K1_FILC pointing at a Fil-C
# installation (Linux/x86_64 only)
filc: libk1rt-filc.a

libk1rt-filc.a: k1rt.c k1rt_hosted.c $(HEADERS)
	$(K1_FILC)/build/bin/clang -O2 -c k1rt.c -o k1rt-filc.o
	$(K1_FILC)/build/bin/clang -O2 -c k1rt_hosted.c -o k1rt_hosted-filc.o
	ar rcs libk1rt-filc.a k1rt-filc.o k1rt_hosted-filc.o
