OS := $(shell uname -s)
DYLIB_EXT := $(if $(filter Darwin, $(OS)),dylib,so)

HEADERS := stb_sprintf.h ffc.h

BT_DIR := vendor/libbacktrace
BT_SRCS := atomic.c backtrace.c dwarf.c elf.c fileline.c mmap.c mmapio.c posix.c print.c simple.c sort.c state.c
ifneq ($(OS),Darwin)
BT_OBJS := $(BT_SRCS:%.c=bt-%.o)
endif

.PHONY: all build clean filc

build: libk1rt.a libk1rt.$(DYLIB_EXT) libk1rt-freestanding.a

clean:
	rm -f *.o libk1rt.a libk1rt.so libk1rt.dylib libk1rt-freestanding.a libk1rt-filc.a

k1rt.o: k1rt.c $(HEADERS)
	$(CC) -fPIC -c k1rt.c -o k1rt.o

k1rt_hosted.o: k1rt_hosted.c
	$(CC) -fPIC -c k1rt_hosted.c -o k1rt_hosted.o

bt-%.o: $(BT_DIR)/%.c
	$(CC) -O2 -fPIC -I$(BT_DIR) -c $< -o $@

libk1rt.a: k1rt.o k1rt_hosted.o $(BT_OBJS)
	ar rcs libk1rt.a k1rt.o k1rt_hosted.o $(BT_OBJS)

libk1rt.$(DYLIB_EXT): k1rt.c k1rt_hosted.c $(HEADERS) $(BT_OBJS)
	$(CC) -fPIC -shared k1rt.c k1rt_hosted.c $(BT_OBJS) -o libk1rt.$(DYLIB_EXT)

k1rt-freestanding.o: k1rt.c $(HEADERS)
	$(CC) -ffreestanding -fno-stack-protector -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -c k1rt.c -o k1rt-freestanding.o

libk1rt-freestanding.a: k1rt-freestanding.o
	ar rcs libk1rt-freestanding.a k1rt-freestanding.o

# Fil-C variant for `k1 --filc` builds; requires K1_FILC pointing at a Fil-C
# installation (Linux/x86_64 only)
filc: libk1rt-filc.a

libk1rt-filc.a: k1rt.c k1rt_hosted.c $(HEADERS)
	$(K1_FILC)/build/bin/clang -O2 -c k1rt.c -o k1rt-filc.o
	$(K1_FILC)/build/bin/clang -O2 -c k1rt_hosted.c -o k1rt_hosted-filc.o
	ar rcs libk1rt-filc.a k1rt-filc.o k1rt_hosted-filc.o
