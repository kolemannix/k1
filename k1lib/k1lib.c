#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// #pragma pack(1)
typedef struct {
  uint64_t len;
  char *data;
} K1String;
// #pragma pack()

_Static_assert (sizeof(K1String) == 16, "K1String size");
_Static_assert (sizeof(size_t) == 8, "64-bit ptr is required");

void _k1_crash(K1String* reason, K1String* filename, uint64_t line) {
    fprintf(stderr, "%.*s at %.*s:%llu\n", (int)reason->len, reason->data, (int)filename->len, filename->data, line);
    abort();
}

void* _k1_malloc(uint64_t size_bytes) {
    void* ptr = malloc(size_bytes);
    printf("k1_malloc(%llu)\n", size_bytes);
    return ptr;
}

void _k1_free(void* ptr) {
    printf("k1_free(%zu)\n", (size_t)ptr);
    free(ptr);
}


// Passing struct; not guaranteed ABI
// One day pass by reference OR pass each field
K1String _k1_readFileToString(K1String filename) {
    char* as_cstring = malloc(filename.len + 1);
    // snprintf(as_cstring, filename.len, "%s", filename.data);
    memcpy(as_cstring, filename.data, filename.len);
    as_cstring[filename.len] = '\0';

    FILE* file = fopen(as_cstring, "r");
    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    char* buf = malloc(fsize);

    fseek(file, 0, SEEK_SET);
    fread(buf, fsize, 1, file);
    K1String string = {
        .len = fsize,
        .data = buf
    };
    fclose(file);
    free(as_cstring);
    return string;
}

// int main(int argc, char** argv) {
//     puts("Hello from the runtime");
//     void *heap = _k1_malloc(1024 * 1024);
//     int res = _k1_main()
//
//     return int
// }
