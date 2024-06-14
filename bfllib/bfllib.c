#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #pragma pack(1)
typedef struct {
  uint64_t len;
  char *data;
} BflString;
// #pragma pack()

_Static_assert (sizeof(BflString) == 16, "BflString size");

BflString _bfl_charToString(char c) {
  char *data = malloc(1);
  data[0] = c;
  BflString string = {
      .len = 1,
      .data = data,
  };
  return string;
}

// Passing struct; not guaranteed ABI
// One day pass by reference OR pass each field
BflString _bfl_readFileToString(BflString filename) {
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
    BflString string = {
        .len = fsize,
        .data = buf
    };
    fclose(file);
    free(as_cstring);
    return string;
}

// To be removed once we have basic casts

int64_t _bfl_charToInt(char c) {
    return (int64_t)c;
}

char _bfl_intToChar(int64_t i) {
    return (char)i;
}
