#include <stdio.h>
#include <stdlib.h>

typedef struct {
  uint64_t len;
  char *data;
} BflString;

BflString _bfl_charToString(char c) {
  char *data = malloc(1);
  data[0] = c;
  BflString string = {
      .len = 1,
      .data = data,
  };
  return string;
}

BflString _bfl_readFileToString(BflString filename) {
    FILE* file = fopen(filename.data, "r");
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
    return string;
}

// To be removed once we have basic casts

int64_t _bfl_charToInt(char c) {
    return (int64_t)c;
}

char _bfl_intToChar(int64_t i) {
    return (char)i;
}
