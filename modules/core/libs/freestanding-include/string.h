/* Stub for the sysroot-less wasm64 k1rt build; definitions live in k1rt.c */
#pragma once
#include <stddef.h>
void *memcpy(void *dst, const void *src, size_t n);
void *memmove(void *dst, const void *src, size_t n);
void *memset(void *dst, int value, size_t n);
int memcmp(const void *p1, const void *p2, size_t n);
