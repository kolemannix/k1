#include <pthread.h>
#include <stdio.h>

void *go(void *arg) {
  printf("Hello from thread!\n");
  return NULL;
}

int main(void) {
  printf("size of pthread_t: %lu\n", sizeof(pthread_t));

  pthread_t thread;
  pthread_create(&thread, NULL, go, NULL);
  pthread_join(thread, NULL);

  return 0;
}
