// kqueue_hello.c
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    int kq = kqueue();
    if (kq == -1) {
        perror("kqueue");
        return 1;
    }

    struct kevent change;
    EV_SET(&change, STDIN_FILENO, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, NULL);

    printf("Type something and press Enter:\n");

    struct kevent event;
    int nev = kevent(kq, &change, 1, &event, 1, NULL);
    if (nev == -1) {
        perror("kevent");
        close(kq);
        return 1;
    }

    if (event.filter == EVFILT_READ) {
        char buf[256];
        ssize_t n = read(STDIN_FILENO, buf, sizeof(buf) - 1);
        if (n > 0) {
            buf[n] = '\0';
            printf("Hello, kqueue saw input: %s", buf);
        }
    }

    close(kq);
    return 0;
}
