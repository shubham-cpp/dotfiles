#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

int main(int argc, char **argv) {
    if (argc != 2 || (strcmp(argv[1], "launcher") != 0 && strcmp(argv[1], "clipboard") != 0)) {
        fprintf(stderr, "usage: qs-toggle launcher|clipboard\n");
        return 2;
    }

    const char *runtime = getenv("XDG_RUNTIME_DIR");
    struct sockaddr_un address = { .sun_family = AF_UNIX };
    int length = runtime ? snprintf(address.sun_path, sizeof(address.sun_path), "%s/qs-shell-toggle.sock", runtime) : -1;
    if (length < 0 || (size_t)length >= sizeof(address.sun_path)) {
        fprintf(stderr, "qs-toggle: invalid XDG_RUNTIME_DIR\n");
        return 1;
    }

    int fd = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
    if (fd < 0) {
        perror("qs-toggle: socket");
        return 1;
    }
    if (connect(fd, (struct sockaddr *)&address, offsetof(struct sockaddr_un, sun_path) + (size_t)length + 1) < 0) {
        close(fd);
        // Preserve the existing binding while Quickshell is restarting or the fast path is unavailable.
        char *args[] = { "qs", "ipc", "call", argv[1], "toggle", NULL };
        execvp(args[0], args);
        perror("qs-toggle: qs");
        return 1;
    }

    const char command[] = { argv[1][0] == 'l' ? 'L' : 'C', '\n' };
    size_t written = 0;
    while (written < sizeof(command)) {
        ssize_t n = send(fd, command + written, sizeof(command) - written, MSG_NOSIGNAL);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) {
            perror("qs-toggle: send");
            close(fd);
            return 1;
        }
        written += (size_t)n;
    }
    close(fd);
    return 0;
}
