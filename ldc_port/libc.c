#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

// Simple syscall wrapper for the Internet Computer OS
long ic_syscall(long number, long a1, long a2, long a3, long a4, long a5) {
    long ret;
    asm volatile ("syscall"
                  : "=a"(ret)
                  : "a"(number), "D"(a1), "S"(a2), "d"(a3), "r10"(a4), "r8"(a5)
                  : "rcx", "r11", "memory");
    return ret;
}

int open(const char *path, int flags, int mode) {
    return ic_syscall(2, (long)path, flags, mode, 0, 0);
}

long read(int fd, void *buf, long count) {
    return ic_syscall(4, fd, (long)buf, count, 0, 0);
}

long write(int fd, const void *buf, long count) {
    return ic_syscall(5, fd, (long)buf, count, 0, 0);
}

int close(int fd) {
    return ic_syscall(7, fd, 0, 0, 0, 0);
}

void _exit(int code) {
    ic_syscall(21, code, 0, 0, 0, 0);
    while (1) {}
}

void *sbrk(ptrdiff_t increment) {
    static char *heap_end;
    if (!heap_end) {
        // assume linker defined end symbol
        extern char _end;
        heap_end = &_end;
    }
    char *prev = heap_end;
    heap_end += increment;
    return prev;
}

/* additional wrappers required for running a full D compiler */

typedef struct {
    int   kind;
    size_t size;
} ic_stat;

long lseek(int fd, long offset, int whence) {
    return ic_syscall(6, fd, offset, whence, 0, 0);
}

int unlink(const char *path) {
    return ic_syscall(14, (long)path, 0, 0, 0, 0);
}

int stat(const char *path, ic_stat *st) {
    return ic_syscall(10, (long)path, (long)st, 0, 0, 0);
}

int fstat(int fd, ic_stat *st) {
    return ic_syscall(11, fd, (long)st, 0, 0, 0);
}

int dup(int fd) {
    return ic_syscall(8, fd, -1, 0, 0, 0);
}

int dup2(int oldfd, int newfd) {
    return ic_syscall(8, oldfd, newfd, 0, 0, 0);
}

int pipe(int fds[2]) {
    return ic_syscall(25, (long)fds, 0, 0, 0, 0);
}

int fork(void) {
    return ic_syscall(19, 1, 0, 0, 0, 0);
}

int execve(const char *path, char *const argv[], char *const envp[]) {
    (void)envp; /* environment currently ignored */
    return ic_syscall(20, (long)path, (long)argv, 0, 0, 0);
}

int waitpid(int pid, int *status, int options) {
    (void)pid; (void)status; (void)options;
    return ic_syscall(24, 0, 0, 0, 0, 0);
}

long brk(void *addr) {
    return ic_syscall(29, (long)addr, 0, 0, 0, 0);
}

int getpid(void) {
    /* kernel lacks a getpid syscall; return a constant */
    return 1;
}

char *getcwd(char *buf, size_t size) {
    if (size == 0)
        return 0;
    buf[0] = '/';
    if (size > 1)
        buf[1] = '\0';
    return buf;
}
