// ===== userspace/syscall_wrappers.d =====
module userspace.syscall_wrappers;

extern(C) size_t syscall(size_t num, size_t arg1, size_t arg2);

size_t write(size_t fd, string msg) {
    return syscall(3, fd, cast(size_t) msg.ptr);
}

size_t fork() {
    return syscall(2, 0, 0);
}

size_t exit(size_t code) {
    return syscall(0, code, 0);
}

size_t getpid() {
    return syscall(1, 0, 0);
}