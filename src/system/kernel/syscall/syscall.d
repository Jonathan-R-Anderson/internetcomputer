// ===== kernel/syscall.d =====
module kernel.syscall;

alias SyscallHandler = size_t function(size_t, size_t);

SyscallHandler[6] syscallTable;

void registerSyscalls() {
    import kernel.syscall.exit;
    import kernel.syscall.getpid;
    import kernel.syscall.fork;
    import kernel.syscall.write;
    import kernel.syscall.open;

    syscallTable[0] = &exit;
    syscallTable[1] = &getpid;
    syscallTable[2] = &fork;
    syscallTable[3] = &write;
    syscallTable[4] = &open;
}

size_t syscallDispatcher(size_t num, size_t arg1, size_t arg2) {
    if (auto f = num in syscallTable)
        return (*f)(arg1, arg2);
    return size_t.max; // invalid syscall
}
