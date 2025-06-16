// src/kernel/syscall/getpid.d
module syscall.action.getpid;

import syscall;
import kernel.process;

@Syscall(5) // syscall number
size_t getpid() {
    return currentProcess().pid;
}
