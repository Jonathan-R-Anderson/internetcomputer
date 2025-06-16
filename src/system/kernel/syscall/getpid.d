module system.kernel.syscall.getpid;

import syscall;                  // existing syscall framework in repo
import system.kernel.process;

@Syscall(4)  // Pick an unused number
size_t getpid() {
    return currentProcess().pid;
}
