module system.kernel.process;

enum TaskStatus { Runnable, Exited }

struct Process {
    size_t pid;
    TaskStatus status;
    size_t exitCode;
}

// Temporary global for proof of concept
__gshared Process currentProc = Process(pid: 1, status: TaskStatus.Runnable);

Process* currentProcess() {
    return &currentProc;
}
