module syscall.action.exit;

import syscall;
import task.scheduler;
import task.task;       // Assuming this defines Task structure
import memory.manager;  // If needed to deallocate resources

@Syscall(0)
@SyscallArgument!(size_t)
size_t exit(size_t returnValue) {
    auto current = Scheduler.currentTask();

    // Mark the task as exited (assuming status/exitCode exist)
    current.exitCode = returnValue;
    current.status = TaskStatus.Exited;

    // Remove from scheduler queue or mark as inactive
    Scheduler.terminateCurrent();

    // Prevent returning
    while (true)
        Scheduler.yield();

    assert(0, "Scheduler.terminateCurrent() failed to halt the process.");
    return 0;
}
