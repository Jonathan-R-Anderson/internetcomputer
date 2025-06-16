// ===== kernel/scheduler.d =====
module kernel.scheduler;

import kernel.process;
import userspace.syscall_wrappers : write, yield, getpid;
import std.conv : to;

extern(C) void context_switch(CPUContext* oldCtx, CPUContext* newCtx);

@safe:

struct CPUContext {
    ulong r15, r14, r13, r12, r11, r10, r9, r8;
    ulong rsi, rdi, rbp, rdx, rcx, rbx, rax;
    ulong rip, rsp, rflags;
}

struct Task {
    size_t pid;
    CPUContext context;
    ubyte[] stack;
    bool runnable = true;
}

__gshared Task[] tasks;
__gshared size_t current = 0;

void initScheduler() {
    Task t;
    t.pid = 1;
    t.stack.length = 4096 * 4; // 16 KB stack
    t.context.rsp = cast(ulong)t.stack.ptr + t.stack.length;
    t.context.rip = cast(ulong)&dummyStart;
    tasks ~= t;
}

extern(C) void dummyStart() {
    size_t id = getpid();
    for (int i = 0; i < 5; i++) {
        write(1, "Task " ~ id.to!string ~ " running...\n");
        yield();
    }
}

void yield() {
    size_t prev = current;
    do {
        current = (current + 1) % tasks.length;
    } while (!tasks[current].runnable && current != prev);

    if (prev != current)
        context_switch(&tasks[prev].context, &tasks[current].context);
}

size_t fork(void function() entry) {
    auto parent = &tasks[current];

    Task child;
    child.pid = tasks.length + 1;
    child.stack.length = parent.stack.length;
    child.stack[] = parent.stack[]; // basic copy

    size_t offset = cast(size_t)child.stack.ptr - cast(size_t)parent.stack.ptr;

    child.context = parent.context;
    child.context.rsp += offset;
    child.context.rip = cast(ulong)entry;

    tasks ~= child;
    return child.pid;
}

void terminateCurrent() {
    tasks[current].runnable = false;
    yield(); // Immediately yield to next runnable task
}

Task* currentTask() {
    return &tasks[current];
}