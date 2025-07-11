module kernel.syscall;

pragma(LDC_no_moduleinfo);

import kernel.terminal : terminal_putchar, terminal_writestring;
import kernel.fs : fs_create_user, fs_open_file, fs_close_file,
                    fs_create_file_desc, fs_pread_file, fs_pwrite_file,
                    fs_seek_file, fs_dup_fd, fs_fd2path, fs_stat,
                    fs_fstat, fs_wstat, fs_fwstat, fs_remove,
                    fs_chdir, fs_mount, fs_bind, fs_unmount,
                    fs_create_pipe, g_fdtable, Pipe,
                    Stat;
import kernel.hypervisor : vmm_create_vm, vmm_run_vm, vmm_destroy_vm;
import kernel.container_service : start_container, ContainerConfig;
import kernel.types; // for ulong
import kernel.process_manager : process_create_with_parent, process_exit,
                               process_wait, get_current_pid, g_processes;
import kernel.process_manager : scheduler_run; // to run new procs
import kernel.interrupts : timer_ticks;
import kernel.elf_loader : load_elf;
import kernel.process_manager : EntryFunc;
import kernel.sync : rendezvous, sem_acquire, sem_release, semaphore_init;
import kernel.lib.stdc.stdlib : free;
import kernel.memory.virtmem : brk, seg_attach, seg_detach, seg_brk,
                              seg_free, seg_flush;
import kernel.keyboard : keyboard_getchar;
import kernel.logger : log_message, log_hex;

public:

enum SyscallID : ulong {
    WriteString = 0,
    CreateUser  = 1,
    Open        = 2,
    Create      = 3,
    PRead       = 4,
    PWrite      = 5,
    Seek        = 6,
    Close       = 7,
    Dup         = 8,
    FD2Path     = 9,
    Stat        = 10,
    FStat       = 11,
    WStat       = 12,
    FWStat      = 13,
    Remove      = 14,
    ChDir       = 15,
    Mount       = 16,
    Bind        = 17,
    Unmount     = 18,
    RFork       = 19,
    Exec        = 20,
    Exit        = 21,
    ErrStr      = 22,
    Sleep       = 23,
    Await       = 24,
    Pipe        = 25,
    Rendezvous  = 26,
    SemAcquire  = 27,
    SemRelease  = 28,
    Brk         = 29,
    SegAttach   = 30,
    SegDetach   = 31,
    SegBrk      = 32,
    SegFree     = 33,
    SegFlush    = 34,
    FVersion    = 35,
    FAuth       = 36,
    Alarm       = 37,
    Notify      = 38,
    Noted       = 39,
    VMCreate    = 40,
    VMRun       = 41,
    VMDestroy   = 42,
    ContainerStart = 43,
    ReadChar    = 44,
}

alias SyscallHandler = extern(C) long function(ulong, ulong, ulong, ulong, ulong, ulong);

__gshared SyscallHandler[64] g_syscalls;
__gshared char[256] g_errstr;

struct P9Conn {
    size_t msize;
    char[32] version_;
    size_t fid;
    bool authenticated;
}

alias NoteHandlerFn = extern(C) void function(const(char)*);

__gshared P9Conn g_p9conn;

extern(C) long sys_write_string(ulong strPtr, ulong len, ulong, ulong, ulong, ulong)
{
    auto s = cast(const(char)*)strPtr;
    for(size_t i = 0; i < len; i++) {
        terminal_putchar(s[i]);
    }
    return cast(long)len;
}

extern(C) long sys_create_user(ulong namePtr, ulong, ulong, ulong, ulong, ulong)
{
    auto name = cast(const(char)*)namePtr;
    fs_create_user(name);
    return 0;
}

extern(C) long sys_open(ulong pathPtr, ulong mode, ulong, ulong, ulong, ulong)
{
    auto path = cast(const(char)*)pathPtr;
    int fd = fs_open_file(path, cast(int)mode);
    return fd;
}

extern(C) long sys_create(ulong pathPtr, ulong mode, ulong perm, ulong, ulong, ulong)
{
    auto path = cast(const(char)*)pathPtr;
    int fd = fs_create_file_desc(path, cast(int)mode, cast(int)perm);
    return fd;
}

extern(C) long sys_pread(ulong fd, ulong buf, ulong count, ulong offset, ulong, ulong)
{
    return fs_pread_file(cast(int)fd, cast(void*)buf, cast(size_t)count, cast(size_t)offset);
}

extern(C) long sys_pwrite(ulong fd, ulong buf, ulong count, ulong offset, ulong, ulong)
{
    return fs_pwrite_file(cast(int)fd, cast(const(void)*)buf, cast(size_t)count, cast(size_t)offset);
}

extern(C) long sys_seek(ulong fd, ulong offset, ulong whence, ulong, ulong, ulong)
{
    return fs_seek_file(cast(int)fd, cast(long)offset, cast(int)whence);
}

extern(C) long sys_close(ulong fd, ulong, ulong, ulong, ulong, ulong)
{
    return fs_close_file(cast(int)fd);
}

extern(C) long sys_dup(ulong oldfd, ulong newfd, ulong, ulong, ulong, ulong)
{
    return fs_dup_fd(cast(int)oldfd, cast(int)newfd);
}

extern(C) long sys_fd2path(ulong fd, ulong, ulong, ulong, ulong, ulong)
{
    auto p = fs_fd2path(cast(int)fd);
    return cast(long)p;
}

extern(C) long sys_stat(ulong pathPtr, ulong statPtr, ulong, ulong, ulong, ulong)
{
    auto path = cast(const(char)*)pathPtr;
    auto st = cast(Stat*)statPtr;
    return fs_stat(path, st);
}

extern(C) long sys_fstat(ulong fd, ulong statPtr, ulong, ulong, ulong, ulong)
{
    auto st = cast(Stat*)statPtr;
    return fs_fstat(cast(int)fd, st);
}

extern(C) long sys_wstat(ulong pathPtr, ulong statPtr, ulong, ulong, ulong, ulong)
{
    auto path = cast(const(char)*)pathPtr;
    auto st = cast(const Stat*)statPtr;
    return fs_wstat(path, st);
}

extern(C) long sys_fwstat(ulong fd, ulong statPtr, ulong, ulong, ulong, ulong)
{
    auto st = cast(const Stat*)statPtr;
    return fs_fwstat(cast(int)fd, st);
}

extern(C) long sys_remove(ulong pathPtr, ulong, ulong, ulong, ulong, ulong)
{
    auto path = cast(const(char)*)pathPtr;
    return fs_remove(path);
}

extern(C) long sys_chdir(ulong pathPtr, ulong, ulong, ulong, ulong, ulong)
{
    auto path = cast(const(char)*)pathPtr;
    return fs_chdir(path);
}

extern(C) long sys_mount(ulong specPtr, ulong targetPtr, ulong flags, ulong fsPtr, ulong anamePtr, ulong)
{
    auto spec = cast(const(char)*)specPtr;
    auto target = cast(const(char)*)targetPtr;
    auto fsStr = cast(const(char)*)fsPtr;
    auto aname = cast(const(char)*)anamePtr;
    return fs_mount(spec, target, cast(int)flags, fsStr, aname);
}

extern(C) long sys_bind(ulong oldPtr, ulong newPtr, ulong flags, ulong, ulong, ulong)
{
    auto oldp = cast(const(char)*)oldPtr;
    auto newp = cast(const(char)*)newPtr;
    return fs_bind(oldp, newp, cast(int)flags);
}

extern(C) long sys_unmount(ulong targetPtr, ulong, ulong, ulong, ulong, ulong)
{
    auto target = cast(const(char)*)targetPtr;
    return fs_unmount(target);
}

enum RForkFlags : ulong { RFPROC = 1 }

extern(C) long sys_rfork(ulong flags, ulong, ulong, ulong, ulong, ulong)
{
    size_t pid = get_current_pid();
    size_t child = process_create_with_parent(g_processes[pid].entry, pid, g_processes[pid].name);
    if(child == size_t.max)
    {
        auto msg = "rfork failed";
        foreach(i; 0 .. msg.length) g_errstr[i] = msg[i];
        g_errstr[msg.length] = 0;
        return -1;
    }
    scheduler_run();
    return cast(long)child;
}

extern(C) long sys_exec(ulong pathPtr, ulong argvPtr, ulong, ulong, ulong, ulong)
{
    log_message("sys_exec called with pathPtr=");
    log_hex(pathPtr);
    log_message("\n");
    
    // Validate the path pointer
    if(pathPtr == 0 || pathPtr < 0x1000) {
        log_message("sys_exec: invalid path pointer\n");
        auto msg = "exec failed: invalid path";
        foreach(j; 0 .. msg.length) g_errstr[j] = msg[j];
        g_errstr[msg.length] = 0;
        return -1;
    }
    
    auto path = cast(const(char)*)pathPtr;
    void* entry = null;
    if(load_elf(path, &entry) != 0 || entry is null)
    {
        auto msg = "exec failed";
        foreach(j; 0 .. msg.length) g_errstr[j] = msg[j];
        g_errstr[msg.length] = 0;
        return -1;
    }
    size_t parent = get_current_pid();
    auto child = process_create_with_parent(cast(EntryFunc)entry, parent, path);
    if(child == size_t.max)
    {
        auto msg = "exec failed";
        foreach(j; 0 .. msg.length) g_errstr[j] = msg[j];
        g_errstr[msg.length] = 0;
        return -1;
    }
    scheduler_run();
    return cast(long)child;
}

extern(C) long sys_exit(ulong status, ulong, ulong, ulong, ulong, ulong)
{
    auto pid = get_current_pid();
    process_exit(pid, cast(int)status);
    scheduler_run();
    asm { "hlt"; }
    return 0;
}

extern(C) long sys_errstr(ulong bufPtr, ulong n, ulong, ulong, ulong, ulong)
{
    auto buf = cast(char*)bufPtr;
    size_t len = 0;
    while(len < g_errstr.length && g_errstr[len]) ++len;
    size_t copyLen = (n < len) ? cast(size_t)n : len;
    for(size_t i = 0; i < copyLen; i++)
        buf[i] = g_errstr[i];
    return cast(long)copyLen;
}

extern(C) long sys_sleep(ulong nsec, ulong, ulong, ulong, ulong, ulong)
{
    auto start = timer_ticks;
    while(timer_ticks - start < nsec) {
        asm { "hlt"; }
    }
    return 0;
}

extern(C) long sys_await(ulong, ulong, ulong, ulong, ulong, ulong)
{
    auto pid = get_current_pid();
    auto child = process_wait(pid);
    if(child == size_t.max)
        return -1;
    return cast(long)child;
}

extern(C) long sys_pipe(ulong fdsPtr, ulong, ulong, ulong, ulong, ulong)
{
    auto fds = cast(int*)fdsPtr;
    auto n = fs_create_pipe();
    if(n is null) return -1;
    int rd = -1, wr = -1;
    foreach(i, ref f; g_fdtable)
    {
        if(f.node is null)
        {
            if(rd == -1)
            {
                rd = cast(int)i;
                f.node = n;
                f.pos = 0;
                f.pipeRead = true;
            }
            else
            {
                wr = cast(int)i;
                f.node = n;
                f.pos = 0;
                f.pipeWrite = true;
                break;
            }
        }
    }
    if(rd == -1 || wr == -1)
    {
        if(rd != -1)
        {
            g_fdtable[rd].node = null;
            g_fdtable[rd].pipeRead = false;
        }
        auto p = cast(Pipe*)n.data;
        free(p);
        free(n);
        return -1;
    }
    fds[0] = rd;
    fds[1] = wr;
    return 0;
}

extern(C) long sys_rendezvous(ulong tag, ulong val, ulong, ulong, ulong, ulong)
{
    return rendezvous(tag, cast(long)val);
}

extern(C) long sys_semacquire(ulong id, ulong, ulong, ulong, ulong, ulong)
{
    return sem_acquire(cast(size_t)id);
}

extern(C) long sys_semrelease(ulong id, ulong, ulong, ulong, ulong, ulong)
{
    return sem_release(cast(size_t)id);
}

extern(C) long sys_brk(ulong addr, ulong, ulong, ulong, ulong, ulong)
{
    auto pid = get_current_pid();
    return brk(pid, cast(size_t)addr);
}

extern(C) long sys_segattach(ulong pid, ulong seg, ulong addr, ulong len, ulong readonly, ulong)
{
    auto id = seg_attach(cast(size_t)pid, cast(void*)addr, cast(size_t)len, readonly != 0);
    return id == size_t.max ? -1 : cast(long)id;
}

extern(C) long sys_segdetach(ulong pid, ulong seg, ulong, ulong, ulong, ulong)
{
    return seg_detach(cast(size_t)pid, cast(size_t)seg);
}

extern(C) long sys_segbrk(ulong pid, ulong seg, ulong len, ulong, ulong, ulong)
{
    return seg_brk(cast(size_t)pid, cast(size_t)seg, cast(size_t)len);
}

extern(C) long sys_segfree(ulong pid, ulong seg, ulong addr, ulong len, ulong, ulong)
{
    return seg_free(cast(size_t)pid, cast(size_t)seg, cast(void*)addr, cast(size_t)len);
}

extern(C) long sys_segflush(ulong pid, ulong seg, ulong addr, ulong len, ulong, ulong)
{
    return seg_flush(cast(size_t)pid, cast(size_t)seg, cast(void*)addr, cast(size_t)len);
}

extern(C) long sys_fversion(ulong msize, ulong versionPtr, ulong fid, ulong, ulong, ulong)
{
    g_p9conn.msize = cast(size_t)msize;
    auto ver = cast(const(char)*)versionPtr;
    size_t len = strlen(ver);
    if(len >= g_p9conn.version_.length)
        len = g_p9conn.version_.length - 1;
    memcpy(g_p9conn.version_.ptr, ver, len);
    g_p9conn.version_[len] = 0;
    g_p9conn.fid = cast(size_t)fid;
    g_p9conn.authenticated = false;
    return 0;
}

extern(C) long sys_fauth(ulong afid, ulong unamePtr, ulong anamePtr, ulong, ulong, ulong)
{
    auto uname = cast(const(char)*)unamePtr;
    auto aname = cast(const(char)*)anamePtr;
    if(uname[0] == 0 || aname[0] == 0)
        return -1;
    g_p9conn.authenticated = true;
    return 0;
}

extern(C) long sys_alarm(ulong timeout, ulong, ulong, ulong, ulong, ulong)
{
    auto pid = get_current_pid();
    if(pid == size_t.max)
        return -1;
    if(timeout == 0)
        g_processes[pid].alarm_tick = 0;
    else
        g_processes[pid].alarm_tick = timer_ticks + timeout;
    return 0;
}

extern(C) long sys_notify(ulong handlerPtr, ulong, ulong, ulong, ulong, ulong)
{
    auto pid = get_current_pid();
    if(pid == size_t.max)
        return -1;
    g_processes[pid].note_handler = cast(NoteHandlerFn)(handlerPtr);
    return 0;
}

extern(C) long sys_noted(ulong mode, ulong, ulong, ulong, ulong, ulong)
{
    // Minimal implementation: simply acknowledge the note
    return (mode == 0) ? 0 : -1;
}

extern(C) long sys_vmcreate(ulong, ulong, ulong, ulong, ulong, ulong)
{
    return vmm_create_vm();
}

extern(C) long sys_vmrun(ulong id, ulong, ulong, ulong, ulong, ulong)
{
    return vmm_run_vm(cast(int)id);
}

extern(C) long sys_vmdestroy(ulong id, ulong, ulong, ulong, ulong, ulong)
{
    vmm_destroy_vm(cast(int)id);
    return 0;
}

extern(C) long sys_container_start(ulong cfgPtr, ulong, ulong, ulong, ulong, ulong)
{
    log_message("sys_container_start called with cfgPtr=");
    log_hex(cfgPtr);
    log_message("\n");
    
    // Validate the config pointer
    if(cfgPtr == 0 || cfgPtr < 0x1000) {
        log_message("sys_container_start: invalid config pointer\n");
        return -1;
    }
    
    start_container(cast(ContainerConfig*)cfgPtr);
    return 0;
}

extern(C) long sys_read_char(ulong, ulong, ulong, ulong, ulong, ulong)
{
    char c = keyboard_getchar();
    return cast(long)c;
}

extern(C) long do_syscall(ulong id, ulong a1, ulong a2, ulong a3, ulong a4, ulong a5, ulong a6)
{
    if(id < g_syscalls.length && g_syscalls[id] !is null)
        return g_syscalls[id](a1, a2, a3, a4, a5, a6);
    return -1;
}

extern(C) void syscall_init()
{
    foreach(i; 0 .. g_syscalls.length)
        g_syscalls[i] = null;
    g_syscalls[cast(size_t)SyscallID.WriteString] = &sys_write_string;
    g_syscalls[cast(size_t)SyscallID.CreateUser]  = &sys_create_user;
    g_syscalls[cast(size_t)SyscallID.Open]        = &sys_open;
    g_syscalls[cast(size_t)SyscallID.Create]      = &sys_create;
    g_syscalls[cast(size_t)SyscallID.PRead]       = &sys_pread;
    g_syscalls[cast(size_t)SyscallID.PWrite]      = &sys_pwrite;
    g_syscalls[cast(size_t)SyscallID.Seek]        = &sys_seek;
    g_syscalls[cast(size_t)SyscallID.Close]       = &sys_close;
    g_syscalls[cast(size_t)SyscallID.Dup]         = &sys_dup;
    g_syscalls[cast(size_t)SyscallID.FD2Path]     = &sys_fd2path;
    g_syscalls[cast(size_t)SyscallID.Stat]        = &sys_stat;
    g_syscalls[cast(size_t)SyscallID.FStat]       = &sys_fstat;
    g_syscalls[cast(size_t)SyscallID.WStat]       = &sys_wstat;
    g_syscalls[cast(size_t)SyscallID.FWStat]      = &sys_fwstat;
    g_syscalls[cast(size_t)SyscallID.Remove]      = &sys_remove;
    g_syscalls[cast(size_t)SyscallID.ChDir]       = &sys_chdir;
    g_syscalls[cast(size_t)SyscallID.Mount]       = &sys_mount;
    g_syscalls[cast(size_t)SyscallID.Bind]        = &sys_bind;
    g_syscalls[cast(size_t)SyscallID.Unmount]     = &sys_unmount;
    g_syscalls[cast(size_t)SyscallID.RFork]       = &sys_rfork;
    g_syscalls[cast(size_t)SyscallID.Exec]        = &sys_exec;
    g_syscalls[cast(size_t)SyscallID.Exit]        = &sys_exit;
    g_syscalls[cast(size_t)SyscallID.ErrStr]      = &sys_errstr;
    g_syscalls[cast(size_t)SyscallID.Sleep]       = &sys_sleep;
    g_syscalls[cast(size_t)SyscallID.Await]       = &sys_await;
    g_syscalls[cast(size_t)SyscallID.Pipe]        = &sys_pipe;
    g_syscalls[cast(size_t)SyscallID.Rendezvous]  = &sys_rendezvous;
    g_syscalls[cast(size_t)SyscallID.SemAcquire]  = &sys_semacquire;
    g_syscalls[cast(size_t)SyscallID.SemRelease]  = &sys_semrelease;
    g_syscalls[cast(size_t)SyscallID.Brk]         = &sys_brk;
    g_syscalls[cast(size_t)SyscallID.SegAttach]   = &sys_segattach;
    g_syscalls[cast(size_t)SyscallID.SegDetach]   = &sys_segdetach;
    g_syscalls[cast(size_t)SyscallID.SegBrk]      = &sys_segbrk;
    g_syscalls[cast(size_t)SyscallID.SegFree]     = &sys_segfree;
    g_syscalls[cast(size_t)SyscallID.SegFlush]    = &sys_segflush;
    g_syscalls[cast(size_t)SyscallID.FVersion]    = &sys_fversion;
    g_syscalls[cast(size_t)SyscallID.FAuth]       = &sys_fauth;
    g_syscalls[cast(size_t)SyscallID.Alarm]       = &sys_alarm;
    g_syscalls[cast(size_t)SyscallID.Notify]      = &sys_notify;
    g_syscalls[cast(size_t)SyscallID.Noted]       = &sys_noted;
    g_syscalls[cast(size_t)SyscallID.VMCreate]    = &sys_vmcreate;
    g_syscalls[cast(size_t)SyscallID.VMRun]       = &sys_vmrun;
    g_syscalls[cast(size_t)SyscallID.VMDestroy]   = &sys_vmdestroy;
    g_syscalls[cast(size_t)SyscallID.ContainerStart] = &sys_container_start;
    g_syscalls[cast(size_t)SyscallID.ReadChar] = &sys_read_char;
    semaphore_init();
}
