// ===== kernel/syscall/open.d =====
module kernel.syscall.open;

import kernel.fs;

size_t open(size_t ptr, size_t unused) {
    string path = cast(string) ptr;
    auto file = open(path);
    return cast(size_t) file;
}