// ===== userspace/programs/cat.d =====
module userspace.programs.cat;

import userspace.syscall_wrappers;

void cat(string path) {
    write(1, "contents of " ~ path ~ "\n");
}
