// ===== userspace/programs/ls.d =====
module userspace.programs.ls;

import userspace.syscall_wrappers;

void ls() {
    write(1, "file1\nfile2\n");
}
