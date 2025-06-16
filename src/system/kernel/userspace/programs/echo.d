
// ===== userspace/programs/echo.d =====
module userspace.programs.echo;

import userspace.syscall_wrappers;

void echo(string msg) {
    write(1, msg ~ "\n");
}
