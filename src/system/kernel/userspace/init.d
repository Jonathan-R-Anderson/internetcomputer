// ===== userspace/init.d =====
module userspace.init;

import userspace.shell;

int main(string[] args) {
    write(1, "Init started\n");
    shell();
    return 0;
}