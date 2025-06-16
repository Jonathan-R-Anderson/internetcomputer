// ===== userspace/shell.d =====
module userspace.shell;

import userspace.syscall_wrappers;

void shell() {
    while (true) {
        write(1, "> ");
        string input;
        readln(input);
        if (input == "exit")
            break;
        else if (input == "ls")
            ls();
        else if (input.length)
            write(1, "Unknown command\n");
    }
}

void ls() {
    write(1, "file1\nfile2\n");
}
