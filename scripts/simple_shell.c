#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>

int main() {
    char line[1024];
    char *argv[64];

    while (1) {
        printf("$ ");
        fflush(stdout);

        if (!fgets(line, sizeof(line), stdin)) {
            break;
        }

        line[strcspn(line, "\n")] = 0;

        int i = 0;
        char *token = strtok(line, " ");
        while (token != NULL) {
            argv[i++] = token;
            token = strtok(NULL, " ");
        }
        argv[i] = NULL;

        if (i == 0) {
            continue;
        }

        pid_t pid = fork();
        if (pid == -1) {
            perror("fork");
            continue;
        }

        if (pid == 0) {
            execvp(argv[0], argv);
            perror("execvp");
            _exit(1);
        } else {
            int status;
            waitpid(pid, &status, 0);
        }
    }

    printf("\nGoodbye!\n");
    return 0;
} 