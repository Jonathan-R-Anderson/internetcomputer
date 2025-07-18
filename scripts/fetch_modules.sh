#!/bin/sh
set -e
SCRIPT_DIR="$(dirname "$0")"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

fetch_repo() {
    DIR="$1"
    REPO="$2"
    if GIT_TERMINAL_PROMPT=0 git ls-remote "$REPO" >/dev/null 2>&1; then
        if [ ! -d "$PROJECT_ROOT/$DIR/.git" ]; then
            echo "Cloning $REPO into $DIR"
            rm -rf "$PROJECT_ROOT/$DIR"
            git clone --depth 1 "$REPO" "$PROJECT_ROOT/$DIR"
        else
            echo "Updating $DIR"
            cd "$PROJECT_ROOT/$DIR"
            if ! git fetch origin && git reset --hard origin/HEAD; then
                echo "Failed to update $DIR. Skipping." >&2
            fi
        fi
    else
        echo "Repository $REPO not found. Skipping." >&2
    fi
}

fetch_repo kernel https://github.com/Jonathan-R-Anderson/anonymos-microkernel.git
fetch_repo user-services https://github.com/Jonathan-R-Anderson/anonymos-user-services.git
fetch_repo hypervisor https://github.com/Jonathan-R-Anderson/anonymos-hypervisor.git
fetch_repo containers https://github.com/Jonathan-R-Anderson/anonymos-containers.git
fetch_repo object-tree https://github.com/Jonathan-R-Anderson/anonymos-object-tree.git
fetch_repo distributed-fs https://github.com/Jonathan-R-Anderson/distributedFS.git
