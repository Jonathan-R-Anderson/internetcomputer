#!/bin/sh
set -e
SCRIPT_DIR="$(dirname "$0")"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
POSIX_DIR="$PROJECT_ROOT/third_party/posix"
# Repository providing the POSIX wrappers used by anonymOS
REPO_URL="https://github.com/Jonathan-R-Anderson/anonymos-posix"
if [ ! -d "$POSIX_DIR/.git" ]; then
    mkdir -p "$PROJECT_ROOT/third_party"
    if [ -d "$POSIX_DIR" ] && [ "$(ls -A "$POSIX_DIR" 2>/dev/null)" ]; then
        echo "Removing existing non-git directory $POSIX_DIR"
        rm -rf "$POSIX_DIR"
    fi
    git clone --depth 1 "$REPO_URL" "$POSIX_DIR"
else
    echo "Updating POSIX repository..."
    cd "$POSIX_DIR"
    git fetch origin main
    git reset --hard origin/main
fi
