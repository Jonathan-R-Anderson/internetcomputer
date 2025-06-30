#!/bin/sh
set -e
SCRIPT_DIR="$(dirname "$0")"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SH_DIR="$PROJECT_ROOT/modules/-sh"
# Repository for the interactive shell included with anonymOS
REPO_URL="https://github.com/Jonathan-R-Anderson/-sh"
if [ ! -d "$SH_DIR/.git" ]; then
    mkdir -p "$PROJECT_ROOT/modules"
    git clone --depth 1 "$REPO_URL" "$SH_DIR"
else
    git -C "$SH_DIR" pull --ff-only
fi
