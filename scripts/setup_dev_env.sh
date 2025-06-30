#!/bin/sh
# Setup development environment: fetch sources for POSIX wrappers and the D compiler.
# The -sh shell sources are now bundled under modules/-sh.
set -e
SCRIPT_DIR="$(dirname "$0")"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Fetch POSIX wrappers
"$SCRIPT_DIR/fetch_posix.sh"

# Fetch D compiler sources
"$SCRIPT_DIR/fetch_dmd.sh"
