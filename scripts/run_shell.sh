#!/bin/sh
# Helper to launch the Haskell gremlin shell built in userland/shell.
# If the executable is not present, the Makefile target 'gremlin_shell'
# can be used to build it (requires cabal and ghc).

HERE="$(dirname "$0")/.."
exec "$HERE/build/gremlin_shell" "$@"
