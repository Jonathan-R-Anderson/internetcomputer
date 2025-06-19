This directory holds userspace utilities and applications.
The design follows Plan 9 conventions: there is no privileged root user and each
process operates within its own namespace.  The default shell is implemented in
Haskell (see `shell/`), providing a Bash‑like experience for navigating the
system.
