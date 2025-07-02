#!/bin/bash
set -e
LDC_PATH=$(which ldc2)
make -C /home/ubuntu/Code/internetcomputer_backup/phobos -f Makefile -j1 \
    MODEL=64 \
    BUILD=release \
    DMD="$LDC_PATH" \
    CC=clang \
    UDFLAGS="-Isrc -Iimport -w -de -preview=dip1000 -preview=fieldwise -fPIC -O -release -inline -mtriple=x86_64-anonymos-none -L-L../sysroot/lib" \
    CFLAGS="-m64 -fPIC -DHAVE_UNISTD_H -O3 --target=x86_64-anonymos-none" \
    SHARED=0 \
    target
cp phobos/generated/linux/release/64/libphobos.a sysroot/lib/ 