#!/bin/sh
# Boot anonymOS ISO with QEMU using serial console
qemu-system-x86_64 -cdrom build/anonymOS.iso -m 512M -nographic -monitor none

