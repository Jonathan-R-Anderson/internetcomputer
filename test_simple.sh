#!/bin/sh

# test_simple.sh - Simple VGA mode test
echo "🎮 Testing D Shell with VGA mode..."
echo "This should open a QEMU window where you can type normally."
echo "Press any key to continue..."
read -n 1

# Use VGA mode with real keyboard support
qemu-system-x86_64 \
    -cdrom build/anonymOS.iso \
    -m 512M 