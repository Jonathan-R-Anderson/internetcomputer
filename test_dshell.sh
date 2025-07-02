#!/bin/sh

# test_dshell.sh - Interactive test of the D shell
echo "🚀 Testing anonymOS D Shell - Full third_party/sh Implementation 🚀"
echo ""
echo "The system will boot into a D shell with these commands:"
echo "  help, echo, ls, cd, pwd, clear, history, exit"
echo ""
echo "You can type commands and press Enter to execute them!"
echo "To exit QEMU: Ctrl+A then X"
echo ""
echo "Starting..."

# Run with simple settings for best compatibility
qemu-system-x86_64 \
    -cdrom build/anonymOS.iso \
    -m 512M \
    -nographic \
    -monitor none 
