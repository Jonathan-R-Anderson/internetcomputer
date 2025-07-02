#!/bin/sh

# test_keyboard.sh - Test keyboard input with different QEMU modes
echo "🔧 Testing keyboard input modes..."
echo ""
echo "Choose test mode:"
echo "1. VGA mode (best for real keyboard input)"
echo "2. No graphics mode (for headless)"
echo "3. Serial console"
echo ""
read -p "Enter choice (1-3): " choice

case $choice in
    1)
        echo "🎮 Starting with VGA display mode..."
        echo "Use normal keyboard input. Close window to exit."
        qemu-system-x86_64 \
            -cdrom build/anonymOS.iso \
            -m 512M \
            -display gtk
        ;;
    2)
        echo "📺 Starting with nographic mode..."
        echo "Type directly in terminal. Ctrl+A then X to exit."
        qemu-system-x86_64 \
            -cdrom build/anonymOS.iso \
            -m 512M \
            -nographic
        ;;
    3)
        echo "🔌 Starting with serial console..."
        echo "Type in terminal. Ctrl+A then X to exit."
        qemu-system-x86_64 \
            -cdrom build/anonymOS.iso \
            -m 512M \
            -nographic \
            -serial stdio
        ;;
    *)
        echo "Invalid choice. Using VGA mode..."
        qemu-system-x86_64 \
            -cdrom build/anonymOS.iso \
            -m 512M \
            -display gtk
        ;;
esac 