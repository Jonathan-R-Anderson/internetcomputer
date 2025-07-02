#!/bin/bash

# Create filesystem image with embedded shell binary

if [ ! -f "build/bin/sh" ]; then
    echo "Error: Shell binary not found at build/bin/sh"
    echo "Please build the shell first with: make build/bin/sh"
    exit 1
fi

echo "Creating filesystem image with embedded shell..."

# Create directory structure
cat > build/fs.img << 'EOF'
D /
D /sys
D /sys/boot
D /sys/kernel
D /sys/drivers
D /sys/init
D /sys/profiles
D /apps
D /apps/coreutils
D /apps/coreutils/v1.2.3
D /apps/browser
D /apps/browser/v105.0
D /apps/editor
D /apps/editor/v3.1
D /bin
D /third_party
D /third_party/sh
D /third_party/dmd
D /users
D /users/alice
D /users/alice/bin
D /users/alice/cfg
D /users/alice/doc
D /users/alice/media
D /users/alice/projects
D /users/alice/vault
D /users/bob
D /users/bob/bin
D /users/bob/cfg
D /users/bob/doc
D /users/bob/media
D /users/bob/projects
D /users/bob/vault
D /srv
D /srv/sshd
D /srv/web
D /srv/dns
D /srv/db
D /cfg
D /cfg/users
D /cfg/network
D /cfg/system
D /vol
D /vol/usb0
D /vol/backup_drive
D /vol/encrypted_partition
D /log
D /run
D /tmp
D /dev
D /net
D /net/ip
D /net/tcp
D /net/dns
F /bin/sh
EOF

# Append the actual shell binary content
cat build/bin/sh >> build/fs.img

# Add other default files (empty)
cat >> build/fs.img << 'EOF'
F /cfg/hostname
F /cfg/users/alice.json
F /cfg/users/bob.json
F /cfg/network/interfaces.json
F /cfg/system/packages.json
EOF

echo "Filesystem image created at build/fs.img"
echo "Shell binary size: $(stat -c%s build/bin/sh) bytes"
echo "Total filesystem image size: $(stat -c%s build/fs.img) bytes" 