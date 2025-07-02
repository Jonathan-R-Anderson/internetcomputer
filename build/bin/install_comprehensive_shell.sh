#!/bin/sh
# Runtime installer for comprehensive shell
echo "Installing comprehensive shell from /third_party/sh..."
cd /third_party/sh
if [ -x "build_runtime.sh" ]; then
    ./build_runtime.sh
elif [ -x "interpreter" ]; then
    cp interpreter /bin/sh_comprehensive
    echo "Comprehensive shell installed as /bin/sh_comprehensive"
else
    echo "Comprehensive shell source available but not compiled"
    echo "D compiler needed to build: /bin/dmd"
fi
