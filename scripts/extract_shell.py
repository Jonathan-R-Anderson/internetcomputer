#!/usr/bin/env python3

import os
import sys

def extract_shell_binary():
    """Extract the embedded shell binary to build/bin/sh"""
    
    # Read the embedded shell binary data
    with open('embedded_shell_binary.d', 'r') as f:
        content = f.read()
    
    # Find the embedded_shell_binary array
    start_marker = "__gshared immutable ubyte[] embedded_shell_binary = ["
    end_marker = "];"
    
    start_idx = content.find(start_marker)
    if start_idx == -1:
        print("Error: Could not find embedded shell binary data")
        return False
    
    start_idx += len(start_marker)
    end_idx = content.find(end_marker, start_idx)
    if end_idx == -1:
        print("Error: Could not find end of embedded shell binary data")
        return False
    
    # Extract the hex bytes
    hex_data = content[start_idx:end_idx]
    
    # Parse hex bytes
    bytes_data = []
    for line in hex_data.split('\n'):
        line = line.strip()
        if not line:
            continue
        # Remove comments and extra whitespace
        if '//' in line:
            line = line[:line.find('//')]
        # Extract hex values
        hex_values = [x.strip() for x in line.split(',') if x.strip()]
        for hex_val in hex_values:
            hex_val = hex_val.strip()
            if hex_val.startswith('0x') and len(hex_val) > 2:
                try:
                    byte_val = int(hex_val, 16)
                    bytes_data.append(byte_val)
                except ValueError:
                    pass
    
    if not bytes_data:
        print("Error: No valid hex bytes found")
        return False
    
    # Create output directory
    os.makedirs('build/bin', exist_ok=True)
    
    # Write binary file
    with open('build/bin/sh', 'wb') as f:
        f.write(bytes(bytes_data))
    
    # Make executable
    os.chmod('build/bin/sh', 0o755)
    
    print(f"Extracted shell binary to build/bin/sh ({len(bytes_data)} bytes)")
    return True

if __name__ == "__main__":
    if extract_shell_binary():
        print("Shell binary extraction successful!")
        sys.exit(0)
    else:
        print("Shell binary extraction failed!")
        sys.exit(1) 