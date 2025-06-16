module kernel.fs;

import stl.io.log : Log;
import stl.vector : Vector; // For directory listings

enum NodeType {
    File,
    Directory,
    Symlink,
    Device,
    MountPoint, // For VFS internal use
    Overlay // Special node type for overlay FS
}

/// Represents a node in a filesystem (file, directory, device, etc.)
interface FsNode {
    string getName();
    NodeType getType();
    ulong getSize();

    // For files/devices
    ulong read(ulong offset, ubyte[] buffer);
    ulong write(ulong offset, const ubyte[] buffer);

    // For directories
    FsNode lookup(string name);
    Vector!FsNode list(); // Consider returning names or lightweight structs

    // For mount points / overlays
    FsNode getUnderlyingNode(); // For mount points
    FsNode[] getOverlayLayers(); // For overlay nodes [writable_layer, readonly_layer1, ...]
}

/// Represents a filesystem implementation
interface FileSystem {
    FsNode getRootNode();
    string getFsType(); // e.g., "tarfs", "ramfs", "overlayfs"
    // Potentially: mount, unmount, sync operations
}

// --- Example: A simple In-Memory Filesystem Node (for /writable or testing) ---
class RamFsNode : FsNode {
    private string _name;
    private NodeType _type;
    private ubyte[] _data; // For files
    private Vector!RamFsNode _children; // For directories

    this(string name, NodeType type) {
        this._name = name;
        this._type = type;
        if (type == NodeType.Directory) {
            _children = Vector!RamFsNode();
        } else if (type == NodeType.File) {
            _data = new ubyte[0];
        }
    }

    string getName() { return _name; }
    NodeType getType() { return _type; }
    ulong getSize() { return _type == NodeType.File ? _data.length : 0; }

    ulong read(ulong offset, ubyte[] buffer) {
        if (_type != NodeType.File) return 0;
        // Simplified read
        ulong lenToRead = buffer.length;
        if (offset + lenToRead > _data.length) {
            lenToRead = _data.length - offset;
        }
        if (offset >= _data.length || lenToRead == 0) return 0;
        buffer[0 .. lenToRead] = _data[offset .. offset + lenToRead];
        return lenToRead;
    }

    ulong write(ulong offset, const ubyte[] buffer) {
        if (_type != NodeType.File) return 0;
        // Simplified write, may need to resize _data
        if (offset + buffer.length > _data.length) {
             // Resize _data - this is complex, for now, append simply
             ubyte[] newData = new ubyte[offset + buffer.length];
             newData[0 .. _data.length] = _data[];
             _data = newData;
        }
        _data[offset .. offset + buffer.length] = buffer[];
        return buffer.length;
    }

    FsNode lookup(string name) {
        if (_type != NodeType.Directory) return null;
        foreach(child; _children) {
            if (child.getName() == name) return child;
        }
        return null;
    }
    Vector!FsNode list() {
        if (_type != NodeType.Directory) return Vector!FsNode(); // Empty
        Vector!FsNode result;
        foreach(child; _children) result.put(child);
        return result;
    }
    FsNode getUnderlyingNode() { return null; }
    FsNode[] getOverlayLayers() { return null; }

    // RamFs specific
    void addChild(RamFsNode child) {
        if (_type == NodeType.Directory) _children.put(child);
    }
}
}