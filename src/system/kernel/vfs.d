module kernel.vfs;

import kernel.fs;
import stl.io.log : Log;
import stl.vector : Vector;

/// Virtual File System Manager
class VFS {
    private FsNode _root;
    private Vector!MountInfo _mountTable;

    private struct MountInfo {
        string path;       // Mount path in the VFS
        FileSystem mountedFs; // The filesystem mounted here
        FsNode mountPointNode; // The node in the parent FS that is mounted over
        FsNode mountedRootNode; // The root node of the mounted FS
    }

    this() {
        // Initialize with a root RamFs for "/"
        // This root could later be an overlay of /base (readonly) and /writable (ramfs or persistent)
        auto rootRamFs = new RamFs("root_ramfs"); // Assuming RamFs constructor
        this._root = rootRamFs.getRootNode();
        _mountTable = Vector!MountInfo();
        Log.info("VFS initialized with a RamFs root.");
    }

    /// Mounts a filesystem at a given path.
    /// TODO: Implement overlay mounting logic here.
    bool mount(string path, FileSystem fsToMount, string fsType = "") {
        if (path == "/" && fsToMount.getRootNode() !is null) { // Special case for replacing root
            this._root = fsToMount.getRootNode();
            Log.info("VFS: Filesystem '", fsToMount.getFsType(), "' mounted at /");
            return true;
        }

        FsNode mountTargetNode = resolvePathNode(path, false);
        if (!mountTargetNode || mountTargetNode.getType() != NodeType.Directory) {
            Log.error("VFS: Mount path '", path, "' not found or not a directory.");
            return false;
        }

        // Basic mount
        MountInfo info;
        info.path = path;
        info.mountedFs = fsToMount;
        info.mountPointNode = mountTargetNode; // The directory node we are mounting on
        info.mountedRootNode = fsToMount.getRootNode();
        _mountTable.put(info);

        // Mark the original node as a MountPoint internally if VFS supports it
        // This is conceptual; FsNode might need a setType or similar, or VFS handles it.
        Log.info("VFS: Filesystem '", fsToMount.getFsType(), "' mounted at '", path, "'");
        return true;
    }

    /// Resolves a path to an FsNode.
    /// Returns null if path not found.
    /// `followMounts` controls if we look into mounted filesystems.
    FsNode resolvePathNode(string path, bool followMounts = true) {
        if (path.length == 0) return null;
        if (path == "/") return _root;

        string[] parts = path.strip("/").split("/");
        FsNode current = _root;

        foreach (i, part; parts) {
            if (current is null) return null;

            // Check if 'current' is a mount point for the *current* segment of the path
            if (followMounts) {
                string currentPathSegment = "/" ~ parts[0 .. i].join("/");
                foreach (ref mountInfo; _mountTable) {
                    if (mountInfo.path == currentPathSegment) {
                        current = mountInfo.mountedRootNode; // Switch to the mounted FS's root
                        // Log.info("VFS: Crossed mount point at '", mountInfo.path, "', now in '", mountInfo.mountedFs.getFsType(), "'");
                        break; 
                    }
                }
            }
            
            if (current.getType() != NodeType.Directory) return null; // Cannot traverse non-directory
            current = current.lookup(part);
        }
        return current;
    }

    /// Opens a file/directory at the given path.
    FsNode open(string path) {
        Log.info("VFS: Opening '", path, "'");
        FsNode node = resolvePathNode(path);
        if (!node) {
            Log.warning("VFS: Path '", path, "' not found.");
            return null;
        }
        // TODO: Handle overlay logic if node is an OverlayNode
        // If node.getType() == NodeType.Overlay, then node.read/write should handle layers.
        return node;
    }

    // TODO: mkdir, rmdir, create, delete, etc.
    // These operations need to be aware of overlays for /writable.
}
