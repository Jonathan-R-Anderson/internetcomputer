module system.loader.snap_loader;

import system.loader.config;
import fs.node; // For FSNode
import stl.io.log : Log;
import task.scheduler : Scheduler, KernelTaskFunction; // For adding tasks
import stl.vmm.heap : newStruct;
import task.thread : VMProcess, VMThread, ImageInfo, TLS;
import stl.elf64;
import arch.paging : VMPageFlags, getKernelPaging, makeAddress;
import kmain : instantiateELF, newUserStack; // For ELF loading and stack
import stl.arch.amd64.msr : MSR;
import stl.arch.amd64.gdt : GDT;
import syscall : SyscallHandler;


extern extern (C) void switchToUserMode();

struct SnapLoader {
    static SystemConfig* globalConfig;
    static FSNode** rootFsNode; // Pointer to the root FSNode (e.g., initrdFS)

    static void init(SystemConfig* config, FSNode** rootFS) {
        globalConfig = config;
        rootFsNode = rootFS;
        Log.info("SnapLoader initialized.");
    }

    static bool launchSnapByName(string snapName) {
        if (!globalConfig) {
            Log.error("SnapLoader: SystemConfig not initialized.");
            return false;
        }
        if (snapName !in globalConfig.snapSpecificConfigs) {
            Log.error("SnapLoader: Manifest for snap '", snapName, "' not found in config.");
            return false;
        }
        auto snapDetails = globalConfig.snapSpecificConfigs[snapName];
        return launchSnap(snapName, &snapDetails);
    }

    static bool launchSnap(string snapName, SystemConfig.SnapDetails* details) {
        Log.info("Launching snap: ", snapName, " from path: ", details.path, " with entry: ", details.entry);

        if (!rootFsNode || !*rootFsNode) {
            Log.error("SnapLoader: Root FSNode not available.");
            return false;
        }

        // 1. Locate the snap's entry point in the filesystem
        // The path in SnapDetails should be relative to some root or absolute if VFS is mature.
        // For now, assume details.path is like "/apps/snap_dir" and details.entry is "/bin/service"
        // So, effective path is details.path ~ details.entry
        string fullEntryPath = details.path ~ details.entry; // This needs VFS path joining
        if (fullEntryPath.startsWith("//")) fullEntryPath = fullEntryPath[1..$]; // Basic path normalization

        Log.info("SnapLoader: Attempting to find entry point: ", fullEntryPath);
        auto snapFsNode = (*rootFsNode).findNode(fullEntryPath);

        if (!snapFsNode) {
            Log.error("SnapLoader: Entry point '", fullEntryPath, "' for snap '", snapName, "' not found.");
            return false;
        }

        // 2. Load ELF (similar to how 'init' is loaded in kmain)
        // Assuming snapFsNode.readData gives the ELF content
        ubyte[] elfData;
        ulong bytesRead = snapFsNode.readData(elfData, 0); // This needs FSNode to support reading into a dynamic array
        if (bytesRead == 0 || elfData.length == 0) {
             Log.error("SnapLoader: Could not read ELF data for snap '", snapName, "'");
             return false;
        }
        
        ELF64 snapElf = ELF64(stl.address.VirtMemoryRange.fromArray(elfData));
        if (!snapElf.isValid) {
            Log.error("SnapLoader: ELF for snap '", snapName, "' is not valid.");
            return false;
        }

        // 3. Create a new process/thread for the snap
        // This is a simplified version. Real sandboxing (namespaces, syscall filters) is complex.
        VMThread* currentThread = Scheduler.getCurrentThread; // Get current thread to fork from or create anew
        VMProcess* snapProcess = newStruct!VMProcess(PhysAddress()); // New address space
        VMThread* snapThread = newStruct!VMThread;

        snapThread.process = snapProcess;
        snapProcess.bind(); // Activate the new address space

        kmain.ELFInstance snapElfInstance = instantiateELF(snapThread, snapElf);

        // TODO: Apply SnapManifest.syscalls as a filter (requires syscall dispatcher modification)
        // TODO: Use SnapManifest.capabilities to bind-mount only allowed devices/files (requires VFS enhancements)
        Log.info("Snap '", snapName, "' syscalls: ", details.syscalls.length ? details.syscalls.toString : "none");
        Log.info("Snap '", snapName, "' capabilities: ", details.capabilities.length ? details.capabilities.toString : "none");

        // 4. Set up stack and jump to entry point (simplified from kmain)
        // ... (stack setup, argument passing if any, TLS setup) ...
        // For now, just log that we would execute it.
        // A proper execution would involve setting up user mode, stack, and jumping.
        Log.info("SnapLoader: ELF loaded for '", snapName, "'. Entry point: ", snapElfInstance.main.to!string);
        Log.info("Snap '", snapName, "' would be executed here. (Full execution not yet implemented in stub)");

        // Scheduler.addKernelTask(snapName, Scheduler.getCPUInfo(0), cast(KernelTaskFunction)snapElfInstance.main, null);
        return true;
    }
}