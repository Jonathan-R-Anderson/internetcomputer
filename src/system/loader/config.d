module system.loader.config;

import stl.text;
import stl.io.log : Log;
import stl.fs.file : readText;
import std.conv : to;

/// Describes the entire system configuration loaded from config.toml
struct SystemConfig {
    // [system]
    string hostname = "PowerNex";
    string timezone = "UTC";
    string[] boot_snaps; // Snaps to load at boot
    string[] services;   // Other services to start
 
    // [network]
    struct NetworkConfig {
        bool enabled;
        string[] interfaces; // Changed from iface to interfaces
    }
    NetworkConfig network;

    // [ethereum]
    struct EthereumConfig {
        bool enabled;
        string provider; // Changed from node to provider
        string wallet_private_key;
        bool use_zksync;
        string zksync_api_url;
    }
    EthereumConfig ethereum;

    // [compute]
    struct ComputeConfig {
        bool allow_rented_devices;
        string default_encryption = "none"; // e.g., "homomorphic" or "none"
        string[] trusted_nodes;
        bool enable_fhe_device;
    }
    ComputeConfig compute;

    // [snaps.*] - This will be a map of snap-specific configurations
    // Example: [snaps.mysnap]
    // path = "/apps/mysnap"
    // entry = "/bin/mysnap_service"
    struct SnapDetails {
        string path;
        string entry;
        string[] syscalls;
        string[] capabilities;
    }
    SnapDetails[string] snapSpecificConfigs;
}

/// Loads and parses the system configuration from the given TOML file path.
SystemConfig loadSystemConfig(string path = "/writable/system/config.toml") @safe {
	auto content = readText(path);

	SystemConfig config;
	string currentSection;
    string currentSnapName;

	foreach (line; content.splitLines) {
		line = line.strip;
		if (line.length == 0 || line.startsWith("#"))
			continue;

		// Section headers like [system], [network]
		if (line.startsWith("[") && line.endsWith("]")) {
			currentSection = line.strip("[]");
            if (currentSection.startsWith("snaps.")) {
                currentSnapName = currentSection["snaps.".length .. $];
            } else {
                currentSnapName = null; // Not in a snap-specific section
            }
			continue;
		}

		auto parts = line.split("=", 2);
		if (parts.length != 2)
			continue;

		auto key = parts[0].strip;
		auto val = parts[1].strip.strip(`"`);

        if (currentSnapName.length > 0) {
            SystemConfig.SnapDetails details;
            if (currentSnapName in config.snapSpecificConfigs) {
                details = config.snapSpecificConfigs[currentSnapName];
            }
            if (key == "path") details.path = val;
            else if (key == "entry") details.entry = val;
            else if (key == "syscalls") details.syscalls = val.strip("[]").split(",").map!(s => s.strip.strip(`"`)).array;
            else if (key == "capabilities") details.capabilities = val.strip("[]").split(",").map!(s => s.strip.strip(`"`)).array;
            config.snapSpecificConfigs[currentSnapName] = details;
        } else final switch (currentSection) {
            case "system":
                if (key == "hostname") config.hostname = val;
                else if (key == "timezone") config.timezone = val;
                else if (key == "boot_snaps") config.boot_snaps = val.strip("[]").split(",").map!(s => s.strip.strip(`"`)).array;
                else if (key == "services") config.services = val.strip("[]").split(",").map!(s => s.strip.strip(`"`)).array;
                break;
            case "network":
                if (key == "enabled") config.network.enabled = (val == "true");
                else if (key == "interfaces") config.network.interfaces = val.strip("[]").split(",").map!(s => s.strip.strip(`"`)).array;
                break;
            case "ethereum":
                if (key == "enabled") config.ethereum.enabled = (val.toLower == "true");
                else if (key == "provider") config.ethereum.provider = val; // e.g. http://localhost:8545
                else if (key == "wallet_private_key") config.ethereum.wallet_private_key = val; // BE CAREFUL with this
                else if (key == "use_zksync") config.ethereum.use_zksync = (val.toLower == "true");
                else if (key == "zksync_api_url") config.ethereum.zksync_api_url = val;
                break;
            case "compute":
                if (key == "allow_rented_devices") config.compute.allow_rented_devices = (val.toLower == "true");
                else if (key == "default_encryption") config.compute.default_encryption = val;
                else if (key == "trusted_nodes") config.compute.trusted_nodes = val.strip("[]").split(",").map!(s => s.strip.strip(`"`)).array;
                else if (key == "enable_fhe_device") config.compute.enable_fhe_device = (val.toLower == "true");
                break;
            default:
                Log.warning("Unknown section in config.toml: ", currentSection);
                break;
        }
	}

    Log.info("SystemConfig loaded. Hostname: ", config.hostname, ", Boot Snaps: ", config.boot_snaps.length);
	return config;
}
