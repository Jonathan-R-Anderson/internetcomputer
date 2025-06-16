#!/usr/bin/env rdmd
/**
 * PowerNex's toolchain manager
 *
 * Copyright: © 2017, Dan Printzell
 * License: $(LINK2 https://www.mozilla.org/en-US/MPL/2.0/, Mozilla Public License Version 2.0)
 *  (See accompanying file LICENSE)
 * Authors: Dan Printzell
 */

pragma(lib, "curl");

import core.sys.posix.stdio : fgets;
import core.sys.posix.unistd : isatty;
import core.stdc.string : strlen;
import std.stdio : write, writeln, stdout, stderr, stdin, File;
import std.file : exists, readText, fwrite = write, rmdirRecurse, mkdirRecurse, read, remove, getcwd;
import std.ascii : toLower, toUpper;
import std.json : JSONValue, parseJSON, JSONType;
import std.net.curl : get, HTTP;
import std.range : repeat;
import std.math : isNaN;
import std.format : format;
import std.process : executeShell, pipeShell, wait, ProcessPipes, Redirect;
import std.conv : to, ConvException;
import std.typecons : Yes, No, Flag;
import std.getopt : getopt, defaultGetoptPrinter;
import std.process : ProcessException;
import std.path : buildPath, absolutePath;
import std.string : strip, replace;

//
enum size_t major = 1;
enum size_t minor = 0;
enum size_t patch = 0;

//Flags
bool showVersion;
bool clean;
bool noconfirm;

void normal(Args...)(Args args) {
	write("\x1b[39;1m", args, "\x1b[0m");
}

void good(Args...)(Args args) {
	write("\x1b[33;1m", args, "\x1b[0m");
}

void warning(Args...)(Args args) {
	stderr.write("\x1b[31;1m", args, "\x1b[0m");
}

void error(Args...)(Args args) {
	stderr.write("\x1b[37;41;1m", args, "\x1b[0m");
}

//Version = Successfull ci build
struct VersionInfo {
	size_t binutilsVersionId;
	string binutilsVersionTag;
	string binutilsCommitHash;
}

const string toolchainFolder = "build/cc";
const string versionInfoFile = toolchainFolder ~ "/versionInfo";
const string toolchainSrcFolder = "build/src";

VersionInfo getOldInfo() {
	VersionInfo oldVI;
	if (!exists(versionInfoFile)) return oldVI;
	try {
		JSONValue data = parseJSON(readText(versionInfoFile));
		if (auto val = "binutilsCommitHash" in data) {
			if (val.type == JSONType.string) oldVI.binutilsCommitHash = val.str.idup;
		}
	} catch (Exception e) {
		warning("Could not parse versionInfo: ", e.msg, "\n");
	}
	return oldVI;
}


private string getGitCommitHash(string repoPath) {
	try {
		auto result = executeShell("git -C \"" ~ repoPath ~ "\" rev-parse HEAD");
		if (result.status == 0) return result.output.strip.idup;
		warning("Failed to get git commit hash: ", result.output, "\n");
	} catch (ProcessException e) {
		warning("Git error: ", e.msg, "\n");
	}
	return null;
}


private bool ensureRepo(string url, string path, bool fullCloneIfNew = false) {
	try {
		if (!exists(path ~ "/.git")) {
			mkdirRecurse(path);
string cloneCmd = "git clone ";
            if (!fullCloneIfNew) { // If not requesting full, use shallow for non-version-pinned repos
                cloneCmd ~= "--depth 1 ";
            }
            // If fullCloneIfNew is true, it will be a full clone.
            cloneCmd ~= url ~ " \"" ~ path ~ "\"";
			normal("Cloning ", url, " into ", path, " with command: ", cloneCmd, "\n");
			auto res = executeShell(cloneCmd);
			if (res.status != 0) {
				error("Clone failed: ", res.output, "\n");
				error("Clone failed for ", url, ": ", res.output, "\n");
				return false;
			}
            // After a fresh clone, ensure tags are fetched if we need them for versioning,
            // especially for the main binutils-gdb repo.
            if (url == "git://sourceware.org/git/binutils-gdb.git") {
                normal("Fetching tags for ", path, "...\n");
                auto fetchTagRes = executeShell("git -C \"" ~ path ~ "\" fetch --tags");
                if (fetchTagRes.status != 0) {
                    warning("Failed to fetch tags for ", path, ": ", fetchTagRes.output, "\n");
                    // Not necessarily fatal, checkout might still work if tag was included in initial clone
                }
            }
			if (res.status != 0) {
				error("Clone failed: ", res.output, "\n");
				return false;
			}
		} else {
            normal("Repository at ", path, " already exists. Fetching updates and tags...\n");
			auto fetchRes = executeShell("git -C \"" ~ path ~ "\" fetch --all --tags"); // Fetch all branches and tags
            if (fetchRes.status != 0) {
                warning("Failed to fetch updates for ", path, ": ", fetchRes.output, "\n");
                // Continue, as the existing local repo might still be usable.
            }
		}
		return true;
	} catch (ProcessException e) {
		error("Git process error on ", path, ": ", e.msg, "\n");
		return false;
	}
}

VersionInfo getNewInfo() {
	VersionInfo newVI;
	string binutilsVersionTag = "binutils-2_28"; // Target tag for binutils 2.28
	string upstreamUrl = "git://sourceware.org/git/binutils-gdb.git";
	string upstreamPath = toolchainSrcFolder ~ "/binutils-gdb";
	string patchUrl = "https://github.com/PowerNex/powernex-binutils.git";
	string patchPath = toolchainSrcFolder ~ "/powernex-binutils";

	mkdirRecurse(toolchainSrcFolder);
	if (!ensureRepo(upstreamUrl, upstreamPath)) return newVI;
	if (!ensureRepo(patchUrl, patchPath, false)) return newVI; // Patch repo can be shallow

	// Checkout the specific binutils version in upstreamPath
    string checkoutCmd = "bash -c 'cd \"" ~ upstreamPath ~ "\" && " ~
                       "git checkout tags/" ~ binutilsVersionTag ~ " -B " ~ binutilsVersionTag ~ "_local_branch && " ~
                       "git reset --hard HEAD'"; // Ensure clean state on the tag branch
    normal("Attempting to checkout tag: ", binutilsVersionTag, " in ", upstreamPath, "\n");
    auto checkoutResult = executeShell(checkoutCmd);
    if (checkoutResult.status != 0) {
        error("Failed to checkout binutils tag ", binutilsVersionTag, " in ", upstreamPath, ". Error:\n", checkoutResult.output);
        return newVI;
    }
    normal("Checked out ", binutilsVersionTag, " to local branch ", binutilsVersionTag, "_local_branch in ", upstreamPath, "\n");
	string actualPatchFile = buildPath(patchPath, "binutils-2.28.patch");
	string absoluteActualPatchFile = absolutePath(actualPatchFile);

	auto patchResult = executeShell(
		"bash -c 'cd \"" ~ upstreamPath ~ "\" && " ~
		"git checkout -B powernex " ~ binutilsVersionTag ~ "_local_branch && " ~ 
		"git apply -p1 --whitespace=fix --reject \"" ~ absoluteActualPatchFile ~ "\" && " ~
		"git add -A && " ~
		"git commit --allow-empty -m \"Applied PowerNex custom patches from " ~ actualPatchFile.replace("\"", "\\\"") ~ " to " ~ binutilsVersionTag ~ "\"'"
	);


	if (patchResult.status != 0) {
		error("Patch application failed for \"", absoluteActualPatchFile, "\". Error:\n", patchResult.output);
		return newVI;
	}

	newVI.binutilsCommitHash = getGitCommitHash(upstreamPath);
	return newVI;
}



char question(char defaultAlt, char[] alternative, Args...)(Args args) {
	char[64] data;

	warning(args, " [");
	foreach (idx, alt; alternative) {
		if (idx)
			warning("/");
		if (alt == defaultAlt)
			alt = alt.toUpper;
		warning(alt);
	}
	warning("]: ");

	stdout.flush();
	stderr.flush();

	if (noconfirm) {
		warning("\n");
		return defaultAlt;
	}

	if (!fgets(data.ptr, data.length, stdin.getFP)) {
		error("[fgets] Is stdin valid?\n");
		return char.init;
	}

	char[] input = data[0 .. data.ptr.strlen];

	char[2] altStr;
	altStr[1] = '\0';
	foreach (alt; alternative) {
		altStr[0] = alt.toLower;

		if (!strcasecmp(altStr, input))
			return alt;
	}
	if (!strcasecmp("\n", input))
		return defaultAlt;

	error("Invalid choice!\n");
	return char.init;
}

struct SaveFile {
	File f;

	@disable this();
	this(string path) {
		f = File(path, "wb");
	}

	~this() {
		f.close();
	}

	size_t opCall(ubyte[] data) {
		f.rawWrite(data);
		return data.length;
	}
}

struct ProcessPipe {
	ProcessPipes p;

	@disable this();
	this(string command, Flag!"IgnoreStdOut" ignoreStdOut = No.IgnoreStdOut, Flag!"IgnoreStdErr" ignoreStdErr = No.IgnoreStdErr) {
		Redirect flags = Redirect.stdin;
		if (ignoreStdOut)
			flags |= Redirect.stdout;
		if (ignoreStdErr)
			flags |= Redirect.stderr;

		p = pipeShell(command, flags);
	}

	~this() {
		p.stdin.flush();
		p.stdin.close();
		wait(p.pid);
	}

	size_t opCall(ubyte[] data) {
		p.stdin.rawWrite(data);
		return data.length;
	}
}


bool checkGitExists() {
    try {
        auto result = executeShell("git --version");
        return result.status == 0;
    } catch (ProcessException) {
        return false;
    }
}

bool checkDmdExists() {
    try {
        auto result = executeShell("dmd --version");
        return result.status == 0;
    } catch (ProcessException) {
        return false;
    }
}


private string getAbsoluteInstallPrefix() {
    return getcwd() ~ "/" ~ toolchainFolder;
}

void downloadProgress(T = SaveFile, Args...)(string name, const(char)[] url, Args args) {
	T receiver = T(args);
	HTTP http = HTTP(url); // Because opCall
	http.onReceive = (ubyte[] data) => receiver(data);

	normal("\x1b[?25l");

	static float lastDiff = -1;

	http.onProgress = (size_t total, size_t current, size_t _, size_t __) {
		import std.string : leftJustifier;

		enum width = 64;
		float fDiff = cast(float)current / cast(float)total;
		if (fDiff.isNaN)
			fDiff = 0;
		if (cast(size_t)(100 * lastDiff) == cast(size_t)(100 * fDiff))
			return 0;

		size_t procent = cast(size_t)(100 * fDiff);

		size_t filled = cast(size_t)(width * fDiff * 8);

		dchar[] step = [' ', '▏', '▎', '▍', '▋', '▊', '▉', '█'];

		long fullFilled = cast(long)(filled) / 8;
		if (fullFilled < 0)
			fullFilled = 0;
		long empty = width - fullFilled - 1;
		if (empty < 0)
			empty = 0;

		normal("\r", name, ":", leftJustifier("", 8 - name.length + 1, ' '), format("%3d", procent), "% \x1b[36;46;1m",
				repeat(step[$ - 1], fullFilled), repeat(step[filled % 8], (procent != 100) * 1), repeat(step[0], empty));
		return 0;
	};
	http.perform();
	normal("\x1b[?25h\n");
}

int main(string[] args) {
	static string versionMsg = "PowerNex's toolchain manager v" ~ major.to!string ~ "." ~ minor.to!string ~ "."
		~ patch.to!string ~ "\n" ~ "Copyright © 2017, Dan Printzell - https://github.com/Vild/PowerNex";


	if (!checkGitExists()) {
		error("Git is not installed or not found in PATH. Please install Git to proceed.\n");
		return 1;
	}

	if (!checkDmdExists()) {
		error("DMD (D Compiler) is not installed or not found in PATH.\n");
		error("This script relies on a system-installed DMD. Please install DMD to proceed.\n");
		error("You can typically install it from https://dlang.org/download.html or your system's package manager.\n");
		return 1;
	}
	// dfmt off
	auto helpInformation = getopt(args,
		"v|version", "Show the updaters version", &showVersion,
		"c|clean", "Clean out the toolchain folder before starting", &clean,
		"noconfirm", "Always choose the default answer to questions", &noconfirm
	);
	// dfmt on

	if (helpInformation.helpWanted) {
		defaultGetoptPrinter(versionMsg, helpInformation.options);
		return 0;
	}
	if (showVersion) {
		writeln(versionMsg);
		return 0;
	}

	normal("PowerNex's toolchain manager - https://github.com/Vild/PowerNex\n");
	VersionInfo oldVI = clean ? VersionInfo.init : getOldInfo();
	VersionInfo newVI = getNewInfo();
	// Check if fetching new info failed

		if (newVI.binutilsCommitHash is null) {
		error("Failed to retrieve latest version information for PowerNex-binutils. Check network or Git setup.\n");
		if (oldVI.binutilsCommitHash is null) { // No old version either
			return 1; // Critical failure if binutils cannot be established
		}
		warning("Proceeding with potentially outdated PowerNex-binutils information due to fetch failure.\n");

		// Use oldVI as newVI if fetching failed but an old version exists
		if (newVI.binutilsCommitHash is null && oldVI.binutilsCommitHash !is null) newVI.binutilsCommitHash = oldVI.binutilsCommitHash;
	}

	bool newBinutils = newVI.binutilsCommitHash !is null && oldVI.binutilsCommitHash != newVI.binutilsCommitHash;

	if (!clean && !newBinutils && oldVI.binutilsCommitHash !is null) {
		good("You already have the latest PowerNex-binutils!\n");
		return 0;
	}
	if (newBinutils) {
		good("BINUTILS is missing or version info is incomplete! Will download version ", newVI.binutilsVersionTag, ".\n");
	}

	char answer = question!('y', ['y', 'n'])("Do you want to continue with the download?");
	if (answer == char.init)
		return -1;
	if (answer == 'n')
		return 0;
	if (exists(toolchainFolder) || exists(toolchainSrcFolder)) {
		if (!clean) {
			answer = question!('n', ['y', 'n'])(
				"Erase toolchain install (" ~ toolchainFolder ~ ") and source (" ~ toolchainSrcFolder ~ ") folders before starting? (Will force rebuild)");
			if (answer == char.init)
				return -1;
			clean |= answer == 'y';
		}
		if (clean) {
			normal("Cleaning toolchain install directory: ", toolchainFolder, "\n");
			if (exists(toolchainFolder))
				rmdirRecurse(toolchainFolder);
			normal("Cleaning toolchain source directory: ", toolchainSrcFolder, "\n");
			if (exists(toolchainSrcFolder))
				rmdirRecurse(toolchainSrcFolder);
			// Re-ensure repos are cloned if clean was chosen
			newVI = getNewInfo(); // This will re-clone if src folder was removed
		}
	}

	mkdirRecurse(toolchainFolder ~ "/bin");
	string installPrefix = getAbsoluteInstallPrefix();

	

	if (newBinutils || clean) {
		if (newVI.binutilsCommitHash) {
			normal("Building PowerNex Binutils...\n");
			string binutilsSrcPath = toolchainSrcFolder ~ "/binutils-gdb"; // Build from the patched binutils-gdb directory
			// !!! IMPORTANT: Replace this with the actual build command for powernex-binutils !!!
			string buildCmdBinutils = "cd \"" ~ binutilsSrcPath ~ "\" && ./configure --prefix=\"" ~ installPrefix ~ "\" --target=x86_64-powernex --enable-ld --enable-gas --disable-gdb --disable-nls && make -j$(nproc) MAKEINFO=/bin/true && make MAKEINFO=/bin/true install";
			warning("Executing placeholder Binutils build: ", buildCmdBinutils, "\n");
			auto buildRes = executeShell(buildCmdBinutils);
			if (buildRes.status != 0) {
				error("Failed to build PowerNex Binutils: ", buildRes.output, "\n");
				return 1;
			}
			good("PowerNex Binutils build successful.\n");
		} else {
			error("Binutils commit hash is missing, cannot build. Ensure Git clone was successful.\n");
			return 1;
		}
	}

	normal("Saving new version file...\n");
	{
		import std.json : JSONValue;

		JSONValue data = JSONValue([
			"binutilsCommitHash": JSONValue(newVI.binutilsCommitHash)
		]);

		fwrite(versionInfoFile, data.toString);
	}
	normal("Everything is now up to date :)\n");

	return 0;
}

// Not defined in phobos, or has a wrapper in core.sys.posix.string
int strcasecmp(scope const char[] s1, scope const char[] s2) @trusted pure @nogc {
	size_t len = s1.length < s2.length ? s1.length : s2.length;
	size_t idx;
	while (idx < len && s1[idx]) {
		if (s1[idx].toLower != s2[idx].toLower)
			return s1[idx] - s2[idx];
		idx++;
	}
	return 0;
}
