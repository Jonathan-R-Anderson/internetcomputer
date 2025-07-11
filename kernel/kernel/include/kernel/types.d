// kernel/types.d

module kernel.types;

// VGA Colors (inspired by OSDev wiki)
enum VGAColor : ubyte {
    BLACK = 0,
    BLUE = 1,
    GREEN = 2,
    CYAN = 3,
    RED = 4,
    MAGENTA = 5,
    BROWN = 6,
    LIGHT_GREY = 7,
    DARK_GREY = 8,
    LIGHT_BLUE = 9,
    LIGHT_GREEN = 10,
    LIGHT_CYAN = 11,
    LIGHT_RED = 12, // Often Pink
    LIGHT_MAGENTA = 13, // Often Pink/Purple
    YELLOW = 14,      // Changed from LIGHT_BROWN
    WHITE = 15,
}

enum ErrorCode : ubyte {
    NONE = 0,
    GDT_LOAD_FAILURE = 0x01,
    IDT_INIT_FAILURE = 0x02,
    ISR_INSTALL_FAILURE = 0x03,
    UNKNOWN_FAILURE = 0xFF,
}

// Structure to hold general-purpose register states passed from assembly interrupt stubs.
// This order must match the order of pushes in isr_common_stub in interrupts_asm.s
// when %rsp is passed to the D handler.
// The D handler will receive a pointer to the first field (r15_val in this case).
struct Registers {
    // Registers are pushed starting with %rax and ending with %r15 in
    // interrupts_asm.s, so this struct is ordered accordingly when %rsp is
    // passed to D.
    ulong r15_val;
    ulong r14_val;
    ulong r13_val;
    ulong r12_val;
    ulong r11_val;
    ulong r10_val;
    ulong r9_val;
    ulong r8_val;
    ulong rbp_val;
    ulong rdi_val; // Original RDI saved
    ulong rsi_val; // Original RSI saved
    ulong rdx_val;
    ulong rcx_val;
    ulong rbx_val;
    ulong rax_val;
    // Note: int_no, err_code, and CPU-pushed state (rip, cs, rflags etc.)
    // are handled as separate parameters to interrupt_handler_d or accessed via offsets.
    // For this iteration, they will be separate parameters.
}

// Basic C library function implementations needed by the compiler/runtime
extern (C) void* memset(void* ptr, int value, size_t num) {
    ubyte* p = cast(ubyte*)ptr;
    ubyte val = cast(ubyte)value;
    for (size_t i = 0; i < num; i++) {
        p[i] = val;
    }
    return ptr;
}

// Minimal implementation of memcpy for -betterC builds.
// This avoids relying on an external C runtime while
// providing the basic functionality needed by parts of
// the kernel such as realloc.
extern (C) void* memcpy(void* dest, const void* src, size_t num) {
    auto d = cast(ubyte*)dest;
    auto s = cast(const ubyte*)src;
    for (size_t i = 0; i < num; i++) {
        d[i] = s[i];
    }
    return dest;
}

// Compare two memory regions
extern(C) int memcmp(const void* lhs, const void* rhs, size_t num)
{
    auto a = cast(const ubyte*)lhs;
    auto b = cast(const ubyte*)rhs;
    for (size_t i = 0; i < num; ++i)
    {
        if (a[i] != b[i])
            return a[i] < b[i] ? -1 : 1;
    }
    return 0;
}

// Compute length of a C string
extern(C) size_t strlen(const(char)* str) {
    size_t len = 0;
    while (str[len] != 0) {
        ++len;
    }
    return len;
}

// Find first occurrence of character in C string
extern(C) char* strchr(const char* str, int c) {
    size_t i = 0;
    auto ch = cast(char)c;
    while (str[i] != 0) {
        if (str[i] == ch)
            return cast(char*)str + i;
        ++i;
    }
    return null;
}
