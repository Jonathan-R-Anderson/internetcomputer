// kernel/interrupts.d

module kernel.interrupts;

import kernel.types : Registers, ErrorCode;
import kernel.io : outb;
import kernel.terminal : terminal_writestring, terminal_write_hex, terminal_putchar; // scancode_to_char is used in kernel.keyboard
import kernel.process_manager : get_current_pid, g_processes;

__gshared ulong timer_ticks = 0; // simple tick counter for IRQ0
import kernel.panic : kernel_panic;

public: // Export interrupt_handler_d

// PIC Constants
enum PIC1_COMMAND = 0x20;
enum PIC1_DATA = 0x21;
enum PIC2_COMMAND = 0xA0;
enum PIC2_DATA = 0xA1;
enum PIC_EOI = 0x20; // End Of Interrupt command code

// The assembly stub will pass:
// %rdi: pointer to saved GPRs (Registers* regs_ptr)
// %rsi: interrupt number (ulong int_no)
// %rdx: error code (ulong err_code)
extern (C) void interrupt_handler_d(Registers* regs_ptr, ulong int_no, ulong err_code_val) {
    // Send EOI (End Of Interrupt) to PICs
    // IRQs are from 32 to 47
    if (int_no >= 32 && int_no <= 47) {
        if (int_no >= 40) { // IRQs 8-15 are on slave PIC (remapped to 40-47)
            outb(PIC2_COMMAND, PIC_EOI); // Slave PIC
        }
        outb(PIC1_COMMAND, PIC_EOI); // Master PIC

        if (int_no == 0x20) {
            // Timer interrupt - increment tick counter and check alarms
            timer_ticks++;
            auto pid = get_current_pid();
            if(pid != size_t.max) {
                auto proc = &g_processes[pid];
                if(proc.alarm_tick != 0 && timer_ticks >= proc.alarm_tick) {
                    proc.alarm_tick = 0;
                    if(proc.note_handler !is null)
                        proc.note_handler("alarm");
                }
            }
            return;
        }
    }

    // --- Logging the interrupt ---
    // These debug prints generated excessive output during normal operation and
    // could overwhelm the virtual terminal.  They were useful when initially
    // bringing up the interrupt handling code but are now disabled.  The logic
    // remains in comments so it can be quickly re-enabled while debugging.
    /*
    terminal_writestring("Interrupt: ");
    terminal_write_hex(int_no);

    bool has_cpu_error_code = (int_no == 8 || (int_no >= 10 && int_no <= 14) ||
                               int_no == 17 || int_no == 21 || int_no == 29 ||
                               int_no == 30);
    if (has_cpu_error_code) {
        terminal_writestring(", Error Code: ");
        terminal_write_hex(err_code_val);
    }
    terminal_putchar('\n');
    */

    // --- Handling specific CPU exceptions ---
    // Note: Interrupt 33 (Keyboard) is now handled by keyboard_handler_asm.s
    // and kernel.keyboard.keyboard_interrupt_handler, so it's not processed here.
    if (int_no < 32 && int_no != 1 && int_no != 3 && int_no != 33) { // CPU Exception (excluding debug/breakpoint and keyboard)
        if (int_no == 13) { // GPF
            terminal_writestring("General Protection Fault! Selector: ");
            terminal_write_hex(err_code_val); // Error code for GPF is selector
            terminal_putchar('\n');
        } else if (int_no == 14) { // Page Fault
            terminal_writestring("Page Fault! Details: ");
            terminal_write_hex(err_code_val); // Error code for Page Fault has details
            ulong fault_addr; // Changed from uint to ulong for 64-bit CR2
            asm { "mov %%cr2, %0" : "=r"(fault_addr); }
            terminal_writestring(" Addr: ");
            terminal_write_hex(fault_addr); // Ensure this function can handle ulong
            terminal_putchar('\n');
        }
        if (int_no != 2) { // Don't panic on NMI for now, just log it.
            kernel_panic("CPU Exception Encountered.", cast(ErrorCode)int_no);
        }
    }
}

extern(C) void default_isr_handler() {
    // Log the spurious or unexpected interrupt but keep running
    terminal_writestring("Unhandled interrupt triggered.\n");
}
