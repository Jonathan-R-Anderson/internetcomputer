; clone_helper.S

global cloneHelperFork

section .text
bits 64

cloneHelperFork:
    ; Stack layout:
    ; [top] syscallRegisters
    ;        ├── r15
    ;        ├── ...
    ;        ├── rip
    ;        ├── cs
    ;        ├── rflags
    ;        ├── rsp
    ;        └── ss
    ; So we'll pop in reverse order for iretq.

    ; Pop general-purpose registers manually (if needed)
    ; But for user return, we care most about the final iretq set

    ; Align stack and perform iretq with user-mode frame
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rsi
    pop rdi
    pop rbp
    pop rdx
    pop rcx
    pop rbx
    pop rax

    ; Now do iretq-compatible pops:
    pop r11        ; user rip
    pop r10        ; user rsp
    pop r9         ; user rflags
    pop r8         ; user cs
    pop rdx        ; user ss

    ; Prepare for iretq
    push rdx       ; ss
    push r10       ; rsp
    push r9        ; rflags
    push r8        ; cs
    push r11       ; rip

    iretq
