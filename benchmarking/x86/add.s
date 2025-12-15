.section __TEXT,__text
.global _start

_start:
    # Add 5 + 10
    movq $5, %rax
    movq $10, %rbx
    addq %rbx, %rax

    # Convert result (assume < 100) to ASCII character (single digit only!)
    addb $'0', %al        # Only converts if result < 10, otherwise it’s wrong

    # Store ASCII in buffer
    movb %al, result(%rip)

    # Write(1, result, 1)
    movq $0x2000004, %rax   # syscall number for write
    movq $1, %rdi           # stdout
    leaq result(%rip), %rsi # buffer
    movq $1, %rdx           # length
    syscall

    # Exit(0)
    movq $0x2000001, %rax   # syscall number for exit
    xorq %rdi, %rdi         # status = 0
    syscall

.section __DATA,__data
.balign 1
result:
    .byte 0