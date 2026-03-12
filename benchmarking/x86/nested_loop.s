.section __TEXT,__text
.global _start

# Nested loop: outer 1..5, inner 1..4, counting total iterations.
# Result: 20 (exit code)
# Tests: nested cmp + jle, inc, mov, multiple labels and branch targets

_start:
    movq $0, %rax           # total iteration count = 0
    movq $1, %rbx           # outer counter = 1

outer_loop:
    cmpq $5, %rbx           # outer <= 5?
    jg outer_done

    movq $1, %rcx           # inner counter = 1

inner_loop:
    cmpq $4, %rcx           # inner <= 4?
    jg inner_done

    incq %rax               # total++
    incq %rcx               # inner++
    jmp inner_loop

inner_done:
    incq %rbx               # outer++
    jmp outer_loop

outer_done:
    # rax = 20 (5 * 4)
    movq %rax, %rbx         # save result

    # Store result byte
    movb %bl, result(%rip)

    # Write(1, result, 1)
    movq $0x2000004, %rax   # syscall: write
    movq $1, %rdi           # fd: stdout
    leaq result(%rip), %rsi # buffer
    movq $1, %rdx           # length
    syscall

    # Exit with iteration count as status
    movq $0x2000001, %rax   # syscall: exit
    movq %rbx, %rdi         # status = 20
    syscall

.section __DATA,__data
result:
    .byte 0