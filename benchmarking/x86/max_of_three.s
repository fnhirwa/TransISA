.section __TEXT,__text
.global _start

# Find the maximum of three values: 17, 42, 29.
# Result: 42 (exit code)
# Tests: mov, cmp, jge (multiple branch paths), conditional control flow

_start:
    movq $17, %rax          # a = 17
    movq $42, %rbx          # b = 42
    movq $29, %rcx          # c = 29

    # max = a
    movq %rax, %rdx         # rdx = current max

    # if b > max: max = b
    cmpq %rdx, %rbx
    jle skip_b
    movq %rbx, %rdx
skip_b:

    # if c > max: max = c
    cmpq %rdx, %rcx
    jle skip_c
    movq %rcx, %rdx
skip_c:

    # rdx = max = 42
    # Store result byte
    movb %dl, result(%rip)

    # Write(1, result, 1)
    pushq %rdx              # save max
    movq $0x2000004, %rax   # syscall: write
    movq $1, %rdi           # fd: stdout
    leaq result(%rip), %rsi # buffer
    movq $1, %rdx           # length
    syscall
    popq %rdi               # restore max into rdi

    # Exit with max as status
    movq $0x2000001, %rax   # syscall: exit
    # rdi = max
    syscall

.section __DATA,__data
result:
    .byte 0