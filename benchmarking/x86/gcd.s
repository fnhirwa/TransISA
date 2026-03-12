.section __TEXT,__text
.global _start

# Compute GCD(48, 18) = 6 using the Euclidean algorithm (subtraction variant).
# Result: 6 (exit code)
# Tests: cmp, je, jl, sub, jmp (data-dependent branching, classic numeric kernel)

_start:
    movq $48, %rax          # a = 48
    movq $18, %rbx          # b = 18

gcd_loop:
    cmpq %rbx, %rax         # compare a and b
    je gcd_done             # if a == b, done

    cmpq %rbx, %rax         # a < b?
    jl b_greater

    # a > b: a = a - b
    subq %rbx, %rax
    jmp gcd_loop

b_greater:
    # b > a: b = b - a
    subq %rax, %rbx
    jmp gcd_loop

gcd_done:
    # rax = gcd = 6
    movq %rax, %rbx         # save result

    # Convert to ASCII digit and store
    addb $48, %al           # '0' + 6 = '6'
    movb %al, result(%rip)

    # Write(1, result, 1)
    movq $0x2000004, %rax   # syscall: write
    movq $1, %rdi           # fd: stdout
    leaq result(%rip), %rsi # buffer
    movq $1, %rdx           # length
    syscall

    # Exit with gcd as status
    movq $0x2000001, %rax   # syscall: exit
    movq %rbx, %rdi         # status = 6
    syscall

.section __DATA,__data
result:
    .byte 0