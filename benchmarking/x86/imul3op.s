.section __TEXT,__text
.global _start

# Compute 6 * 8 = 48 using the 3-operand imul form: imul dst, src, imm.
# Result: 48 (exit code)
# Tests: imul 3-op (imul dst, src, imm — distinct from the 2-op dst*=src form),
#        verifies that dst = src * imm and src is not modified.

_start:
    movq $6, %rcx           # rcx = 6 (source operand)

    imulq $8, %rcx, %rax    # rax = rcx * 8 = 48  (3-op: dst, src, imm)

    movq %rax, %rdi         # exit status = 48

    movq $0x2000001, %rax   # syscall: exit
    syscall
