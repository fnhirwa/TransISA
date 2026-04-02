.section __TEXT,__text
.global _start

# Compute abs(-37) = 37 using the branchless cdq idiom.
# Result: 37 (exit code)
# Tests: cdq (sign-extend, producing all-1s mask for negative input),
#        xor (conditional flip), sub (conditional increment) — the
#        standard branchless absolute value pattern used in real compilers.
#
# Branchless abs pattern:
#   cdq           -> edx = 0xFFFFFFFF if eax < 0, else 0x00000000
#   xor eax, edx  -> flips all bits of eax if negative (one's complement)
#   sub eax, edx  -> adds 1 if negative (two's complement, completing negation)
# Net effect: eax = (eax >= 0) ? eax : -eax

_start:
    movq $-37, %rax         # eax = -37

    cdq                     # edx = sign mask (0xFFFFFFFF since rax < 0)
    xorq %rdx, %rax         # rax ^= rdx  (flip bits if negative)
    subq %rdx, %rax         # rax -= rdx  (add 1 if negative, via sub -1)

    # rax = 37
    movq %rax, %rdi         # exit status = 37

    movq $0x2000001, %rax   # syscall: exit
    syscall
