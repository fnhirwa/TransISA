.section __TEXT,__text
.global _start

# Compute NOT(0xFFFFFFF0) & 0xFF = 0x0F = 15.
# Result: 15 (exit code)
# Tests: not (bitwise complement), and (mask to extract low byte)
#
# Algorithm:
#   rax = 0xFFFFFFF0
#   not rax          -> rax = 0x000000000000000F
#   and rax, 0xFF    -> rax = 0x0F = 15

_start:
    movq $0xFFFFFFF0, %rax  # rax = 0xFFFFFFF0
    notq %rax               # rax = ~rax = 0x000000000000000F
    andq $0xFF, %rax        # mask to low byte -> rax = 15

    movq %rax, %rdi         # exit status = 15

    movq $0x2000001, %rax   # syscall: exit
    syscall
