.section __TEXT,__text
.global _start

# Compute 5! = 120 iteratively using imul.
# Result: 120 (exit code)
# Tests: imul (2-op form in a counted loop), dec, jnz, register-to-register
#
# Algorithm:
#   rax = 1 (accumulator)
#   rcx = 5 (counter)
# loop:
#   rax = rax * rcx
#   rcx--
#   if rcx != 0 goto loop

_start:
    movq $1, %rax           # accumulator = 1
    movq $5, %rcx           # counter = 5

fact_loop:
    imulq %rcx, %rax        # rax *= rcx  (imul 2-op: dst = dst * src)
    decq %rcx               # counter--
    jnz fact_loop           # repeat while counter != 0

    # rax = 120 = 5!
    movq %rax, %rdi         # exit status = 120

    movq $0x2000001, %rax   # syscall: exit
    syscall
