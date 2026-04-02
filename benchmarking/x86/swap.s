.section __TEXT,__text
.global _start

# Swap rax=17 and rbx=73 using xchg, then exit with the value now in rax.
# Result: 73 (exit code)
# Tests: xchg (register-to-register swap), verifies both registers
#        are written correctly by checking that rax holds the old rbx value.

_start:
    movq $17, %rax          # rax = 17
    movq $73, %rbx          # rbx = 73

    xchgq %rax, %rbx        # rax <-> rbx  (rax = 73, rbx = 17)

    movq %rax, %rdi         # exit status = 73 (old rbx, now in rax)

    movq $0x2000001, %rax   # syscall: exit
    syscall
