.section __TEXT,__text
.global _start

# Compute 99 / 9 = 11 using signed integer division (cdq + idiv).
# Result: 11 (exit code)
# Tests: cdq (sign-extend EAX into EDX:EAX), idiv (signed division),
#        verifies quotient in rax, remainder in rdx
#
# Algorithm:
#   eax = 99   (dividend)
#   cdq        (sign-extend into edx — makes edx = 0 here, since 99 > 0)
#   ecx = 9    (divisor)
#   idiv ecx   (eax = quotient = 11, edx = remainder = 0)

_start:
    movq $99, %rax          # dividend = 99
    cqo                     # sign-extend rax into rdx:rax (cqo = 64-bit cdq)
    movq $9, %rcx           # divisor = 9
    idivq %rcx              # rax = 99 / 9 = 11, rdx = 99 % 9 = 0

    # rax = 11
    movq %rax, %rdi         # exit status = 11

    movq $0x2000001, %rax   # syscall: exit
    syscall
