.section __TEXT,__text,regular,pure_instructions
.globl _main
_main:
    mov     $0x2000004, %rax       # syscall: write
    mov     $1, %rdi               # fd: stdout
    lea     msg(%rip), %rsi        # buf: message address
    mov     $13, %rdx              # count
    syscall

    mov     $0x2000001, %rax       # syscall: exit
    xor     %rdi, %rdi             # status 0
    syscall

.section __TEXT,__cstring
msg:
    .asciz "Hello World!\n"
