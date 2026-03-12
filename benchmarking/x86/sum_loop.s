.section __TEXT,__text
.global _start

# Sum integers 1 through 8 using a loop.
# Result: 36 (exit code)
# Tests: mov, add, cmp, jle, inc, jmp (loop pattern)

_start:
    movq $0, %rax           # accumulator = 0
    movq $1, %rcx           # counter = 1

loop:
    addq %rcx, %rax         # accumulator += counter
    incq %rcx               # counter++
    cmpq $8, %rcx           # compare counter to 8
    jle loop                # if counter <= 8, continue

    # Save result in rbx (preserved across syscalls)
    movq %rax, %rbx

    # Convert low byte to ASCII and store
    addb $48, %al           # add '0' — will produce '$' (36+48=84 is 'T'... 
                            # actually just store raw for verification)
    movb %bl, result(%rip)

    # Write(1, result, 1)
    movq $0x2000004, %rax   # syscall: write
    movq $1, %rdi           # fd: stdout
    leaq result(%rip), %rsi # buffer
    movq $1, %rdx           # length
    syscall

    # Exit with result as status code
    movq $0x2000001, %rax   # syscall: exit
    movq %rbx, %rdi         # status = computed sum
    syscall

.section __DATA,__data
result:
    .byte 0