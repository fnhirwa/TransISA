.section __TEXT,__text
.global _start

# Compute Fibonacci(10) = 55 iteratively.
# Tests: mov, add, cmp, jl, register-to-register data flow, loop-carried dependency

_start:
    movq $0, %rax           # fib(0) = 0 (prev)
    movq $1, %rbx           # fib(1) = 1 (curr)
    movq $2, %rcx           # counter = 2
    movq $10, %rdx          # target N = 10

fib_loop:
    cmpq %rdx, %rcx         # compare counter to N
    jg fib_done             # if counter > N, done

    # temp = curr + prev; prev = curr; curr = temp
    movq %rbx, %rsi         # rsi = curr (temp save)
    addq %rax, %rbx         # curr = curr + prev
    movq %rsi, %rax         # prev = old curr

    incq %rcx               # counter++
    jmp fib_loop

fib_done:
    # rbx = fib(10) = 55
    # Save result
    movq %rbx, %rdi         # exit status = result

    # Store low byte for write
    movb %bl, result(%rip)

    # Write(1, result, 1)
    pushq %rdi              # save exit code
    movq $0x2000004, %rax   # syscall: write
    movq $1, %rdi           # fd: stdout
    leaq result(%rip), %rsi # buffer
    movq $1, %rdx           # length
    syscall
    popq %rdi               # restore exit code

    # Exit with fib(10) as status
    movq $0x2000001, %rax   # syscall: exit
    # rdi already set
    syscall

.section __DATA,__data
result:
    .byte 0