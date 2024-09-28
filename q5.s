
.section .data
    fmt: .string "%d\n"

.section .text 
.global main
.type main, @function

main:
    pushq %rbp
    movq %rsp, %rbp

    # print (let x = 3 in x * x)
    subq $32, %rsp
    movl $3, 0(%rbp)
    movl 0(%rbp), %edi
    imul %edi, %edi
    call print_num

    # print (let x = 3 in (let y = x + x in x * y) + (let z = x + 3 in z / z))
    movl $3, 0(%rbp)
    movl 0(%rbp), %r8d
    addl %r8d, %r8d
    movl %r8d, 4(%rbp)
    movl 4(%rbp), %r8d
    imul 0(%rbp), %r8d

    addl $3, %eax
    movl %eax, 8(%rsp)
    movl 8(%rsp), %eax
    idivl %eax
    addl %r8d, %eax
    movl %eax, %edi
    call print_num 

    xorq %rax, %rax
    addq $32, %rsp
    popq %rbp
    ret

print_num:
    pushq %rbp
    movq %rsp, %rbp

    movl %edi, %esi
    leaq fmt(%rip), %rdi
    call printf

    xorq %rax, %rax
    popq %rbp

    ret
