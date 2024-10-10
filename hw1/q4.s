.section .data
    fmt: .string "%d\n"
    x: .long 0
    y: .long 0

.section .text 
.global main
.type main, @function

main:
    pushq %rbp
    movq %rsp, %rbp

    movl $2, x(%rip) 
    movl x(%rip), %eax
    imul %eax, %eax
    movl %eax, y(%rip)

    movl x(%rip), %ecx
    addl %ecx, %eax
    movl %eax, %edi
    call print_num

    xorq %rax, %rax
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
