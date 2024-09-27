.section .data
    fmt: .string "%d\n"


.section .text 
.global main 
.type main, @function
main: 

    # 4 + 6 
    pushq %rbp
    movq %rsp, %rbp

    movl $4, %eax
    addl $6, %eax

    leaq fmt(%rip), %rdi
    movl %eax, %esi
    call printf

    # 21 * 2
    movl $21, %eax
    movl $2, %ecx
    mull %ecx

    leaq fmt(%rip), %rdi
    movl %eax, %esi
    call printf

    # 4 + 7 / 2
    movl $7, %eax
    movl $2, %ecx
    divl %ecx
    
    addl $4, %eax

    leaq fmt(%rip), %rdi
    movl %eax, %esi
    call printf

    # 3 - 6 * (10 / 5)

    movl $10, %eax
    movl $5, %ecx
    divl %ecx
    movl $6, %ecx
    mull %ecx
    movl %eax, %ebx
    movl $3, %eax
    subl %ebx, %eax

    leaq fmt(%rip), %rdi
    movl %eax, %esi
    call printf
    

    xorq %rax, %rax
    popq %rbp

    ret
