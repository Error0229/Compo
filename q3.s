.section .data
    true_s: .string "true"
    false_s: .string "false"
    fmt: .string "%d\n"

.section .text 
.global main
.type main, @function

main:

    pushq %rbp
    movq %rsp, %rbp

    # p1: true && false
    movl $1, %eax
    andl $0, %eax
    movl %eax, %edi
    call print_bool 

    # p2: if 3 <> 4 then 10 * 2 else 14 
    movl $3, %eax
if_0:
    cmpl $4, %eax
    jne if_0_f

if_0_t:
    movl $14, %eax
    jmp end_if_0

if_0_f: 
    movl $10, %eax
    movl $2, %ecx 
    mull %ecx

end_if_0:
    movl %eax, %edi
    call print_num

    # p3: 2 = 3 || 4 <= 2 * 3

    movl $2, %eax
if_1:
    cmpl $3, %eax
    je if_1_t

    movl $2, %eax
    movl $3, %ecx
    mull %ecx
    cmpl $4, %eax
    jle if_1_t

if_t_f:
    movl $0, %edi    

if_1_t:
    movl $1, %edi

end_if_1:
    call print_bool

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

print_bool:
    pushq %rbp
    movq %rsp, %rbp
    testl %edi, %edi
    jz print_false
    leaq true_s(%rip), %rdi
    jmp excute_print_bool

print_false:
    leaq false_s(%rip), %rdi

excute_print_bool:
    call puts
    xorq %rax, %rax
    popq %rbp
    ret
