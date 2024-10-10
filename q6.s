.section .data
    fmt: .string "sqrt(%2d) = %2d\n"

.section .text 
.global main
.type main, @function

isqrt: 
    pushq %rbp
    movq %rsp, %rbp

	movl $0, %eax # c
	movl $1, %ecx # s
	movl $1, %edx # tmp = 1 -> 3 -> 5 -> 7 ... 
while_0:
	cmpl %ecx, %edi # s <= n? 
	jl  end_while_0 # if n < s
	incl %eax
	addl $2, %edx # tmp += 2
	addl %edx, %ecx # s += tmp
	jmp while_0

end_while_0:
    popq %rbp
	ret


main:
    pushq %rbp # after this, %rbp % 16 = 8
    movq %rsp, %rbp

	subq $16, %rsp # 8 bytes for n also align to 8 bytes for printf

	movl $0, -4(%rbp) # n = 0
for_0:
	cmpl $20, -4(%rbp)
	jg end_for_0
	movl -4(%rbp), %edi 
	call isqrt

	movl -4(%rbp), %esi
	movl %eax, %edx
    leaq fmt(%rip), %rdi 
    call printf
	incl -4(%rbp)
	jmp for_0

end_for_0:
	addq $16, %rsp
    xorq %rax, %rax
    popq %rbp
    ret
