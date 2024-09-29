 .section .data 
	fmt: .string "n = %d\n"
.section	.text
.globl	main
.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$42, %esi
	leaq	fmt(%rip), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	printf
	xorq 	%rax, %rax
	popq	%rbp
	ret
