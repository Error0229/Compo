	.file	"test.c"
# GNU C17 (Ubuntu 13.1.0-8ubuntu1~20.04.2) version 13.1.0 (x86_64-linux-gnu)
#	compiled by GNU C version 13.1.0, GMP version 6.2.0, MPFR version 4.0.2, MPC version 1.1.0, isl version isl-0.22.1-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -mtune=generic -march=x86-64 -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB6:
	.cfi_startproc
	endbr64	
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
# test.c:4:   int *x = (int *)malloc(sizeof(int));
	movl	$4, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -8(%rbp)	# tmp85, x
# test.c:5:   *x = 10;
	movq	-8(%rbp), %rax	# x, tmp86
	movl	$10, (%rax)	#, *x_4
# test.c:6:   printf("%d\n", *x);
	movq	-8(%rbp), %rax	# x, tmp87
	movl	(%rax), %eax	# *x_4, _1
	movl	%eax, %esi	# _1,
	leaq	.LC0(%rip), %rax	#, tmp88
	movq	%rax, %rdi	# tmp88,
	movl	$0, %eax	#,
	call	printf@PLT	#
	movl	$0, %eax	#, _7
# test.c:7: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE6:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 13.1.0-8ubuntu1~20.04.2) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
