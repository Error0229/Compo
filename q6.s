	.file	"q6.c"
# GNU C17 (Ubuntu 13.1.0-8ubuntu1~20.04.2) version 13.1.0 (x86_64-linux-gnu)
#	compiled by GNU C version 13.1.0, GMP version 6.2.0, MPFR version 4.0.2, MPC version 1.1.0, isl version isl-0.22.1-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -mtune=generic -march=x86-64 -O3 -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.p2align 4
	.globl	isqrt
	.type	isqrt, @function
isqrt:
.LFB23:
	.cfi_startproc
	endbr64	
# q6.c:4:   while (s <= n) {
	testl	%edi, %edi	# n
	jle	.L4	#,
	movl	$3, %edx	#, ivtmp.9
# q6.c:3:   int c = 0, s = 1;
	movl	$1, %eax	#, s
# q6.c:3:   int c = 0, s = 1;
	xorl	%ecx, %ecx	# <retval>
	.p2align 4,,10
	.p2align 3
.L3:
# q6.c:6:     s += 2 * c + 1;
	addl	%edx, %eax	# ivtmp.9, s
# q6.c:5:     c++;
	addl	$1, %ecx	#, <retval>
# q6.c:4:   while (s <= n) {
	addl	$2, %edx	#, ivtmp.9
	cmpl	%eax, %edi	# s, n
	jge	.L3	#,
# q6.c:9: }
	movl	%ecx, %eax	# <retval>,
	ret	
	.p2align 4,,10
	.p2align 3
.L4:
# q6.c:3:   int c = 0, s = 1;
	xorl	%ecx, %ecx	# <retval>
# q6.c:9: }
	movl	%ecx, %eax	# <retval>,
	ret	
	.cfi_endproc
.LFE23:
	.size	isqrt, .-isqrt
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"sqrt(%2d) = %2d\n"
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB24:
	.cfi_startproc
	endbr64	
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
# /usr/include/x86_64-linux-gnu/bits/stdio2.h:107:   return __printf_chk (__USE_FORTIFY_LEVEL - 1, __fmt, __va_arg_pack ());
	leaq	.LC0(%rip), %rbp	#, tmp91
	xorl	%ecx, %ecx	#
	xorl	%edx, %edx	#
# q6.c:10: int main() {
	pushq	%rbx	#
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
# /usr/include/x86_64-linux-gnu/bits/stdio2.h:107:   return __printf_chk (__USE_FORTIFY_LEVEL - 1, __fmt, __va_arg_pack ());
	movq	%rbp, %rsi	# tmp91,
	movl	$1, %edi	#,
	xorl	%eax, %eax	#
	movl	$1, %ebx	#, n
# q6.c:10: int main() {
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 32
# /usr/include/x86_64-linux-gnu/bits/stdio2.h:107:   return __printf_chk (__USE_FORTIFY_LEVEL - 1, __fmt, __va_arg_pack ());
	call	__printf_chk@PLT	#
	.p2align 4,,10
	.p2align 3
.L9:
	movl	$3, %edx	#, ivtmp.20
# q6.c:3:   int c = 0, s = 1;
	movl	$1, %eax	#, s
# q6.c:3:   int c = 0, s = 1;
	xorl	%ecx, %ecx	# c
	.p2align 4,,10
	.p2align 3
.L8:
# q6.c:6:     s += 2 * c + 1;
	addl	%edx, %eax	# ivtmp.20, s
# q6.c:5:     c++;
	addl	$1, %ecx	#, c
# q6.c:4:   while (s <= n) {
	addl	$2, %edx	#, ivtmp.20
	cmpl	%eax, %ebx	# s, n
	jge	.L8	#,
# /usr/include/x86_64-linux-gnu/bits/stdio2.h:107:   return __printf_chk (__USE_FORTIFY_LEVEL - 1, __fmt, __va_arg_pack ());
	movl	%ebx, %edx	# n,
	movq	%rbp, %rsi	# tmp91,
	movl	$1, %edi	#,
	xorl	%eax, %eax	#
	call	__printf_chk@PLT	#
# q6.c:12:   for (n = 0; n <= 20; n++)
	addl	$1, %ebx	#, n
# q6.c:12:   for (n = 0; n <= 20; n++)
	cmpl	$21, %ebx	#, n
	jne	.L9	#,
# q6.c:15: }
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 24
	xorl	%eax, %eax	#
	popq	%rbx	#
	.cfi_def_cfa_offset 16
	popq	%rbp	#
	.cfi_def_cfa_offset 8
	ret	
	.cfi_endproc
.LFE24:
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
