	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $20, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $3, 8(%rax)
	leaq 16(%rax), %rdi
	movq $str_0, %rsi
	movq %rax, %r12
	call strcpy
	movq %r12, %rax
	movq %rax, %rdi
	call print_value
	xorq %rax, %rax
end_main:
	popq %rbp
	ret
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	call malloc
end_my_malloc:
	popq %rbp
	ret
print_value:
	pushq %rbp
	movq %rsp, %rbp
	movq %rdi, %r8
	movq 0(%r8), %r9
	cmpq $0, %r9
	je print_none
	cmpq $1, %r9
	je print_bool
	cmpq $2, %r9
	je print_int
	cmpq $3, %r9
	je print_string
	cmpq $4, %r9
	je print_list
print_error:
	movq $error_msg, %rdi
	xorq %rax, %rax
	call printf
	movq $1, %rdi
	call exit
print_none:
	movq $none_str, %rdi
	xorq %rax, %rax
	call printf
	jmp print_newline
print_bool:
	movq 8(%r8), %r10
	cmpq $0, %r10
	je print_false
print_true:
	movq $true_str, %rdi
	xorq %rax, %rax
	call printf
	jmp print_newline
print_false:
	movq $false_str, %rdi
	xorq %rax, %rax
	call printf
	jmp print_newline
print_int:
	movq 8(%r8), %rsi
	movq $int_fmt, %rdi
	xorq %rax, %rax
	call printf
	jmp print_newline
print_string:
	leaq 16(%r8), %rsi
	movq $str_fmt, %rdi
	xorq %rax, %rax
	call printf
	jmp print_newline
print_list:
	movq $list_start, %rdi
	xorq %rax, %rax
	call printf
	movq 8(%r8), %r10
	cmpq $0, %r10
	je print_list_end
print_list_end:
	movq $list_end, %rdi
	xorq %rax, %rax
	call printf
	jmp print_newline
print_newline:
	movq $newline_str, %rdi
	xorq %rax, %rax
	call printf
end_print_value:
	popq %rbp
	ret
	.data
error_msg:
	.string "error: invalid value\n"
false_str:
	.string "False"
int_fmt:
	.string "%ld"
list_end:
	.string "]"
list_start:
	.string "["
newline_str:
	.string "\n"
none_str:
	.string "None"
str_0:
	.string "foo"
str_fmt:
	.string "%s"
true_str:
	.string "True"
