	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %r12
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	movq %rax, %rsi
	movq %r12, %rdi
	call Badd
	movq %rax, %rdi
	call print_value
	call print_newline
	xorq %rax, %rax
end_main:
	popq %rbp
	ret
my_malloc:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc
	movq %rbp, %rsp
end_my_malloc:
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
print_value:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
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
	call my_printf
	movq $1, %rdi
	call exit
print_none:
	movq $none_str, %rdi
	xorq %rax, %rax
	call my_printf
	jmp end_print
print_bool:
	movq 8(%r8), %r10
	cmpq $0, %r10
	je print_false
print_true:
	movq $true_str, %rdi
	xorq %rax, %rax
	call my_printf
	jmp end_print
print_false:
	movq $false_str, %rdi
	xorq %rax, %rax
	call my_printf
	jmp end_print
print_int:
	movq 8(%r8), %rsi
	movq $int_fmt, %rdi
	xorq %rax, %rax
	call my_printf
	jmp end_print
print_string:
	leaq 16(%r8), %rsi
	movq $str_fmt, %rdi
	xorq %rax, %rax
	call my_printf
	jmp end_print
print_list:
	movq %r8, %r14
	movq 8(%r14), %r12
	movq $0, %r13
	movq $list_start, %rdi
	xorq %rax, %rax
	call my_printf
print_list_loop:
	cmpq %r12, %r13
	je print_list_end
	cmpq $0, %r13
	je skip_comma
	movq $comma_space, %rdi
	xorq %rax, %rax
	call my_printf
skip_comma:
	leaq 16(%r14), %rsi
	movq %r13, %rcx
	imulq $8, %rcx
	addq %rcx, %rsi
	movq 0(%rsi), %rdi
	pushq %r8
	pushq %r9
	pushq %r10
	pushq %r11
	call print_value
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	incq %r13
	jmp print_list_loop
print_list_end:
	movq $list_end, %rdi
	xorq %rax, %rax
	call my_printf
	jmp end_print
print_newline:
	movq $newline_str, %rdi
	xorq %rax, %rax
	call my_printf
	ret
end_print:
end_print_value:
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
Badd:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	subq $64, %rsp

inline_Badd:
  movq 0(%rdi), %r9
  movq 0(%rsi), %r10
  cmpq %r9, %r10
  jne add_error
  cmpq $2, %r9
  je add_int
  cmpq $3, %r9
  je add_string
  cmpq $4, %r9
  je add_list
add_error:
  movq $add_error_msg, %rdi
  xorq %rax, %rax
  call printf
  movq $1, %rdi
  call exit
add_int:
  movq 8(%rdi), %r9
  movq 8(%rsi), %r10
  addq %r9, %r10
  movq %rdi, %rax
  movq %r10, 8(%rax)
  jmp end_inline_Badd
add_string:
  movq 8(%rdi), %r8
  movq 8(%rsi), %r9
  addq %r8, %r9
  movq %r9, -24(%rbp)  # new size
  movq %rdi, -16(%rbp) # first string
  leaq 16(%rdi), %rdi
  leaq 16(%rsi), %rsi
  call strcat
  movq %rax, -32(%rbp) # new string
  movq -24(%rbp), %rdi
  call my_malloc
	movq $3, 0(%rax) # type tag
  movq -24(%rbp), %rsi
  movq %rsi, 8(%rax)
  movq %rax, %r12
  leaq 16(%rax), %rdi
  movq -32(%rbp), %rsi 
  call strcpy
  movq %r12, %rax
  jmp end_inline_Badd
add_list:
  movq %rdi, %r13
  movq %rsi, %r14
  movq 8(%r13), %r9
  movq 8(%r14), %r10
  movq %r9, %r11
  addq %r10, %r11
  movq %r11, %rcx
  imulq $8, %rcx
  addq $16, %rcx
  movq %rcx, %rdi
  call my_malloc
  movq %rax, %r12
  movq $4, 0(%r12)
  movq %r11, 8(%r12)
  movq %r9, %rcx
  imulq $8, %rcx
  leaq 16(%r13), %rsi
  leaq 16(%r12), %rdi
  movq %rcx, %rdx
  call memcpy
  movq %r10, %rcx
  imulq $8, %rcx
  leaq 16(%r14), %rsi
  leaq 16(%r12,%r9,8), %rdi
  movq %rcx, %rdx
  call memcpy
  movq %r12, %rax
  jmp end_inline_Badd
end_inline_Badd:
        	addq $64, %rsp
end_Badd:
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
  
my_printf:
  movq %rsp, %rbp
  andq $-16, %rsp 
  call printf
  movq %rbp, %rsp
  ret
	.data
add_error_msg:
	.string "error: invalid type for '+' operand"
comma_space:
	.string ", "
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
str_fmt:
	.string "%s"
true_str:
	.string "True"
