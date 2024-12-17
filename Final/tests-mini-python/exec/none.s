	.text
	.globl	main
main:
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	addq $-8, %rsp
	pushq %r8
	pushq %r9
	pushq %r10
	pushq %r11
	call foo
	addq $0, %rsp
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	movq %rax, %rdi
	call print_value
	call print_newline
	pushq %r8
	pushq %r9
	pushq %r10
	pushq %r11
	call foo
	addq $0, %rsp
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	pushq %rax
	pushq %r8
	pushq %r9
	pushq %r10
	pushq %r11
	call foo
	addq $0, %rsp
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	popq %rdi
	movq %rax, %rsi
	call Beq
	movq %rax, %rdi
	call print_value
	call print_newline
	pushq %r8
	pushq %r9
	pushq %r10
	pushq %r11
	call foo
	addq $0, %rsp
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	pushq %rax
	pushq %r8
	pushq %r9
	pushq %r10
	pushq %r11
	call foo
	addq $0, %rsp
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	popq %rdi
	movq %rax, %rsi
	call Bneq
	movq %rax, %rdi
	call print_value
	call print_newline
	xorq %rax, %rax
end_main:
	subq $-8, %rsp
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbp
	ret
foo:
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	addq $-16, %rsp
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, -16(%rbp)
	pushq %rdi
	movq $16, %rdi
	call my_malloc
	movq $0, 0(%rax)
	popq %rdi
end_foo:
	subq $-16, %rsp
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbp
	ret

my_memcpy:
  pushq %rbp
  movq %rsp, %rbp
  andq $-16, %rsp 
  call memcpy
  movq %rbp, %rsp
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
	pushq %rdi
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc
	movq %rbp, %rsp
	popq %rbp
	popq %rdx
	movq %rax, %rdi
	xorq %rsi, %rsi
	call memset
end_my_malloc:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
len:
	pushq %rbp
	pushq %r12
	movq %rsp, %rbp
	movq 24(%rbp), %r12
	movq 0(%r12), %rdi
	cmpq $4, %rdi
	jne fail_func_call
	movq 8(%r12), %r12
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq %r12, 8(%rax)
	popq %r12
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
	jmp fail_add
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
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
fail_for:
	movq $for_error_msg, %rdi
	jmp print_error
fail_mul:
	movq $mul_error_msg, %rdi
	jmp print_error
fail_mod:
	movq $mod_error_msg, %rdi
	jmp print_error
fail_add:
	movq $add_error_msg, %rdi
	jmp print_error
fail_div:
	movq $div_error_msg, %rdi
	jmp print_error
fail_sub:
	movq $sub_error_msg, %rdi
	jmp print_error
fail_func_call:
	movq $func_error_msg, %rdi
	jmp print_error
fail_get:
	movq $get_error_msg, %rdi
	jmp print_error
fail_index_must_int:
	movq $bad_index_error_msg, %rdi
	jmp print_error
fail_index_out_of_range:
	movq $out_of_range_error_msg, %rdi
	jmp print_error
fail_neg:
	movq $fail_neg_error_msg, %rdi
	jmp print_error
print_error:
	xorq %rax, %rax
	call my_printf
	movq $1, %rdi
	call exit
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
  jne fail_add
  cmpq $2, %r9
  je add_int
  cmpq $3, %r9
  je add_string
  cmpq $4, %r9
  je add_list
  jmp fail_add
  add_int:
	movq 8(%rdi), %r9
	movq 8(%rsi), %r10
	addq %r9, %r10
	pushq %r10
	movq $16, %rdi
	call my_malloc
	popq %r10
	movq $2, 0(%rax)
	movq %r10, 8(%rax)
	jmp end_inline_Badd

add_string:
  movq 8(%rdi), %r8
  movq 8(%rsi), %r9
  addq %r8, %r9
  movq %r9, -24(%rbp)  # new size
  movq %rdi, -16(%rbp) # first string
  pushq %rdi
  pushq %rsi
  movq -24(%rbp), %rdi
  addq $17, %rdi
  pushq %r9
  call my_malloc
  popq %r9
  popq %rsi
  popq %rdi
	movq $3, 0(%rax) # type tag
  movq %r9, 8(%rax)  
  movq %rax, %r12 

  pushq %rsi 
  pushq %rdi 

  leaq 16(%rdi), %rsi
  leaq 16(%r12), %rdi  # new string
  call strcat

  popq %rdi
  popq %rsi

  leaq 16(%r12), %rdi  
  leaq 16(%rsi), %rsi

  call strcat

  movq %r12, %rax

  jmp end_inline_Badd 
  add_list:
	movq %rdi, %r13
	movq %rsi, %r14
	movq 8(%r13), %r9
	movq 8(%r14), %r10
	movq %r9, %r15
	addq %r10, %r15
	movq %r15, %rcx
	imulq $8, %rcx
	addq $16, %rcx
	movq %rcx, %rdi
	pushq %r9
	call my_malloc
	popq %r9
	movq %rax, %r12
	movq $4, 0(%r12)
	movq %r15, 8(%r12)
	leaq 16(%r13), %rsi
	leaq 16(%r12), %rdi
	movq 8(%r13), %rdx
	imulq $8, %rdx
	pushq %r9
	call my_memcpy
	popq %r9
	leaq 16(%r14), %rsi
	leaq 16(%r12,%r9,8), %rdi
	movq 8(%r14), %rdx
	imulq $8, %rdx
	pushq %r9
	call my_memcpy
	popq %r9
	movq %r12, %rax

end_inline_Badd:
        	addq $64, %rsp
end_Badd:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
  
my_printf:
  pushq %rbp
  movq %rsp, %rbp
  andq $-16, %rsp 
  call printf
  movq %rbp, %rsp
  popq %rbp
  ret

  # Input: %rdi = x, %rsi = y
# Output: %rax = min(x, y)
min:
    mov %rdi, %rax        # Move x into %rax (result register)
    cmp %rsi, %rdi        # Compare y with x
    cmovg %rsi, %rax      # If y < x (greater flag not set), move y into %rax
    ret                   # Return result
Beq:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	movq %rdi, %r8
	movq %rsi, %r9
	movq 0(%r8), %rax
	movq 0(%r9), %rcx
	cmpq %rax, %rcx
	jne fail_cmp
	movq 0(%r8), %r10
	cmpq $0, %r10
	je eq_none_bool_int
	cmpq $1, %r10
	je eq_none_bool_int
	cmpq $2, %r10
	je eq_none_bool_int
	cmpq $3, %r10
	je eq_string
	cmpq $4, %r10
	je eq_list
eq_none_bool_int:
	movq 8(%r8), %rax
	movq 8(%r9), %rcx
	cmpq %rax, %rcx
	je eq_ret_true
	jmp eq_ret_false
eq_string:
	leaq 16(%r8), %rdi
	leaq 16(%r9), %rsi
	call strcmp
	cmpl $0, %eax
	je eq_ret_true
	jmp eq_ret_false
eq_list:
	movq 8(%r8), %rax
	movq 8(%r9), %r11
	cmpq %rax, %r11
	jne eq_ret_false
	leaq 16(%r8), %r12
	leaq 16(%r9), %r13
	movq $0, %r10
eq_list_loop:
	cmpq %r11, %r10
	je eq_ret_true
	movq 0(%r12,%r10,8), %rdi
	movq 0(%r13,%r10,8), %rsi
	movq 0(%rdi), %rax
	cmpq 0(%rsi), %rax
	jne eq_ret_false
	pushq %r10
	pushq %r11
	call Beq
	popq %r11
	popq %r10
	movq 8(%rax), %rax
	cmpq $1, %rax
	jne eq_ret_false
	incq %r10
	jmp eq_list_loop
eq_ret_true:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	jmp eq_end
eq_ret_false:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
eq_end:
end_Beq:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
Bgt:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	movq %rdi, %r14
	movq %rsi, %r15
	movq 0(%r14), %rax
	movq 0(%r15), %rcx
	cmpq %rax, %rcx
	jne fail_cmp
	movq 0(%r14), %r10
	cmpq $0, %r10
	je gt_none_bool_int
	cmpq $1, %r10
	je gt_none_bool_int
	cmpq $2, %r10
	je gt_none_bool_int
	cmpq $3, %r10
	je gt_string
	cmpq $4, %r10
	je gt_list
gt_none_bool_int:
	movq 8(%r14), %rax
	movq 8(%r15), %rcx
	cmpq %rcx, %rax
	jg gt_ret_true
	jmp gt_ret_false
gt_string:
	leaq 16(%r14), %rdi
	leaq 16(%r15), %rsi
	call strcmp
	cmpl $0, %eax
	jg gt_ret_true
	jmp gt_ret_false
gt_list:
	movq 8(%r14), %rdi
	movq 8(%r15), %rsi
	call min
	movq %rax, %r11
	leaq 16(%r14), %r12
	leaq 16(%r15), %r13
	movq $0, %r10
gt_list_loop:
	cmpq %r11, %r10
	je end_gt_list_loop
	movq 0(%r12,%r10,8), %rdi
	movq 0(%r13,%r10,8), %rsi
	movq 0(%rdi), %rax
	cmpq 0(%rsi), %rax
	jne fail_cmp
	pushq %r10
	pushq %r11
	call Bgt
	popq %r11
	popq %r10
	movq 8(%rax), %rax
	cmpq $1, %rax
	jne gt_ret_false
	incq %r10
	jmp gt_list_loop
end_gt_list_loop:
	movq 8(%r14), %rdi
	movq 8(%r15), %rsi
	cmpq %rsi, %rdi
	jle gt_ret_false
gt_ret_true:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	jmp gt_end
gt_ret_false:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
gt_end:
end_Bgt:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
Bge:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	pushq %rdi
	pushq %rsi
	call Bgt
	popq %rsi
	popq %rdi
	pushq %r12
	movq 8(%rax), %r12
	call Beq
	pushq %r13
	movq 8(%rax), %r13
	orq %r12, %r13
	movq %r13, 8(%rax)
	popq %r13
	popq %r12
end_Bge:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
Blt:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	call Bge
	movq 8(%rax), %r12
	xorq $1, %r12
	movq %r12, 8(%rax)
end_Blt:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
Ble:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	call Bgt
	movq 8(%rax), %r12
	xorq $1, %r12
	movq %r12, 8(%rax)
end_Ble:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
Bneq:
	pushq %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	call Beq
	movq 8(%rax), %r12
	xorq $1, %r12
	movq %r12, 8(%rax)
end_Bneq:
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
fail_cmp:
	movq $cmp_error_msg, %rdi
	xorq %rax, %rax
	call my_printf
	movq $1, %rdi
	call exit

is_true:
  pushq %r12
  movq 8(%rdi), %r12
  cmpq $0, %r12
  popq %r12
  je actually_false
  jmp actually_true
actually_true:
  movq $1, %rax
  ret
actually_false:
  movq $0, %rax
  ret
range:
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rsp, %rbp
	movq 48(%rbp), %r12
	movq 0(%r12), %rdi
	cmpq $2, %rdi
	jne fail_func_call
	movq 8(%r12), %r12
	movq %r12, %rdi
	imulq $8, %rdi
	addq $16, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq %r12, 8(%rax)
	movq %rax, %r15
	movq $0, %r14
start_range_loop:
	cmpq %r12, %r14
	je end_range
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq %r14, 8(%rax)
	movq %rax, 16(%r15,%r14,8)
	incq %r14
	jmp start_range_loop
end_range:
	movq %r15, %rax
	movq %rbp, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbp
	ret
list:
	pushq %rbp
	pushq %r12
	movq %rsp, %rbp
	movq 24(%rbp), %r12
	cmpq $4, 0(%r12)
	jne fail_func_call
	movq %r12, %rax
	movq %rbp, %rsp
	popq %r12
	popq %rbp
	ret
	.data
add_error_msg:
	.string "error: invalid type for '+' operand\n"
bad_index_error_msg:
	.string "error: the index of a list must be an interger"
cmp_error_msg:
	.string "error: invalid comparison\n"
comma_space:
	.string ", "
div_error_msg:
	.string "error: invalid type for '/' operand\n"
error_msg:
	.string "error: invalid value\n"
fail_neg_error_msg:
	.string "error: the value cannot apply '-' operation\n"
false_str:
	.string "False"
for_error_msg:
	.string "error: the for loop can only iterate a list\n"
func_error_msg:
	.string "error: fail to call function for whatever reason\n"
get_error_msg:
	.string "error: the [] operator only works on list\n"
int_fmt:
	.string "%ld"
list_end:
	.string "]"
list_start:
	.string "["
mod_error_msg:
	.string "error: invalid type for '%' operand\n"
mul_error_msg:
	.string "error: invalid type for '*' operand\n"
newline_str:
	.string "\n"
none_str:
	.string "None"
out_of_range_error_msg:
	.string "error: the index is out of range\n"
str_fmt:
	.string "%s"
sub_error_msg:
	.string "error: invalid type for '-' operand\n"
true_str:
	.string "True"
