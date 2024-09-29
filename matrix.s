.text
	.globl	main
f:
	pushq 	%rbp
	movq 	%rsp, %rbp	
	subq $48, %rsp
	cmpl 	$15, %edi
	je ret_f_0

	movl %esi, %r8d
	sall $4, %r8d
	orl %edi, %r8d # key
	leaq memo(%rip), %r9 # memo
	movl (%r9, %r8, 4), %edx  # r = memo[key]
	testl %edx, %edx
	jnz ret_f_r

	movl $0, %r10d # s
	movl $0, %ecx  # j
for_0:
	cmpl $15, %ecx
	je ret_for_0
	movl $1, %r11d # col = 1
	sall %cl, %r11d # col = 1 << j
	testl %esi, %r11d
	jz for_0_before_next # continue

	# call f
	# before call
	movl %edi, -4(%rbp) # i
	movl %esi, -8(%rbp) # c
	# movl %edx, (%rbp) # r no need
	movl %ecx, -12(%rbp) # j
	movl %r10d, -16(%rbp) # s
	movl %r8d, -24(%rbp) # key

	# set argument for f
	incl %edi
	subl %r11d, %esi

	call f

	# restore the variable
	movl -4(%rbp), %edi
	movl -8(%rbp), %esi
	movl -12(%rbp), %ecx
	movl -16(%rbp), %r10d
	movl -24(%rbp), %r8d


	leaq m(%rip), %r9 # m
	movl %edi, %edx # tmp0
	imull $15, %edx # tmp0 * 15
	addl %ecx, %edx
	sall $2, %edx
	addl (%r9, %rdx, 4), %eax

	cmpl %r10d, %eax # s < x
	jg if_x_g_s

for_0_before_next:
	incl %ecx
	jmp for_0

ret_for_0:
	leaq memo(%rip), %r9 # memo
	movl %r10d, (%r9, %r8, 4) 
	movl %r10d, %eax
	jmp ret_f

if_x_g_s: 
	movl %eax, %r10d
	jmp for_0_before_next

ret_f_0:
	movl $0, %eax
	jmp ret_f


ret_f_r:
	movl %edx, %eax
	jmp ret_f

end_f_0:
	movl %r10d, (%r9, %r8, 4)  # memo[key] = s
	movl %r10d, %eax
ret_f:
	addq $48, %rsp
	popq	%rbp
    ret
main:
	pushq %rbp
	movq %rsp, %rbp	
	subq $16, %rsp
	movl $0, %edi
	movl $1, %esi # (1)
	shll $15, %esi #(1 << N:15)
	decl %esi # (1 << N) - 1
	call f
    leaq fmt(%rip), %rdi 
	movl %eax, %esi
	movl    $0, %eax   
    call printf

    xorq    %rax, %rax
	addq  	$16, %rsp
	popq 	%rbp
    ret

.data
fmt: .string "solution = %d\n"
m:
	.long	7
	.long	53
	.long	183
	.long	439
	.long	863
	.long	497
	.long	383
	.long	563
	.long	79
	.long	973
	.long	287
	.long	63
	.long	343
	.long	169
	.long	583
	.long	627
	.long	343
	.long	773
	.long	959
	.long	943
	.long	767
	.long	473
	.long	103
	.long	699
	.long	303
	.long	957
	.long	703
	.long	583
	.long	639
	.long	913
	.long	447
	.long	283
	.long	463
	.long	29
	.long	23
	.long	487
	.long	463
	.long	993
	.long	119
	.long	883
	.long	327
	.long	493
	.long	423
	.long	159
	.long	743
	.long	217
	.long	623
	.long	3
	.long	399
	.long	853
	.long	407
	.long	103
	.long	983
	.long	89
	.long	463
	.long	290
	.long	516
	.long	212
	.long	462
	.long	350
	.long	960
	.long	376
	.long	682
	.long	962
	.long	300
	.long	780
	.long	486
	.long	502
	.long	912
	.long	800
	.long	250
	.long	346
	.long	172
	.long	812
	.long	350
	.long	870
	.long	456
	.long	192
	.long	162
	.long	593
	.long	473
	.long	915
	.long	45
	.long	989
	.long	873
	.long	823
	.long	965
	.long	425
	.long	329
	.long	803
	.long	973
	.long	965
	.long	905
	.long	919
	.long	133
	.long	673
	.long	665
	.long	235
	.long	509
	.long	613
	.long	673
	.long	815
	.long	165
	.long	992
	.long	326
	.long	322
	.long	148
	.long	972
	.long	962
	.long	286
	.long	255
	.long	941
	.long	541
	.long	265
	.long	323
	.long	925
	.long	281
	.long	601
	.long	95
	.long	973
	.long	445
	.long	721
	.long	11
	.long	525
	.long	473
	.long	65
	.long	511
	.long	164
	.long	138
	.long	672
	.long	18
	.long	428
	.long	154
	.long	448
	.long	848
	.long	414
	.long	456
	.long	310
	.long	312
	.long	798
	.long	104
	.long	566
	.long	520
	.long	302
	.long	248
	.long	694
	.long	976
	.long	430
	.long	392
	.long	198
	.long	184
	.long	829
	.long	373
	.long	181
	.long	631
	.long	101
	.long	969
	.long	613
	.long	840
	.long	740
	.long	778
	.long	458
	.long	284
	.long	760
	.long	390
	.long	821
	.long	461
	.long	843
	.long	513
	.long	17
	.long	901
	.long	711
	.long	993
	.long	293
	.long	157
	.long	274
	.long	94
	.long	192
	.long	156
	.long	574
	.long	34
	.long	124
	.long	4
	.long	878
	.long	450
	.long	476
	.long	712
	.long	914
	.long	838
	.long	669
	.long	875
	.long	299
	.long	823
	.long	329
	.long	699
	.long	815
	.long	559
	.long	813
	.long	459
	.long	522
	.long	788
	.long	168
	.long	586
	.long	966
	.long	232
	.long	308
	.long	833
	.long	251
	.long	631
	.long	107
	.long	813
	.long	883
	.long	451
	.long	509
	.long	615
	.long	77
	.long	281
	.long	613
	.long	459
	.long	205
	.long	380
	.long	274
	.long	302
	.long	35
	.long	805
        .bss
memo:
        .space	2097152

## Local Variables:
## compile-command: "gcc matrix.s && ./a.out"
## End:
