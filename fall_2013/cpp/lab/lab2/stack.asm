	.file	"stack.cpp"
	.section	.rodata
.LC0:
	.string	"Hacked!"
	.text
	.globl	_Z5hellov
	.type	_Z5hellov, @function
_Z5hellov:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$.LC0, %edi
	call	puts
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	_Z5hellov, .-_Z5hellov
	.globl	_Z1fv
	.type	_Z1fv, @function
_Z1fv:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, -52(%rbp)
	jmp	.L4
.L5:
	movl	-52(%rbp), %eax
	cltq
	movq	$_Z5hellov, -48(%rbp,%rax,8)
	addl	$1, -52(%rbp)
.L4:
	cmpl	$19, -52(%rbp)
	setle	%al
	testb	%al, %al
	jne	.L5
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	_Z1fv, .-_Z1fv
	.globl	main
	.type	main, @function
main:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	call	_Z1fv
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	main, .-main
	.ident	"GCC: (Ubuntu/Linaro 4.7.2-2ubuntu1) 4.7.2"
	.section	.note.GNU-stack,"",@progbits
