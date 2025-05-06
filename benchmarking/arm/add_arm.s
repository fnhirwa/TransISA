	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	stp	x24, x23, [sp, #-64]!
	stp	x22, x21, [sp, #16]
	stp	x20, x19, [sp, #32]
	stp	x29, x30, [sp, #48]
	sub	sp, sp, #3, lsl #12
	sub	sp, sp, #48
	.cfi_def_cfa_offset 12400
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	adrp	x8, _result@PAGE
	mov	w10, #63
	mov	w19, #1
	ldr	w9, [x8, _result@PAGEOFF]
	str	wzr, [sp, #28]
	str	w10, [x9]
	mov	w10, #4
	mov	w9, #1024
	ldr	w8, [x8, _result@PAGEOFF]
	stp	w19, w10, [sp, #20]
	ldur	x20, [sp, #20]
	str	w9, [sp, #8228]
	ldr	w8, [x8]
	stp	w19, w8, [sp, #12]
	ldp	x21, x0, [sp, #16]
	ldur	x22, [sp, #12]
	bl	_convert_x86_to_macos_syscall
	mov	x23, xzr
	; InlineAsm Start
	mov	x16, x8
	mov	x0, x0
	mov	x1, x20
	mov	x2, x21
	mov	x3, x22
	mov	x4, x23
	mov	x5, x23
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	str	w19, [sp, #8]
	ldp	x0, x20, [sp, #8]
	str	wzr, [sp, #20]
	ldur	x19, [sp, #20]
	ldur	x21, [sp, #12]
	str	x8, [sp, #12328]
	bl	_convert_x86_to_macos_syscall
	; InlineAsm Start
	mov	x16, x8
	mov	x0, x0
	mov	x1, x19
	mov	x2, x20
	mov	x3, x21
	mov	x4, x23
	mov	x5, x23
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	str	x8, [sp, #12328]
	add	sp, sp, #3, lsl #12
	add	sp, sp, #48
	ldp	x29, x30, [sp, #48]
	ldp	x20, x19, [sp, #32]
	ldp	x22, x21, [sp, #16]
	ldp	x24, x23, [sp], #64
	ret
	.cfi_endproc

	.globl	_convert_x86_to_macos_syscall
	.p2align	2
_convert_x86_to_macos_syscall:
	.cfi_startproc
	mov	w8, #33554432
	add	x0, x0, x8
	ret
	.cfi_endproc

	.globl	_result
.zerofill __DATA,__common,_result,4,2
	.globl	_result.1
.zerofill __DATA,__common,_result.1,1,0
.subsections_via_symbols
