	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	mov	w11, wzr
	mov	w8, #1
	mov	w10, #-7
LBB0_1:
	mov	w9, w8
	mov	w12, w10
	add	w8, w11, w8
	add	w10, w10, #1
	mov	w11, w9
	cbz	w12, LBB0_1
	add	w11, w12, #10
	orr	w12, w12, w11
	mov	w11, w9
	tbnz	w12, #31, LBB0_1
	adrp	x11, _result@PAGE
	mov	w12, #4
	mov	w14, #1
	mov	x10, xzr
	mov	w9, w9
	movk	w12, #512, lsl #16
	mov	w13, #1
	movk	w14, #512, lsl #16
	str	w8, [x11, _result@PAGEOFF]
	; InlineAsm Start
	mov	x16, x12
	mov	x0, x13
	mov	x1, x9
	mov	x2, x13
	mov	x3, x10
	mov	x4, x10
	mov	x5, x10
	svc	#0x80
	mov	x11, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x16, x14
	mov	x0, x8
	mov	x1, x9
	mov	x2, x13
	mov	x3, x10
	mov	x4, x10
	mov	x5, x10
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ret
	.cfi_endproc

	.globl	_result
.zerofill __DATA,__common,_result,4,2
.subsections_via_symbols
