	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	mov	w11, wzr
	mov	w10, #1
LBB0_1:
	mov	w8, w11
	mov	w9, #-2
LBB0_2:
	mov	w11, w9
	add	w9, w9, #1
	cbz	w11, LBB0_2
	add	w12, w11, #4
	orr	w11, w11, w12
	tbnz	w11, #31, LBB0_2
	add	w12, w10, #1
	subs	w13, w10, #4
	add	w10, w8, w9
	add	w11, w10, #2
	mov	w10, w12
	b.eq	LBB0_1
	orr	w13, w13, w12
	mov	w10, w12
	tbnz	w13, #31, LBB0_1
	add	w8, w8, w9
	mov	w11, #4
Lloh0:
	adrp	x9, _result@PAGE
Lloh1:
	add	x9, x9, _result@PAGEOFF
	add	w8, w8, #2
	movk	w11, #512, lsl #16
	mov	w13, #1
	mov	x10, xzr
	mov	w12, #1
	movk	w13, #512, lsl #16
	str	w8, [x9]
	; InlineAsm Start
	mov	x16, x11
	mov	x0, x12
	mov	x1, x9
	mov	x2, x12
	mov	x3, x10
	mov	x4, x10
	mov	x5, x10
	svc	#0x80
	mov	x11, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x16, x13
	mov	x0, x8
	mov	x1, x9
	mov	x2, x12
	mov	x3, x10
	mov	x4, x10
	mov	x5, x10
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc

	.globl	_result
.zerofill __DATA,__common,_result,4,2
.subsections_via_symbols
