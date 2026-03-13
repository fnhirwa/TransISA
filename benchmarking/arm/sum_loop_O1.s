	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	mov	w8, wzr
	mov	w9, #-6
LBB0_1:
	add	w8, w9, w8
	mov	w10, w9
	add	w9, w9, #1
	add	w8, w8, #7
	cbz	w10, LBB0_1
	add	w11, w10, #8
	orr	w10, w10, w11
	tbnz	w10, #31, LBB0_1
	mov	w11, #4
Lloh0:
	adrp	x10, _result@PAGE
Lloh1:
	add	x10, x10, _result@PAGEOFF
	movk	w11, #512, lsl #16
	mov	w13, #1
	mov	x9, xzr
	mov	w12, #1
	movk	w13, #512, lsl #16
	str	w8, [x10]
	; InlineAsm Start
	mov	x16, x11
	mov	x0, x12
	mov	x1, x10
	mov	x2, x12
	mov	x3, x9
	mov	x4, x9
	mov	x5, x9
	svc	#0x80
	mov	x11, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x16, x13
	mov	x0, x8
	mov	x1, x10
	mov	x2, x12
	mov	x3, x9
	mov	x4, x9
	mov	x5, x9
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc

	.globl	_result
.zerofill __DATA,__common,_result,4,2
.subsections_via_symbols
