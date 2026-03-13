	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	mov	w10, #48
	mov	w9, #18
LBB0_1:
	sub	w8, w10, w9
	eor	w11, w9, w10
	bic	w12, w8, w11
	and	w11, w10, w11
	orr	w11, w11, w12
	and	w12, w10, w11, asr #31
	cmp	w11, #0
	csel	w8, w10, w8, lt
	sub	w9, w9, w12
	mov	w10, w8
	cmp	w8, w9
	b.ne	LBB0_1
	add	w10, w8, #48
Lloh0:
	adrp	x11, _result@PAGE
Lloh1:
	add	x11, x11, _result@PAGEOFF
	mov	w12, #4
	mov	w14, #1
	mov	x9, xzr
	movk	w12, #512, lsl #16
	mov	w13, #1
	movk	w14, #512, lsl #16
	str	w10, [x11]
	; InlineAsm Start
	mov	x16, x12
	mov	x0, x13
	mov	x1, x11
	mov	x2, x13
	mov	x3, x9
	mov	x4, x9
	mov	x5, x9
	svc	#0x80
	mov	x10, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x16, x14
	mov	x0, x8
	mov	x1, x11
	mov	x2, x13
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
