	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	stp	x20, x19, [sp, #-32]!
	stp	x29, x30, [sp, #16]
	add	x29, sp, #16
	sub	sp, sp, #3, lsl #12
	sub	sp, sp, #48
	mov	x19, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	w8, #1024
	str	wzr, [x19, #12]
	stp	w8, wzr, [x19, #16]
	str	w8, [x19, #8216]
	mov	w8, #1
	stp	wzr, w8, [x29, #-36]
LBB0_1:
	ldp	w13, w8, [x29, #-36]
	sub	w9, w8, #7
	add	w10, w8, #1
	add	w8, w13, w8
	cmp	w9, #0
	stp	w8, w10, [x29, #-36]
	cset	w11, lt
	cmp	w10, #0
	cset	w12, lt
	eor	w12, w11, w12
	csel	w12, wzr, w12, ge
	cmp	w11, w12
	ccmp	w9, #0, #4, eq
	b.eq	LBB0_1
	mov	w10, #1
	ldur	w8, [x29, #-36]
Lloh0:
	adrp	x9, _result@PAGE
Lloh1:
	add	x9, x9, _result@PAGEOFF
	movk	w10, #512, lsl #16
	add	w11, w10, #3
	stur	w8, [x29, #-28]
	stur	w11, [x29, #-36]
	mov	x11, sp
	str	w8, [x9]
	mov	w8, #1
	sub	x12, x11, #16
	stp	w8, w8, [x29, #-24]
	mov	sp, x12
	mov	x12, xzr
	ldur	w13, [x29, #-36]
	mov	w14, w8
	stur	x9, [x11, #-16]
	; InlineAsm Start
	mov	x16, x13
	mov	x0, x14
	mov	x1, x9
	mov	x2, x8
	mov	x3, x12
	mov	x4, x12
	mov	x5, x12
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ldur	w11, [x29, #-20]
	ldur	w8, [x29, #-28]
	stur	w10, [x29, #-36]
	stur	w8, [x29, #-24]
	; InlineAsm Start
	mov	x16, x10
	mov	x0, x8
	mov	x1, x9
	mov	x2, x11
	mov	x3, x12
	mov	x4, x12
	mov	x5, x12
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	sub	sp, x29, #16
	ldp	x29, x30, [sp, #16]
	ldp	x20, x19, [sp], #32
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc

	.globl	_result
.zerofill __DATA,__common,_result,4,2
.subsections_via_symbols
