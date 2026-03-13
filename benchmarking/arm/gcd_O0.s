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
	str	w8, [x19, #8220]
	mov	w8, #48
	stur	w8, [x29, #-32]
	mov	w8, #18
	b	LBB0_2
LBB0_1:
	ldp	w9, w8, [x29, #-32]
	sub	w8, w8, w9
LBB0_2:
	stur	w8, [x29, #-28]
LBB0_3:
	ldp	w8, w9, [x29, #-32]
	cmp	w8, w9
	b.eq	LBB0_6
	ldur	w9, [x29, #-28]
	sub	w10, w8, w9
	cmp	w10, #0
	cset	w10, lt
	cmp	w8, #0
	cset	w8, lt
	cmp	w9, #0
	cset	w9, lt
	eor	w9, w8, w9
	eor	w8, w10, w8
	and	w8, w9, w8
	cmp	w10, w8
	b.ne	LBB0_1
	ldp	w8, w9, [x29, #-32]
	sub	w8, w8, w9
	stur	w8, [x29, #-32]
	b	LBB0_3
LBB0_6:
	stur	w8, [x29, #-28]
	ldur	w8, [x29, #-32]
	mov	w9, #1
	movk	w9, #512, lsl #16
Lloh0:
	adrp	x10, _result@PAGE
Lloh1:
	add	x10, x10, _result@PAGEOFF
	add	w8, w8, #48
	add	w11, w9, #3
	str	w8, [x10]
	mov	x8, sp
	stur	w11, [x29, #-32]
	mov	w11, #1
	sub	x12, x8, #16
	stp	w11, w11, [x29, #-24]
	mov	sp, x12
	mov	x12, xzr
	ldur	w13, [x29, #-32]
	mov	w14, w11
	stur	x10, [x8, #-16]
	; InlineAsm Start
	mov	x16, x13
	mov	x0, x14
	mov	x1, x10
	mov	x2, x11
	mov	x3, x12
	mov	x4, x12
	mov	x5, x12
	svc	#0x80
	mov	x10, x0
	; InlineAsm End
	ldur	x8, [x8, #-16]
	ldur	w10, [x29, #-28]
	mov	w11, w11
	stur	w9, [x29, #-32]
	stur	w10, [x29, #-24]
	; InlineAsm Start
	mov	x16, x9
	mov	x0, x10
	mov	x1, x8
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
