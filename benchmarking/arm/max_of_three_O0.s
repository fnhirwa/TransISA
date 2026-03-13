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
	mov	w9, #17
	str	wzr, [x19, #12]
	stp	w8, wzr, [x19, #16]
	str	w8, [x19, #8216]
	mov	w8, #42
	stp	w9, w8, [x29, #-36]
	mov	w8, #29
	stp	w8, w9, [x29, #-28]
	mov	w8, #25
	tbnz	w8, #31, LBB0_3
	cbz	w8, LBB0_3
	ldur	w8, [x29, #-32]
	stur	w8, [x29, #-24]
LBB0_3:
	ldp	w9, w10, [x29, #-28]
	sub	w8, w9, w10
	cmp	w8, #0
	cset	w11, lt
	cmp	w9, #0
	cset	w9, lt
	cmp	w10, #0
	cset	w10, lt
	eor	w10, w9, w10
	eor	w9, w11, w9
	and	w9, w10, w9
	cmp	w11, w9
	b.ne	LBB0_6
	cbz	w8, LBB0_6
	ldur	w8, [x29, #-28]
	stur	w8, [x29, #-24]
LBB0_6:
	mov	w11, #1
	ldr	w10, [x19, #16]
	ldur	w8, [x29, #-24]
	movk	w11, #512, lsl #16
Lloh0:
	adrp	x9, _result@PAGE
Lloh1:
	add	x9, x9, _result@PAGEOFF
	add	w12, w11, #3
	sub	w10, w10, #1
	str	w8, [x9]
	stur	w12, [x29, #-36]
	add	x12, x19, #2, lsl #12
	add	x12, x12, #28
	str	w10, [x19, #16]
	str	w8, [x12, w10, sxtw #2]
	mov	x10, sp
	mov	w8, #1
	sub	x13, x10, #16
	stp	w8, w8, [x29, #-24]
	mov	sp, x13
	mov	x13, xzr
	ldur	w14, [x29, #-36]
	mov	w15, w8
	stur	x9, [x10, #-16]
	; InlineAsm Start
	mov	x16, x14
	mov	x0, x15
	mov	x1, x9
	mov	x2, x8
	mov	x3, x13
	mov	x4, x13
	mov	x5, x13
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ldur	x10, [x10, #-16]
	ldrsw	x8, [x19, #16]
	stur	w11, [x29, #-36]
	ldr	w9, [x12, x8, lsl #2]
	add	w8, w8, #1
	ldur	w12, [x29, #-24]
	str	w8, [x19, #16]
	stur	w9, [x29, #-20]
	; InlineAsm Start
	mov	x16, x11
	mov	x0, x9
	mov	x1, x10
	mov	x2, x12
	mov	x3, x13
	mov	x4, x13
	mov	x5, x13
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
