	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	stp	x28, x27, [sp, #-16]!
	sub	sp, sp, #3, lsl #12
	sub	sp, sp, #48
	.cfi_def_cfa_offset 12352
	.cfi_offset w27, -8
	.cfi_offset w28, -16
	mov	w8, #1024
	str	wzr, [sp, #4]
	stp	w8, wzr, [sp, #8]
	str	w8, [sp, #8212]
	mov	w8, #1
	str	w8, [sp, #12316]
	mov	w8, #2
	str	w8, [sp, #12320]
	mov	w8, #10
	str	wzr, [sp, #12312]
	str	w8, [sp, #12324]
	b	LBB0_2
LBB0_1:
	ldr	w9, [sp, #12316]
	ldr	w10, [sp, #12312]
	ldr	w11, [sp, #12320]
	str	w8, [sp, #12328]
	str	w8, [sp, #12312]
	add	w8, w9, w10
	add	w9, w11, #1
	str	w8, [sp, #12316]
	str	w9, [sp, #12320]
LBB0_2:
	ldr	w8, [sp, #12320]
	ldr	w12, [sp, #12324]
	sub	w9, w8, w12
	cmp	w9, #0
	cset	w10, lt
	cmp	w8, #0
	ldr	w8, [sp, #12316]
	cset	w11, lt
	cmp	w12, #0
	cset	w12, lt
	cbz	w9, LBB0_1
	eor	w12, w11, w12
	eor	w10, w10, w11
	lsr	w9, w9, #31
	and	w10, w12, w10
	cmp	w9, w10
	b.ne	LBB0_1
	ldr	w10, [sp, #12316]
	ldr	w11, [sp, #8]
Lloh0:
	adrp	x12, _result@PAGE
Lloh1:
	add	x12, x12, _result@PAGEOFF
	add	x13, sp, #2, lsl #12
	mov	x9, xzr
	str	w10, [x12]
	sub	w10, w11, #1
	mov	w11, #1
	add	x13, x13, #24
	movk	w11, #512, lsl #16
	str	w10, [sp, #8]
	str	w8, [x13, w10, sxtw #2]
	add	w8, w11, #3
	mov	w10, #1
	str	w8, [sp, #12312]
	str	x12, [sp, #12328]
	mov	w12, w12
	str	w10, [sp, #12332]
	str	w10, [sp, #12324]
	; InlineAsm Start
	mov	x16, x8
	mov	x0, x10
	mov	x1, x12
	mov	x2, x10
	mov	x3, x9
	mov	x4, x9
	mov	x5, x9
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ldr	w12, [sp, #12328]
	ldrsw	x8, [sp, #8]
	str	w11, [sp, #12312]
	ldr	w10, [x13, x8, lsl #2]
	add	w8, w8, #1
	ldr	w13, [sp, #12324]
	str	w8, [sp, #8]
	str	w10, [sp, #12332]
	; InlineAsm Start
	mov	x16, x11
	mov	x0, x10
	mov	x1, x12
	mov	x2, x13
	mov	x3, x9
	mov	x4, x9
	mov	x5, x9
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	add	sp, sp, #3, lsl #12
	add	sp, sp, #48
	ldp	x28, x27, [sp], #16
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc

	.globl	_result
.zerofill __DATA,__common,_result,4,2
.subsections_via_symbols
