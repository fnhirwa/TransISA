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
	mov	w9, #1024
	mov	w11, #1
Lloh0:
	adrp	x10, _result@PAGE
Lloh1:
	add	x10, x10, _result@PAGEOFF
	stp	w9, wzr, [sp, #16]
	movk	w11, #512, lsl #16
	str	w9, [sp, #8220]
	mov	w9, #10
	mov	w12, #1
	str	w9, [sp, #12324]
	mov	w9, #63
	mov	x8, xzr
	str	w9, [x10]
	add	w9, w11, #3
	str	wzr, [sp, #12]
	str	w9, [sp, #12320]
	str	w12, [sp, #12328]
	str	x10, [sp]
	str	w12, [sp, #12332]
	; InlineAsm Start
	mov	x16, x9
	mov	x0, x12
	mov	x1, x10
	mov	x2, x12
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x9, x0
	; InlineAsm End
	mov	x9, x10
	mov	w10, w12
	str	w11, [sp, #12320]
	str	wzr, [sp, #12328]
	; InlineAsm Start
	mov	x16, x11
	mov	x0, x8
	mov	x1, x9
	mov	x2, x10
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
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
