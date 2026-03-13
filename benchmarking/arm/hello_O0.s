	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	stp	x28, x27, [sp, #-16]!
	sub	sp, sp, #3, lsl #12
	sub	sp, sp, #32
	.cfi_def_cfa_offset 12336
	.cfi_offset w27, -8
	.cfi_offset w28, -16
	mov	w9, #1024
	mov	w11, #1
	mov	w12, #13
	stp	w9, wzr, [sp, #8]
	mov	x8, xzr
	str	w9, [sp, #8208]
	mov	w9, #1
	movk	w9, #512, lsl #16
	str	wzr, [sp, #4]
	add	w10, w9, #3
	str	w11, [sp, #12312]
	str	w10, [sp, #12308]
	str	w12, [sp, #12316]
	; InlineAsm Start
	mov	x16, x10
	mov	x0, x11
	mov	x1, x8
	mov	x2, x12
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x10, x0
	; InlineAsm End
	mov	w10, w12
	str	w9, [sp, #12308]
	str	wzr, [sp, #12312]
	; InlineAsm Start
	mov	x16, x9
	mov	x0, x8
	mov	x1, x8
	mov	x2, x10
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	add	sp, sp, #3, lsl #12
	add	sp, sp, #32
	ldp	x28, x27, [sp], #16
	ret
	.cfi_endproc

.subsections_via_symbols
