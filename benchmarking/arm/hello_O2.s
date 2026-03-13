	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	mov	w9, #4
	mov	x8, xzr
	mov	w12, #1
	movk	w9, #512, lsl #16
	mov	w10, #1
	mov	w11, #13
	movk	w12, #512, lsl #16
	; InlineAsm Start
	mov	x16, x9
	mov	x0, x10
	mov	x1, x8
	mov	x2, x11
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x9, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x16, x12
	mov	x0, x8
	mov	x1, x8
	mov	x2, x11
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ret
	.cfi_endproc

.subsections_via_symbols
