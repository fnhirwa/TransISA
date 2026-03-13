	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	mov	w11, #4
	mov	x8, xzr
Lloh0:
	adrp	x9, _result@PAGE
Lloh1:
	add	x9, x9, _result@PAGEOFF
	mov	w10, #42
	movk	w11, #512, lsl #16
	mov	w13, #1
	mov	w12, #1
	str	w10, [x9]
	movk	w13, #512, lsl #16
	; InlineAsm Start
	mov	x16, x11
	mov	x0, x12
	mov	x1, x9
	mov	x2, x12
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x11, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x16, x13
	mov	x0, x10
	mov	x1, x9
	mov	x2, x12
	mov	x3, x8
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc

	.globl	_result
.zerofill __DATA,__common,_result,4,2
.subsections_via_symbols
