	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_def_cfa_offset 32
	mov	w8, #13
Lloh0:
	adrp	x9, _msg@PAGE
Lloh1:
	add	x9, x9, _msg@PAGEOFF
	mov	w10, #1
	mov	w11, #4
	str	x9, [sp, #16]
	str	w8, [sp, #28]
	stp	w11, w10, [sp, #8]
	; InlineAsm Start
	mov	x0, x10
	mov	x1, x9
	mov	x2, x8
	mov	x16, #4
	orr	x16, x16, #0x2000000
	svc	#0x80

	; InlineAsm End
	; InlineAsm Start
	mov	x0, x10
	mov	x16, #1
	orr	x16, x16, #0x2000000
	svc	#0x80

	; InlineAsm End
	add	sp, sp, #32
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc

	.section	__DATA,__data
	.globl	_msg
_msg:
	.asciz	"Hello World!\n"

.subsections_via_symbols
