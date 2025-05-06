	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	__start
	.p2align	2
__start:
	.cfi_startproc
	stp	x22, x21, [sp, #-48]!
	stp	x20, x19, [sp, #16]
	stp	x29, x30, [sp, #32]
	sub	sp, sp, #3, lsl #12
	sub	sp, sp, #48
	.cfi_def_cfa_offset 12384
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	w8, #1024
	mov	w9, #13
Lloh0:
	adrp	x21, _msg@PAGE
Lloh1:
	add	x21, x21, _msg@PAGEOFF
	str	w8, [sp, #8228]
	mov	w8, #4
	stp	w9, wzr, [sp, #24]
	mov	w9, #1
	stp	w8, w9, [sp, #8]
	ldr	x20, [sp, #24]
	ldr	x0, [sp, #8]
	ldur	x19, [sp, #12]
	str	x21, [sp, #16]
	bl	_convert_x86_to_macos_syscall
	mov	x8, xzr
	; InlineAsm Start
	mov	x16, x8
	mov	x0, x0
	mov	x1, x19
	mov	x2, x21
	mov	x3, x20
	mov	x4, x8
	mov	x5, x8
	svc	#0x80
	mov	x8, x0
	; InlineAsm End
	str	x8, [sp, #12328]
	add	sp, sp, #3, lsl #12
	add	sp, sp, #48
	ldp	x29, x30, [sp, #32]
	ldp	x20, x19, [sp, #16]
	ldp	x22, x21, [sp], #48
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc

	.globl	_convert_x86_to_macos_syscall
	.p2align	2
_convert_x86_to_macos_syscall:
	.cfi_startproc
	mov	w8, #33554432
	add	x0, x0, x8
	ret
	.cfi_endproc

	.section	__DATA,__data
	.globl	_msg
_msg:
	.asciz	"Hello World!\n"

.subsections_via_symbols
