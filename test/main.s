#NO_APP
	.file	"main.c"
	.text
	.align	2
	.globl	main
	.type	main, @function
main:
	link.w %fp,#0
	pea 97.w
	jsr outc
	addq.l #4,%sp
	pea 10.w
	jsr outc
	addq.l #4,%sp
	nop
	unlk %fp
	rts
	.size	main, .-main
	.ident	"GCC: (crosstool-NG crosstool-ng-1.23.0) 6.3.0"
	.section	.note.GNU-stack,"",@progbits
