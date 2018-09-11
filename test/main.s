#NO_APP
	.file	"main.c"
	.section	.rodata
.LC0:
	.string	"Hello World, %d\n"
	.text
	.align	2
	.globl	main
	.type	main, @function
main:
	link.w %fp,#0
	pea .LC0
	jsr printf
	addq.l #4,%sp
	moveq #0,%d0
	unlk %fp
	rts
	.size	main, .-main
	.ident	"GCC: (crosstool-NG crosstool-ng-1.23.0) 6.3.0"
	.section	.note.GNU-stack,"",@progbits
