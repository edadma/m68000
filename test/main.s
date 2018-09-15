#NO_APP
	.file	"main.c"
	.globl	letter
	.data
	.type	letter, @object
	.size	letter, 1
letter:
	.byte	97
	.text
	.align	2
	.globl	main
	.type	main, @function
main:
	link.w %fp,#0
	move.b letter,%d0
	addq.b #1,%d0
	ext.w %d0
	move.w %d0,%a0
	move.l %a0,-(%sp)
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
