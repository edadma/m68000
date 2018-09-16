    .section    .rodata

    .long       __stack_end__
    .long       _start

    .text
_start:
    jsr     main
    jmp     halt

	.globl	outc
	.type	outc, @function
outc:
	link.w  %fp, #0
	move.l  8(%fp), %d1
    move    #6, %d0
    trap    #15
	unlk    %fp
    rts

halt:
    move    #9, %d0
    trap    #15
    rts
