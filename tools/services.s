    .globl	outln
    .type	outln, @function
outln:
    link.w  %fp, #0
    move.l  #'\n', %d1
    move    #6, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	outc
    .type	outc, @function
outc:
    link.w  %fp, #0
    move.l  8(%fp), %d1
    move    #6, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	outn
    .type	outn, @function
outn:
    link.w  %fp, #0
    move.l  8(%fp), %d1
    move    #3, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	outnln
    .type	outnln, @function
outnln:
    link.w  %fp, #0
    move.l  8(%fp), %d1
    move    #15, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	outl
    .type	outl, @function
outl:
    link.w  %fp, #0
    move.l  %d2, %sp@-
    move.l  8(%fp), %d1
	move.l  12(%fp),%d2
    move    #15, %d0
    trap    #15
    move.l  %sp@+, %d2
    unlk    %fp
    rts

    .globl	outu
    .type	outu, @function
outu:
    link.w  %fp, #0
    move.l  8(%fp), %d1
    move    #11, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	outx
    .type	outx, @function
outx:
    link.w  %fp, #0
    move.l  8(%fp), %d1
    move    #12, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	outf
    .type	outf, @function
outf:
    link.w  %fp, #0
    move.l  %d2, %sp@-
    move.l  8(%fp), %d1
	move.l  12(%fp),%d2
    move    #10, %d0
    trap    #15
    move.l  %sp@+, %d2
    unlk    %fp
    rts

    .globl	outsln
    .type	outsln, @function
outsln:
    link.w  %fp, #0
    move.l  8(%fp), %a1
    move    #13, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	outs
    .type	outs, @function
outs:
    link.w  %fp, #0
    move.l  8(%fp), %a1
    move    #14, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	halt
    .type	halt, @function
halt:
    move    #9, %d0
    trap    #15

    .globl	currentTime
    .type	currentTime, @function
currentTime:
    link.w  %fp, #0
    move    #7, %d0
    trap    #15
    unlk    %fp
    rts
