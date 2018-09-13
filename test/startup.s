    .section    .rodata

    .long       systemStackTop
    .long       _start

    .section    systemStackSection
    .comm       systemStack, 10000, 2
systemStackTop:

    .text
_start:
    jsr     main
    jmp     halt

outc:
	link.w  %fp,#-4
	move.l  8(%fp),%d1
    move    #6, %d0
    trap    #15
	unlk %fp
    rts

halt:
    move    #9, %d0
    trap    #15
    rts
