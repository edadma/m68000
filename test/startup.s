    .section    .rodata

    .long       __stack_end__
    .long       _start

    .text
    .globl	_start
_start:
    /* zero bss section */
    movea.l #__bss_start__, %a0
    movea.l #__bss_end__, %a1
.1:
    cmpa.l  %a0, %a1
    jeq     .2
    move.b  #0, %a0@+
    jmp     .1
.2:
    /* copy data section */
    movea.l #__data_rom_start__, %a0
    movea.l #__data_start__, %a1
    movea.l #__data_end__, %a2
.3:
    cmpa.l  %a2, %a1
    jeq     .4
    move.b  %a0@+, %a1@+
    jmp     .3
.4:
    jsr     main
    jmp     halt

    .globl	printc
    .type	printc, @function
printc:
    link.w  %fp, #0
    move.l  8(%fp), %d1
    move    #6, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	printn
    .type	printn, @function
printn:
    link.w  %fp, #0
    move.l  8(%fp), %d1
    move    #3, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	println
    .type	println, @function
println:
    link.w  %fp, #0
    move.l  8(%fp), %a1
    move    #13, %d0
    trap    #15
    unlk    %fp
    rts

    .globl	halt
    .type	halt, @function
halt:
    move    #9, %d0
    trap    #15
