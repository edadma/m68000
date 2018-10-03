    .section    .rodata

    .long       __stack_init__
    .long       _start
    .long       0   /* bus error */
    .long       0   /* address error */
    .long       0   /* illegal instruction */
    .long       0   /* zero divide */
    .long       0   /* CHK instruction */
    .long       0   /* TRAPV instruction */
    .long       0   /* priviledge violation */
    .long       0   /* trace */
    .long       0   /* 1010 instruction trap */
    .long       0   /* 1111 instruction trap */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* uninitialized interrupt */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* reserved */
    .long       0   /* spurious interrupt */
    .long       autovector_level1_isr

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
    move    #0x2000, %sr
    jsr     main
    jmp     halt
