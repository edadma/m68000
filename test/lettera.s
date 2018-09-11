    .section    vectorsSection

    .long       systemStack
    .long       _start

    .section    systemStackSection
    .comm       systemStack, 10000, 2

    .text
_start:
    move.b #'A', %d1
    move #6, %d0
    trap #15
    move #9, %d0
    trap #15
