    .section    vectorsSection

    .long       systemStack
    .long       _start

    .section    systemStackSection
    .comm       systemStack, 10000, 2

    .text
_start:
