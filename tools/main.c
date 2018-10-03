#include <stdio.h>
#include <stdlib.h>


void
main() {
    for (int i = 0; i < 10000000; i++) {}
}

void __attribute__ ((interrupt))
autovector_level1_isr() {
    printf( "interrupt" );
}