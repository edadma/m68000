#include <stdio.h>
#include <stdlib.h>
#include "services.h"

#define TIMER (*(char*)0xFFFFFF00)


void
main() {
    TIMER = 250;
    for (int i = 0; i < 5000000; i++) {}
}

void __attribute__ ((interrupt))
autovector_level1_isr() {
    outln( "interrupt" );
    TIMER = 0;
}