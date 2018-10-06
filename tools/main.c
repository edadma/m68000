#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include "services.h"

#define TIMER (*(char*)0xFFFFFF00)


void
main() {
//    TIMER = 250;
//    for (int i = 0; i < 5000000; i++) {}
    char buffer[80];
    time_t rawtime;
    struct tm* tm;

    putenv( "TZ=EST+5EDT,M3.2.0/2,M11.1.0/2" );
    tzset();
    time( &rawtime );
    outl( rawtime );
    outnl();
    tm = localtime( &rawtime );
    strftime( buffer, 80, "%x - %I:%M%p", tm );
    printf( "Formatted date & time : |%s|\n", buffer );
}

void __attribute__ ((interrupt))
autovector_level1_isr() {
    outln( "interrupt" );
//    TIMER = 0;
}
