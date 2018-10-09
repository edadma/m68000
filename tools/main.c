#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <errno.h>
#include "services.h"

#define TIMER (*(char*)0xFFFFFF00)


char ch = 'a';

void
main() {
//    TIMER = 250;
//    for (int i = 0; i < 5000000; i++) {}
//    char buffer[80];
//    time_t rawtime;
//    struct tm* tm;
//
//    putenv( "TZ=EST+5EDT,M3.2.0/2,M11.1.0/2" );
//    tzset();
//    time( &rawtime );
//    outl( rawtime );
//    outnl();
//    tm = localtime( &rawtime );
//    strftime( buffer, 80, "%x - %I:%M%p", tm );
//    printf( "Formatted date & time : |%s|\n", buffer );

//    char* p = strdup( "TZ=EST+5EDT,M3.2.0/2,M11.1.0/2" );
//    char* equal = strchr( p, '=' );
//
//    *equal = '\0';
//
//    int rval = setenv( p, equal + 1, 1 );
//
//    outnln( rval );
//    free( p );
    outn( ch );
    ch = 'b';
    outn( ch );
}

void __attribute__ ((interrupt))
autovector_level1_isr() {
    outln( "interrupt" );
//    TIMER = 0;
}
