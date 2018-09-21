#include <stdint.h>


extern void outc( char c );
extern void outln( char* s );
extern void outn( int n );
extern void outf( double n );


double a = 3.4;
double b = 5.6;

void
main() {
    outf( a + 1 );
}
