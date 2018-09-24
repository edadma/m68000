#include <stdint.h>


extern void outc( char c );
extern void outln( char* s );
extern void outn( int n );
extern void outu( unsigned int n );
extern void outx( unsigned int n );
extern void outf( double n );


//char*
//int2stru( unsigned int n, int radix, char* buf ) {
//	char digits[] = "0123456789ABCDEF";
//	char* p = &buf[33];
//	unsigned int quo = n;
//
//	*p-- = '\0';
//
//	while (quo >= radix) {
//		*p-- = digits[(quo%radix)];
//		quo /= radix;
//	}
//
//	*p = digits[quo];
//
//	return p;
//}

unsigned int n = 0x80000;
unsigned int d = 16;
//char buf[50];

void
main() {

//    outln( int2stru(n, 16, buf) );
    outx( n/d );
    outc( '\n' );
    outx( n%d );
    outc( '\n' );

}