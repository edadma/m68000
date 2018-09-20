#include <stdint.h>


extern void printc( char c );
extern void println( char* s );
extern void printn( int n );

//char*
//bin2str( int n, int radix, char* buf ) {
//	char digits[] = "0123456789ABCDEF";
//	char* p = &buf[33];
//	int quo = n;
//
//	if (n < 0)
//		quo = -quo;
//
//	*p-- = 0;
//
//	while (quo >= radix) {
//		*p-- = digits[(quo%radix)];
//		quo /= radix;
//	}
//
//	*p = digits[quo];
//
//	if (n < 0)
//		*--p = '-';
//
//	return p;
//}

uint32_t a = 19;
uint32_t b = 4;

void
main() {
//	static char buf[34];

//    println( bin2str(123, 10, buf) );

    printn( a/b );
    printc( '\n' );
    printn( a%b );
    printc( '\n' );
}
