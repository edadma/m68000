#include "../tools/services.h"


void
print( char* s ) {
	while (*s)
		outc( *s++ );
}

void
println( char* s ) {
	print( s );
	outc( '\n' );
}

char*
int2str64( int64_t n, int radix, char* buf ) {
	char digits[] = "0123456789ABCDEF";
	char* p = &buf[33];
	int64_t quo = n;

	if (n < 0)
		quo = -quo;

	*p-- = 0;

	while (quo >= radix) {
		*p-- = digits[(quo%radix)];
		quo /= radix;
	}

	*p = digits[quo];

	if (n < 0)
		*--p = '-';

	return p;
}

void
main() {
	char buf[34];

	println( int2str64(0, 10, buf) );
	println( int2str64(123, 10, buf) );
	println( int2str64(0x12AB, 16, buf) );
	println( int2str64(2000000000, 10, buf) );
	println( int2str64(20000000000, 10, buf) );
	println( int2str64(-123, 10, buf) );
	println( int2str64(-0x12AB, 16, buf) );
	println( int2str64(-2000000000, 10, buf) );
	println( int2str64(-20000000000, 10, buf) );
}