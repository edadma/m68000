#include <stdint.h>


extern void outc( char c );


void
print( char* s ) {
	while (*s)
		outc( *s++ );
}

void
println() {
	outc( '\n' );
}

char*
bin2str( int64_t n, int radix, char* buf ) {
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
printn( int64_t n ) {
	char buf[34];
	char* s = bin2str( n, 16, buf );

	print( s );
}

int64_t
myabs( int64_t x ) {
  const int64_t bit31 = x >> 63;

  return (x ^ bit31) - bit31;
}

int64_t
modifyBit( int64_t x, unsigned char position, int64_t newState ) {
  int64_t mask = 1l << position;
  int64_t state = newState;

  return (x & ~mask) | (-state & mask);
}

int64_t
flipBit( int64_t x, unsigned char position ) {
  int64_t mask = 1l << position;

  return x ^ mask;
}

int64_t
isNegative( int64_t n ) {
	return (int64_t)((uint64_t) n >> 63);
}

int
bit( int64_t x, int n ) {
	return (x >> n)&1;
}

void
main() {
	printn( myabs(5) );
	print( ", " );
	printn( myabs(0) );
	print( ", " );
	printn( myabs(-5) );
	println();

	printn( modifyBit(0, 5, 0) );//0
	print( ", " );
	printn( modifyBit(0, 5, 1) );//0x20
	print( ", " );
	printn( modifyBit(0x77, 5, 0) );//0x57
	print( ", " );
	printn( modifyBit(0x77, 5, 1) );//0x77
	println();

	printn( flipBit(0, 5) );//20
	print( ", " );
	printn( flipBit(0x77, 5) );//57
	println();

	printn( isNegative(0) );
	print( ", " );
	printn( isNegative(-5) );
	println();

	printn( bit(5, 2) );
	print( ", " );
	printn( bit(5, 1) );
	println();
}