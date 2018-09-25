#include <stdint.h>

extern void outc( char c );
extern void outs( char* s );
extern void outn( int n );


int64_t
myabs( int64_t x ) {
  const int64_t bit31 = x >> 63;

  return (x ^ bit31) - bit31;
}

void
main() {
	outn( myabs(5) );
	outs( ", " );
	outn( myabs(0) );
	outs( ", " );
	outn( myabs(-5) );
	outc( '\n' );
}