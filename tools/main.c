extern void outc( char c );
extern void outs( char* s );
extern void outn( int n );


short
myabs( short x ) {
  const short bit31 = x >> 15;

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