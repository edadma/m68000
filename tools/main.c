extern void outc( char c );
extern void outs( char* s );
extern void outn( int n );


signed char
isNegative( signed char n ) {
	return (signed char)((unsigned char) n >> 7);
}

void
main() {
	outn( isNegative(0) );
	outs( ", " );
	outn( isNegative(-5) );
	outc( '\n' );
}