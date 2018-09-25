extern void outc( char c );


void
print( char* s ) {
	while (*s)
		outc( *s++ );

	outc( '\n' );
}

char*
bin2str( int n, int radix, char* buf ) {
	char digits[] = "0123456789ABCDEF";
	char* p = &buf[33];
	int quo = n;

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

char*
armstrong( long n ) {
	static char buf[34];
	char* s = bin2str( n, 10, buf );
	long sum = 0;

	for (char* p = s; *p;) {
		long d = *p++ - '0';

		sum += d*d*d;
	}

	return sum == n ? s : (char*) 0;
}

void
main() {
	for (int n = 0; n <= 999; n++) {
		char* p = armstrong( n );

		if (p)
			print( p );
	}
}