extern void halt();
extern void outc( char c );


void
print( char* s ) {
	while (*s)
		outc( *s++ );

	outc( '\n' );
}

int
indexOf( char c, char* s ) {
	char* p = s;
	char ch;

	for (int i = 0; ch = *p++; i++)
		if (ch == c)
			return i;

	return -1;
}

int
str2int( char* n, int radix ) {
	char digits[] = "0123456789ABCDEF";
	char* p = n;
	int result = 0;
	char c;
	int neg = 0;

	if (*p == '-') {
		neg = 1;
		p++;
	}

	while (c = *p++) {
		int idx = indexOf( c, digits );

		if (idx == -1 || idx >= radix) {
			print( "invalid number" );
			halt();
		}

		result = result*radix + idx;
	}

	if (neg)
		result = -result;

	return result;
}

void
main() {
	print( str2int( "0", 10 ) == 0 ? "yes" : "no" );
	print( str2int( "123", 10 ) == 123 ? "yes" : "no" );
	print( str2int( "123", 16 ) == 0x123 ? "yes" : "no" );
	print( str2int( "123", 8 ) == 0123 ? "yes" : "no" );
	print( str2int( "123", 10 ) == 123 ? "yes" : "no" );
	print( str2int( "123", 16 ) == 0x123 ? "yes" : "no" );
	print( str2int( "-123", 8 ) == -0123 ? "yes" : "no" );
	print( str2int( "-123", 10 ) == -123 ? "yes" : "no" );
	print( str2int( "-123", 16 ) == -0x123 ? "yes" : "no" );
}