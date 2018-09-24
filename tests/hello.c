extern void outc( char c );

void
main() {
	for (char* p = "Hello world!\n"; *p;)
		outc( *p++ );
}