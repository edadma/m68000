extern void outc( char c );

char letter = 'a';

void
main() {
    outc( letter + 1 );
    outc( '\n' );
}
