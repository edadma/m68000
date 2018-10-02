#include <stdint.h>

extern void outnl();
extern void outc( char c );
extern void outf( double a );
extern void outs( char* s );
extern void outln( char* s );
extern void outn( int n );
extern void outl( int64_t n );
extern void halt() __attribute__ ((noreturn));
