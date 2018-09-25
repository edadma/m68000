#include "services.h"

//extern uint64_t __udivmoddi4 	( 	uint64_t  	num,
//       		uint64_t  	den,
//       		uint64_t *  	rem
//       	);

//char*
//bin2str( int64_t n, int radix, char* buf ) {
//	char digits[] = "0123456789ABCDEF";
//	char* p = &buf[33];
//	int64_t quo = n;
//
//	if (n < 0)
//		quo = -quo;
//
//	*p-- = 0;
//
//	while (quo >= radix) {
//		*p-- = digits[(quo%radix)];
//		quo /= radix;
//	}
//
//	*p = digits[quo];
//
//	if (n < 0)
//		*--p = '-';
//
//	return p;
//}

uint64_t num = 19;
uint64_t den = 5;

void
main() {
//    static char buf[65];

//	outs( bin2str(20, 10, buf) );

    outl( num/den );
	outc( '\n' );
}