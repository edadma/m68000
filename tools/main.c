#include "services.h"

//extern uint64_t __udivmoddi4 	( 	uint64_t  	num,
//       		uint64_t  	den,
//       		uint64_t *  	rem
//       	);

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

//double a = 3.4;
//double b = 5.6;

void
main() {
    static char buf[65];

	outs( bin2str(-5, 10, buf) );
	outnl();
}