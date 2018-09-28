#include "services.h"


double
bbp( int iterations ) {
	int64_t den = 1;
	double sum = 0;

	for (int k = 0; k <= iterations - 1; k++) {
		double k8 = 8*k;

		sum += 1.0/den*(4/(k8 + 1) - 2/(k8 + 4) - 1/(k8 + 5) - 1/(k8 + 6));
        outf( den*(4/(k8 + 1) - 2/(k8 + 4) - 1/(k8 + 5) - 1/(k8 + 6)) );
        outnl();
		den *= 16;
	}

	return sum;
}

void
main() {
    outf( bbp(10) );
	outnl();
}