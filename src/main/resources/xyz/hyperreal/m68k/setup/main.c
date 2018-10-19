#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <errno.h>
#include "services.h"


double theta = 1.2;

void
main() {
    printf( "sin(%g)^2 + cos(%g)^2 = %g\n", theta, theta, pow(sin(theta), 2) + pow(cos(theta), 2) );
}

void __attribute__ ((interrupt))
autovector_level1_isr() {
}
