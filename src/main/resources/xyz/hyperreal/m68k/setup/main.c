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
    printf( "sin(5)^2 + cos(5)^2 = %g\n", pow(sin(theta), 2) + pow(cos(theta), 2) );  // should be 1, of course
}

void __attribute__ ((interrupt))
autovector_level1_isr() {
}
