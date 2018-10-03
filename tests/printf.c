#include <stdio.h>
#include <stdlib.h>


int
main() {
    printf( "|%5s| |%5d| |%5x| |%5.1f| %e\n", "asdf", 123, 123, 3.4, atof("3.4") );
    return 0;
}
