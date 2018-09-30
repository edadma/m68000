#include <stdio.h>
#include <stdlib.h>
#include "services.h"


typedef struct node {
    int val;
    struct node * next;
} node_t;


void
print_list( node_t * head ) {
    node_t * current = head;

    while (current != NULL) {
        outn( (int)current );
        outs( ": " );
        outn( current->val );
        outnl();
        current = current->next;
    }
}

void
main() {
    node_t * head = NULL;

    head = malloc(sizeof(node_t));
    head->val = 1;
    head->next = NULL;

    head->next = malloc(sizeof(node_t));
    head->next->val = 2;
    head->next->next = NULL;

    print_list( head );
}
