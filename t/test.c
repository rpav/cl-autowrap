/* Quick compile with gcc:
 *
 * $ gcc -fPIC test.c -shared -Wl,-soname,libtest.so -o libtest.so
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "test.h"

static int int_ernal = 42;

int* get_int() { return &int_ernal; }

foo_t* get_foo() {
  foo_t *foo = calloc(1, sizeof(foo_t));

  foo->a = 1;
  foo->x[0].a = 42;
  foo->x[1].f = 19.5;

  foo->x[0].b0 = 2;
  foo->x[0].b1 = 5;

  foo->x[0].p[0] = calloc(1, sizeof(struct quux));
  foo->x[0].p[1] = calloc(1, sizeof(struct quux));

  return foo;
}

void print_bits(foo_t *foo) {
  printf("b0 = %d, b1 = %d\n", foo->x[0].b0, foo->x[0].b1);
}

void free_foo(foo_t *foo) {
  if(!foo) {
    printf("Trying to free_foo NULL!\n");
    return;
  }

  free(foo->x[0].p[0]);
  free(foo->x[0].p[1]);
  free(foo);
}
