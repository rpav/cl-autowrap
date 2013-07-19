typedef struct foo {
  int a, b;
  char c[3];

  struct {
    int a;
    float f;
    int i[4];

    unsigned int b0 : 2, b1 : 3;

    struct {
      char x, y;
    } s;

    struct quux {
      int o, p;
    } *p[2];
  } x[2];
} foo_t;

foo_t* get_foo();
void print_bits(foo_t *foo);
void free_foo(foo_t *foo);

int* get_int();

