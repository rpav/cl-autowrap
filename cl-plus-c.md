# cl-plus-c

Normally, `cl-autowrap` will import and create functions and macros
for C functions and record accessors.  This may be fine for relatively
small libraries, but as imports become larger, this can balloon into
thousands and thousands of definitions, which can take a very long
time to generate, and a considerable amount of space.

`cl-plus-c` provides an alternate mechanism, and can be used in
conjunction with the following new parameters to `c-include`:

```lisp
(c-include "file.h"
           :no-accessors t :no-functions t)
```

Note that neither of these are *required* for `cl-plus-c`, but they do
disable the generation of accessor functions and macros, which can
significantly speed up compile times.  These are unnecessary with
`cl-plus-c`, though they may be used in conjunction.

The following new forms are provided to allow access to functions,
accessors, and allocation:

* `c-fun`: Allows calling functions by symbol rather than as a macro
* `c-ref`: Allows path-like dereferencing of types
* `c-let`/`c-with`: Allows simple allocation and convenient access of "variables"

## Functions

Normally in `cl-autowrap`, one can simply call C functions by their
translated symbol name:

```lisp
(foo x y)
```

With `cl-plus-c`, one may call this by name, even if the macro above
is not defined:

```lisp
(c-fun foo x y)
```

This generates essentially identical code.

## Accessors

Instead of generating potentially thousands or tens of thousands of
recursive accessor functions, `cl-plus-c` will allow accessors to be
generated on demand at compile time based on a path:

```lisp
(c-ref OBJECT C-TYPE &rest FIELDS...)
```

For instance, given the following struct:

```c
typedef struct foo {
  int a, b;
  char c[3];

  struct {
    unsigned int b0 : 2, b1 : 3;

    struct {
      char x, y;
    } s;
  } x[2];
} foo_t;
```

We may access the various fields as follows:

```lisp
(c-ref object foo-t :a)              ;; => object.a
(c-ref object foo-t :c 2)            ;; => object.c[2]
(c-ref object foo-t :x 2 :b0)        ;; => object.x[2].b0
(c-ref object foo-t :x :b0)          ;; => object.x[0].b0
(c-ref object foo-t :c *)            ;; => *(object.c)
(c-ref object foo-t :x 1 :s :x &)    ;; => &(object.x[1].s.x)
```

`SETF` can also be applied to these forms with some exceptions:

* `&` at the end of the form means "return the address of", so you
  can't set that
* `string` at the end of a form means "convert this to a lisp string",
  and can't be set
* You may not set the value of a record directly

**Safety note:** There isn't much safety here!  Nothing stops you from
referencing objects out-of-bounds, setting invalid pointers, etc.
This is not considerably different than `cl-autowrap` accessors, or
simply referencing things in C.  Be careful!

## Pointers and Values

One big goal for `cl-plus-c` is for pointers-vs-values to be clear:
how to get each, and when you need each.  This is actually implemented
so that each are *more* consistent than C, but it's been pointed out
this isn't necessarily intuitive from the above.

The main question is "how do I get different things in `cl-plus-c`,
and when do I use `&`?".  The answer is that you *always* get the
*value* by default; if you want the *address* of the value, you use
`&`.

Imagine the following:

```c
struct a {
    char c0;
    char c1;
};

struct b {
    int x;
    int y;
};

struct c {
    int i;
    char c[5];

    struct a *s;
    struct b t;
} c;
```

The basic cases:

* If you want the *value* of `c.i`, then you use `(c i)`.  If you want
  the *address* `&(c.i)`, then you use `(c i &)`.
* If you want the *value* of `c.s->c0`, then you use `(c s c0)`.  If
  you want the *address* `&(c.s->c0)`, you use `(c s c0 &)`.  This
  consists of some pointer math and dereferences along the way, but it
  still gives you the *address* of the last-named field, much like C.
* If you want the *value* of `c.s`, this *value* is a *pointer*.  You
  say `c.s` in C, and you get a pointer; likewise you say `(c s)` and
  you get a pointer (or a wrapper, in this case).

Now for some "odd" cases:

* Note that in `struct c`, the `char c[5]` field is *not* a pointer; 5
  chars are *inside* the struct.  But, if you say `c.c` in C, this
  gives you a pointer!  In `cl-plus-c`, saying `(c c)` gives you the
  *value*, the equivalent of `c.c[0]`.  If you want the location of
  the struct field, you say `(c c &)`, just like *every other* case.
* You cannot take the value of a record.  Thus, unlike saying `(c s)`,
  which results in a pointer (wrapper), saying `(c t)` is an error.
  But if you want the location, you *still* say `(c t &)`.
* There is no ambiguity between `.` and `->`, and whether the field is
  a pointer or value; thus dereferencing a field uses identical syntax.


## Allocation

For ultimate convenience, `cl-plus-c` defines `c-let`/`c-with`, which
will let you define, set, and access values somewhat like C:

```lisp
(c-with ((x :int))                ;; same as c-let with :free T
  (print x)                       ;; => whatever value was in memory
  (setf x 42)
  (print (x &))                   ;; => &x (pointer)
  (print x))                      ;; => 42
```

You may specify one or more bindings, which are in the following form:

```lisp
(NAME C-TYPE &key (count 1) free ptr from value calloc)
```

* `count`: By default, 1 item is allocated; you may make an array by
  specifying more
* `free`: `C-LET` will *not* free the memory by default, `C-WITH`
  will.  Specifying `free` to be explicit (e.g., per-item).
* `ptr` is mutually exclusive with `alloc` and `free`.  Nothing will
  be allocated or freed; instead, the pointer specified will be used.
  This is useful when pointers from foreign code are acquired.
* `from` is similar to `ptr` (and should not be used with `ptr`),
  except it takes an existing *wrapper* object and provides bindings
  as above.
* `value` sets the initial value.  This may not be used with `:from`
  or `:ptr`.
* `calloc`: if `T`, uses `CALLOC` to allocate, zeroing the memory
  allocated.  `NIL` by default.

Inside the block:

* `NAME` will reference the *value*; if this is a record, it will be a
  wrapper, if this is a basic type, it will be the value of the type
  (such as an integer)
* `(setf NAME VALUE)` may be used, as above, to set the *contents* of
  the variable, much like C
* `(NAME ...)` is equivalent to `(c-ref NAME C-TYPE ...)`, so you do
  not have to manually specify this every time
* `(setf (ref :string-field) "STRING")` will allocate, copy, and
  assign a string pointer to the field.  This must be freed manually.
  Due to the semantics of plus-c, the easiest way to do this is `(free
  (ref :string-field * &))`, which implicitly avoids conversion back
  to a lisp string, and returns the pointer directly.

## c-val

The `c-val` macro is a convenience feature for "rebinding" an existing
wrapper.  You could write the following:

```lisp
(defun some-fun (val)
  (c-let ((val val-type :from val))
    ...))
```

But with `c-val` you can simply write the following:

```lisp
(defun some-fun (val)
  (c-val ((val val-type))
    ...))

;;; Or:

(defun some-fun (...)
  (c-val ((val val-type (get-a-val-form ...)))
    ...))
```

As an added feature, either form uses `(the (or null TYPE) FORM)` to
declare the value of `FORM`.
