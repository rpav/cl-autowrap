# cl-autowrap

This is a new [c2ffi](https://github.com/rpav/c2ffi)-based wrapper
generator for Common Lisp with a focus, performance, convenience, and
completeness.  It works like this:

```lisp
(c-include "file.h")
```

That's it.  This calls `c2ffi` and generates architecture-specific
`.spec` files you can distribute with your project.  **Neither c2ffi,
nor any compiler (or even `.h` files!) are necessary for your users!**

* Types: structs (including bitfields!), unions, enums, typedefs, etc.
* Wrappers: Thin wrappers for structs, unions, and aliases of structs
  and unions: pointers and validation only, no expensive translation
* Accessors: Complete recursive accessors for structs and unions,
  including anonymous struct members, arrays, and pointers for each
  field.
* Functions: Function definitions, with proper type declaration and
  optional inlining
* Metadata: Full access to all the information about all types,
  functions, etc.

For instance:

```c
/* test.h - abbreviated from example */
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

foo_t* get_foo();
void free_foo(foo_t *foo);
int* get_int();
```

Out of this, we can do the following.  (Note: dots are just part of
the function names for disambiguation, this doesn't alter the reader):

```lisp
(c-include "test.h")

(let ((foo (get-foo)))
  (setf (foo-t.a foo) 5)             ;; foo.a = 5;
  (setf (foo-t.x[].b0 foo 0) #b10)   ;; foo.x[0].b0 = 2;
  (print (foo-t.x[].s.x foo 1))      ;; anonymous struct
  (foo-t.x[].s foo 0)                ;; => child wrapper
  (foo-t.x[].s& foo 0)               ;; &(foo.x[0].s) => pointer
  (free-foo foo))
```

## Overview

Using `cl-autowrap` is meant to get you to the "lispifying" stage of
your wrapper as quickly and conveniently as possible:

* Make sure you have c2ffi
* Load your library as you normally would with CFFI
* Make a file for your `c-include`
* Examine wrappers and tweak if necessary
* Back to lisp!

### c2ffi

You will need to build [c2ffi](https://github.com/rpav/c2ffi) if you
have not already done so.  This requires a repository version of LLVM
and Clang, but the build process is straightforward.

Again, note that **your users do not need this**, assuming you
distribute the `.spec` files appropriate to their architecture.
`cl-autowrap` should generate everything for you, though.

### Loading Libraries

This should be done normally with CFFI.  Either the high-level
interface with `CFFI:DEFINE-FOREIGN-LIBRARY` and
`CFFI:USE-FOREIGN-LIBRARY` or the low-level interface with
`CFFI-SYS:%LOAD-FOREIGN-LIBRARY` work.

### Writing the `c-include`

I's highly recommended that you use a separate package *and* file for
`cl-autowrap`.  The reasons are simple:

* A *lot* of symbols will be generated without regard.
* Many symbols will also be exported.
* A rather large number of functions and structs will be generated,
  resulting in a hefty compile time.  With a separate file, this only
  needs to happen once.

Once you have this, you can write a simple `c-include`.  **This must
be a top-level statement**:

```lisp
(c-include "somefile.h")
```

This will generally work if `somefile.h` is in a standard location.
It will also look for and generate `.spec` files in
`*default-pathname-defaults*`, which is probably not very helpful!
To fix this, use the following:

```lisp
(c-include "somefile.h" :spec-path #P"/path/to/spec")
```

(Note that while these parameters *are* `eval`'d, this happens at
*compile time*, so anything you use here probably needs surrounded by
an `EVAL-WHEN`.)

Hardcoded paths aren't very nice though; if we have an ASDF system
called `my-wrapper`, we can do the following:

```lisp
(c-include "somefile.h" :spec-path '(my-wrapper))
```

This also works for the `.h` file; in both cases you can specify a
complete "ASDF path" (starting with the system name), and it'll query
the path from ASDF:

```lisp
(c-include '(my-wrapper some-module "localfile.h")
           :spec-path '(my-wrapper spec-module))
```

Assuming you had defined "localfile.h" as a `:static-file` of
`some-module` in `my-wrapper`, as well as `spec-module`, everything
would work as intended.

### Tweaking

While `c2ffi` and `cl-autowrap` do quite a lot, there are a few times
where you may want to or be required to intervene.  You can look at
any errors that occur, or the symbols that are exported, or even
simply macroexpand the `c-include` and examine the output.

By default, `c2ffi` outputs *everything* and likewise `cl-autowrap`
imports *everything*.  Thus you get a rather large sampling of libc
where you probably don't need it.  Thus you may want to *exclude* some
definitions.  You can do this in two ways:

```lisp
(c-include "file.h"
           :exclude-sources ("/path/to/source1"
                             "/path/.*source2" ...)
           :exclude-definitions ("SomeFunc1"
                                 "_suffix$"))
```

The first, `:exclude-sources`, looks at the source information
generated by `c2ffi` for each definition.  This is an easy way to
exclude the majority of irrelevant definitions.

The second, `:exclude-definitions`, excludes specific definitions.
These may be conflicting or unnecessary.  For instance, SDL2 includes
a number of functions ending in `_inline` and some functions which use
stdargs, all of which are unnecessary (or unsuable).

Both of these use `cl-ppcre` regular expressions to match, thus you
have a great deal of flexibility with a few strings.

You may also wish to simply *rename* some symbols.  The default
routine generally translates symbols like you want, but you may
occasionally find C functions named in a way that breaks this.  The
default rules are as follows:

* `XYZFooBar` => `XYZ-FOO-BAR`
* `foo_barBaz` => `FOO-BAR-BAZ`
* `_x_y` => `_X_Y` (because I think `-X-Y` looks worse)

However if you encounter something like "FOObar", it is likely you
want "FOO-BAR", not "FO-OBAR", which is what you would get.  Thus you
can specify an exception:

```lisp
(c-include "file.h"
           :symbol-exceptions (("FOObar" . "FOO-BAR") ...))
```

These are simple, case-sensitive string matches and replacements.  The
replacement is interned exactly, so if you specify lowercase here, you
will get a symbol with lowercase characters.

Finally, by default all "known" architectures (at the time of writing,
windows, mac, linux on i686 and x86_64) are generated by default.
This may not always work; for instance, one architecture may require
header files your system lacks.  You can exclude it using the
following:

```lisp
(c-include "file.h"
           :exclude-arch ("i686-pc-win32" ...))
```

This will exclude that target triple from being generated and causing
a warning or output if it fails.

## Wrappers and FFI

At this point you probably have definitions generated (or are
hopefully submitting a question or bug report!).  But how to use them?

While `cl-autowrap` uses `CFFI`, it almost exclusively uses the
low-level `CFFI-SYS` interface.  It does not use the high-level type
translation interface, or even `cffi:defcfun`.  Pointers are still
whatever your Lisp provides.

Instead, `cl-autowrap` defines a "new" higher-level interface I call
`SFFI`, for "simplified FFI".  While CFFI's high-level interface is
nice for manually defining types and functions, it proves difficult
when trying to automatically generate things or exercise precise
control over various things like field layout.

You should never have to deal with SFFI directly, but all the
fine-grained type information is available should you require access.
This is occasionally useful.  See below in the SFFI section for
details.

However, you cannot use CFFI constructs from another wrapper directly
with SFFI-defined functions, or vice versa, but you can always use
pointers between the two.

Callbacks via `cffi:defcallback` should work normally.

### Functions

`cl-autowrap` defines functions which wrap C calls with a few helpful
features:

* Wrappers (see below) or pointers are accepted for any
  pointer-to-struct (or union)
* Symbols *or* integers are accepted for any `enum`
* Lisp strings will be temporarily converted to C strings, then freed,
  for `char*` or `unsigned char*`.  (If you need these to persist, you
  must provide your own pointer!)
* For `char*` and `unsigned char*` returns, both a lisp string and a
  pointer are returned as `VALUES`, so you can free the pointer if
  necessary.
* A `DECLAIM` is generated with proper `FTYPE` (except for return
  type) and `INLINE`.  After the function, it is declaimed back to
  `NOTINLINE`, so you may selectively pick which functions are
  inlined.
* Compiler macros are generated for functions as well; this includes
  compile-time determination of enum values.

Otherwise, the call will be like any C call; there is no other type
translation.  In my experience, all but the most trivial C functions
benefit from some wrapping, so this shouldn't be a big issue.

However, see "Other Features" below for some other helpful features,
such as bitmasks.

### Wrappers

Instead of merely returning pointers, `cl-autowrap` defines *very
thin* wrappers for non-atomic named types.  Wrappers are structs which
contain two things:

*  A pointer, which is accessible with `AUTOWRAP:PTR`
*  A `VALID-P` field, which is used for storing pointer validity,
   and can be checked by `AUTOWRAP:VALID-P`

Wrappers are extremely useful for "safely" managing pointers, and are
meant to be safe and "pretty" enough for users of your wrapper to use
directly.  Any dereference using `PTR` automatically checks validity,
and you can use finalizers to clean them up.  Note however that this
is up to you: `cl-autowrap` merely provides the facility, nothing
else.  See "Garbage Collection and Wrappers" below.

Additionally, `cl-autowrap` generates a correct "type hierarchy", as
much as such applies to C:

```c
struct x { ... };
typedef struct x y;
```

Results in:

```lisp
(defstruct (x (:include wrapper)))
(defstruct (y (:include x)))
```

This ensures type compatibility where the C side may arbitrarily
specify compatible type aliases.

There is a second type of wrapper as well, `CHILD-WRAPPER`.  This
differs from `WRAPPER` in that the second field is a reference to the
*parent* wrapper.  You may obtain a child wrapper for a struct which
is a field in another struct, using accessors:

```c
struct foo_t {
   :
   struct { int a, b; } x;
};
```

```lisp
(let* ((foo (get-foo-somehow))
       (x (foo-t.x foo)))
  :
  :
  ... )
```

Child wrappers may also be safely dereferenced using `AUTOWRAP:PTR`,
and checked using `AUTOWRAP:VALID-P`.  In this case, validity is
checked through the parent.  Because a reference is kept to the
parent, even if the reference is discarded by the user, the child is
still safe to use.

### Garbage Collection and Wrappers

One of the primary motivators behind wrappers is the ability to easily
garbage collect C data.  However, this still requires some care.

First, nothing besides checking is done automatically.  Pointers are
assumed valid when they are returned and made into wrappers.  Any
further invalidation and garbage collection must be handled by the one
writing the wrapper.

**Important:** Absolutely no effort is made to keep wrappers unique or
manage duplicates.  Again: **YOU CAN HAVE DUPLICATE WRAPPERS AND THIS
CAN LEAD TO BAD THINGS.**  Generally this should only occur if you
obtain the same pointer from a C API multiple times, such as a
function which returns a global context pointer.  It is up to you to
handle this.  *Beware.*

Once you are aware of this, you can use something like
`trivial-garbage` to free pointers when you need:

```lisp
(defun lispy-get-thing ()
  (let* ((thing (get-thing))
         (ptr (autowrap:ptr (get-thing))))
    (tg:finalize thing (lambda () (free-thing ptr)))
    thing))
```

Note as as always to **never** reference the object, only the pointer,
in the finalizer, or it will never be collected.

It is often useful to free things when you still have a reference.  In
this case, the pointer becomes invalid, and this is also handled by
`WRAPPER`:

```lisp
(defun lispy-free-thing (thing)
  (unwind-protect (free-thing thing)
    (tg:cancel-finalization thing)
    (autowrap:invalidate thing)))
```

In this case, further attempts to dereference `THING` via
`AUTOWRAP:PTR` will result in an `INVALID-WRAPPER` error.

You may be tempted to do this:

```lisp
(defun bad-free-thing (thing)
  (tg:cancel-finalization thing)
  (autowrap:invalidate thing)
  (free-thing thing))
```

Unfortunately, since you invalidated `THING`, when you pass it to
`FREE-THING`, it will be invalid ... resulting in an error.

**Never manage `CHILD-WRAPPER` objects.***  This probably goes without
saying, but they're tied to the parent object, and not meant to be
managed separately.

Also, you may be tempted to do this, to avoid "dangling pointers":

```lisp
(defun terrible-get-thing ()
  (let* ((thing (get-thing))
         (ptr (autowrap:ptr (get-thing))))
    (tg:finalize thing
      (lambda ()
        (free-thing ptr)
        (setf (autowrap:wrapper-ptr thing)
              (cffi:null-pointer))))
    thing))
```

This is both wrong and silly: there is a reference to `THING` in
the finalizer, so it will never get freed.  And if you had gotten here
normally, there would be *no* references, so nothing would have the
dangling pointer!

### Accessors

Having wrappers and functions are nice, but getting at the data is
important too.  Accessors are generated recursively (up to a depth of
5, barring recursive types) for highly convenient access.  From the
top:

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

Accessors are named starting with their type name (in this case, `FOO`
and `FOO-T`), followed by fields, separated by dots.  There is no
reader magic here: these are functions with dots as part of the name.
(Dots were used mostly for disambiguation; if only dashes were used,
name collision would be probable, since underscores are converted to
dashes by default.)

The following special cases are available:

* `type.foo` in the case of `foo` being a record type (struct or
  union), will return a child wrapper.
* `type.foo` in the case of `foo` being a pointer will return the
  pointer
* `type.foo&` will return a *pointer* to the *field*, not a wrapper,
  regardless of the type of `foo`.  If `foo` is a pointer, then you
  get a pointer-pointer.  This is just the same as `&(x.foo)` in C.
  This does not exist for bitfields.
* `type.foo*` will *dereference* `foo` where `foo` is a pointer to a
  well-defined type, including record fields. E.g., `type.foo*.bar`;
  there is no `type.foo.bar`.
* `type.foo[]` references an *array element*, where `foo` is declared
  as an array (not just as a pointer).  Array indices are specified
  in order after the object: `(type.foo[].bar[] obj i0 i1)` is the
  equivalent of C's `obj.foo[i0].bar[i1]`.

Additionally, `SETF` can set almost any field.  The exceptions are any
accessor which dereferences a record (i.e. returns a child wrapper),
or is suffixed with `&`.

Bitfields are supported under the assumption that they are packed
LSB-to-MSB on little endian and MSB-to-LSB on big endian
architectures.  If you *actually encounter* a problem with this, file
a bug report with full details: the architecture, OS, lisp, C
compiler, and an example struct.  Theoretical possibilities are not
considered bugs.

Note that bitfield operations cannot be done atomically and *may* not
be done field-atomically (that is, you *may* have to lock the entire
struct).  Additionally, you cannot take the address of a bitfield.
However, you *can* get information from SFFI metadata, or simply using
the convenience function `AUTOWRAP:BITFIELD-MASK`.

## Other Features

`cl-autowrap` has a number of other features that have not been
discussed:

* Enums
* Bitmasks
* SFFI metadata and functions

### Enums

Enums are imported and created as types, but they're typically used by
specifying a keyword:

```c
enum E {
  FOO_X, FOO_Y, FOO_Z
};

void fun(E);
```

```lisp
(fun :x)
```

As you can see, common prefixes are eliminated and the symbols are
interned as keywords.  Additionally, functions taking enum symbols can
*also* take numbers:

```lisp
(fun 1)
```

You can also find the value or keyword for an enum as follows:

```lisp
(autowrap:foreign-enum-key 'enum-name :key)
(autowrap:foreign-enum-value 'enum-name 1)
```

An actual `AUTOWRAP:FOREIGN-ENUM` can be used in place of `'enum-name`
if desired; otherwise it will be looked up via `AUTOWRAP:FIND-TYPE`.

### Bitmasks

Bitmasks aren't actually a type in C, and are often defined as
constants instead of enums or similar.  Therefore, there is no real
automatic way to determine a bitmask.  Thus `cl-autowrap` provides a
number of convenience facilities for doing this:

```lisp
(autowrap:define-bitmask 'NAME
  '((:key1 . #x0001)
     :
      ...))
```

This defines a bitmask called `NAME`, which is separate from other C
types, and can be used with the `MASK` function:

```
(some-function (autowrap:mask 'NAME :key1 :key5))
```

This also has a compiler macro which will expand to an integer
constant if the value can be determined at compile-time.

Additionally, to aid in converting predefined constants to bitmasks,
there is the following macro, which expands to an
`AUTOWRAP:DEFINE-BITMASK` call:

```lisp
(autowrap:define-bitmask-from-constants (name)
  +some-foo+
  +some-bar+
  +some-baz+)
```

This essentially expands to the following:

```lisp
(autowrap:define-bitmask 'name
  (list `(:foo . ,+some-foo+)
        `(:bar . ,+some-bar+)
        `(:baz . ,+some-baz+)))
```

### SFFI Metadata and Functions

This is not fully-documented at the moment, but full access to
metadata and definition functions is available.  For instance:

```lisp
(autowrap:find-type '(:struct (struct-name)))
```

This will return the object that represents `struct struct_name`, or
nil.  If nothing else, it should be fairly easy to inspect this value
and look at fields, types, etc.  Accessors are exported for all types
(or should be); see `package.lisp` for a complete list.

These values can certainly be useful when doing various tricky things
with C data, and it's also certainly possible to manually write
definitions for every type and generate lisp functions, though for
records (i.e. struct and union), this requires explicitly specifying
bit sizes and field layouts.  While probably not directly useful (or
necessary) for importing C types, these could be useful for generating
similar definitions via other means than `c2ffi`.

## Copying

This is licensed under the [BSD
2-Clause](http://opensource.org/licenses/BSD-2-Clause) license.
