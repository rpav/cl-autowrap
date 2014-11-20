# FAQ

Stuck?  Not sure how to do something?  If this doesn't help, **[file an
issue](https://github.com/rpav/cl-autowrap/issues)**!  Don't feel
guilty.  That's what it's there for.

## I heard this had a problem with C preprocessor macros!

That's news to me.  This will *not* automagically transform CPP macros
into runnable lisp functions (or even attempt to), but *nothing else
will either*.  This is impossible in the general case, and in my
experience unnecessary.

It *will* handle preprocessor **constants**, *including* expressions,
which other systems fail to handle.  This is all taken care of by
[c2ffi](https://github.com/rpav/c2ffi); if you have an issue, please
file one there!

## I'm looking for an example...

[ZMQ4L](https://github.com/rpav/ZMQ4L) is a great example of using
basically every facility autowrap has.  The ZMQ API is quite simple
and easy to follow, and you don't have to use or understand it
in-depth to see how autowrap handles it.

For an example of renaming lots of constants, [my CL-LLVM
fork](https://github.com/rpav/CL-LLVM/blob/master/src/autowrap.lisp)
demonstrates renaming a sizable API.

## I tried PLUS-C and I can't take the address with `&`

You need to use the symbol `PLUS-C:&`.  I recommend importing this
directly, if nothing else:

```lisp
(defpackage :my-package
  (:use #:cl ...)
  (:import-from plus-c #:&))
```

(Why?  Unfortunately, this is not a standard symbol exported from
`COMMON-LISP`, like `*`.)

## It won't find *&lt;foo.h&gt;* / a bunch of definitions are excluded!

When c2ffi runs, it makes a basic guess as to where headers live,
since there is no API for this.  This means that some system headers,
usually internal to GCC or glibc or similar, may not be found.

Specify paths to these with `:sysinclude ("/path/...")` to
`c-include`:

```lisp
(c-include "..."
   :sysincludes ("/usr/lib/gcc/x86_64-pc-linux-gnu/4.8.2/include")
   ...)
```

If you need to quickly test, rather than deleting `.spec` files and
reevaluating the `c-include`, simply run c2ffi yourself with the
appropriate `-i` options:

```console
$ c2ffi -i /usr/local/include/... -i /usr/lib/gcc/... myinput.h
...
```

(If you wrap glib-based things in particular, it can require quite a
list of these.)

Similarly, complaints about "N definitions excluded" are caused by
prior types not being understood.  Either they were not included,
because a header could not be found, or you excluded them with one of
the `:exclude` options.

## I tried to find *type* and it doesn't exist, what gives?

Types are called as follows for disambiguation:

* Basic types: `:char`, `:int`, `:long`, etc., and `:pointer`
* Structs: `struct NAME { ... };` &rarr; `(:struct (NAME))`
* Unions: `union NAME { ... };` &rarr; `(:union (NAME))`
* Enums: `enum NAME { ... };` &rarr; `(:enum (NAME))`
* Aliases, i.e. typedefs: `typedef ANYTHING TYPE-NAME` &rarr; `TYPE-NAME`

## So I have a bitmask...

If you're looking to define a bitmask, see "...bitmasks?" below.  If
you already *have* one, you probably want to use it.

Bitmasks are not C constructs; C does not have a concept of a
"bitmask".  Bitmasks are often specified in C libraries as consts,
enums, or `#define` macros.  Autowrap lets you bundle these into a
named bitmask construct.

For instance, [SDL2 has a
bitmask](https://github.com/lispgames/cl-sdl2/blob/master/src/sdl2.lisp#L30)
for initialization based on `#define` macros.  To get a *value*, you
can use `MASK`:

```lisp
(autowrap:mask 'sdl-init-flags :timer :video :joystick)
;; => 545
```

Of course, most of the time you'll be getting a list of values from a
user, so you probably want `MASK-APPLY`:

```lisp
(defun init (&rest flags)
  (sdl-init (mask-apply 'sdl-init-flags flags)))
```

If you want to *reverse* this and see what flags apply to a *value*:

```lisp
(autowrap:mask-keywords 'sdl-init-flags 545)
;; => (:TIMER :VIDEO :JOYSTICK)
```

Note this returns only *the first* matching keyword for each bit, and
only flags that match *exactly*:

```lisp
(autowrap:mask-keywords 'sdl-init-flags
                        (mask 'sdl-init-flags :everything))
;; => (:TIMER :AUDIO :VIDEO :JOYSTICK :HAPTIC :GAMECONTROLLER)
```

## There are reports about definitions excluded, and my users think it's a bug

Once you get done wrapping things and you're sure it works, the
messages about definitions excluded etc may cause your users to
panic.  For this, there is the `:release` option:

```lisp
(c-include "..."
   :release t)
```

Messages will not be output in this case.

## Can this handle...

### ...callbacks from C?

Yes, and autowrap provides a thin layer around CFFI-SYS's
`%defcallback` which lets you specify autowrap-generated types,
and exports `callback` for convenience:

```lisp
(autowrap:defcallback my-callback :int
    ((foo :int) (bar some-type) ...)
  ...code...)

;; Later...

(some-c-function ... (autowrap:callback my-callback) ...)
```

### ...some funky constants like...

Yes.  There are simple renaming facilities, regexp renaming, and the
ability to write functions to process names.  See "Tweaking" in the
README for details.

File an issue if you encountered some weird naming scheme and don't
know how to approach it.

### ...passing and returning structs by value?

Not yet!  But, if you have this issue, let me know and I will
prioritize it.

### ...struct bitfields?

Yes!  Just reference them like any other field.  You just can't take
their address.

### ...enums?

Yes.  If a function is declared to take an enum, you can simply pass a
symbol representing it:

```c
enum X { A, B, C };

void foo(enum X);
```

```lisp
(foo :a)
```

You can also just pass a number.  It's *not* checked for validity:

```lisp
(foo 42)
```

Unfortunately, not all C authors are particularly strict about
declaring enums.  I see stuff like this a lot:

```c
enum X { A, B, C };

void foo(int x);  /* See enum X */
```

In this case, you can look up the values yourself:

```lisp
(foo (autowrap:enum-value '(:enum (x)) :a))
```

Sometimes C authors don't even bother to make enums, which is pretty
annoying too:

```c
#define A 1
#define B 2
#define C 3
```

If you need to, you can make one yourself:

```lisp
;; Strip common prefix/suffix, find values, and add our own:
(autowrap:define-enum-from-constants (X)
  +A+
  +B+
  +C+
  (:d 9999))

(autowrap:enum-value '(:enum (x)) :b) ;; => 2
```

If you need a *bitmask*, though, see below.

### ...bitmasks?

Yes!  Bitmasks are not a fundamental concept in C, like enums or
structs, so you will have a bit of manual definition to do.  Autowrap
however provides some convenience facilities.

Do you have an enum?

```lisp
(autowrap:define-bitmask-from-enum MY-ENUM-NAME)
```

Do you have a bunch of constants?

```lisp
;; This strips prefix/suffix automatically; if this strips too much,
;; use REGEX to specify your own match, e.g. "FOO_\(.*?\)_BAR"

(autowrap:define-bitmask-from-constants (MY-BITMASK-NAME &optional REGEX)
  +SOME-CONST+ +ANOTHER-CONST+ ...)
```

Do you just want to specify your own values?

```lisp
(autowrap:define-bitmask 'my-bitmask
  '((:x . 1)
    (:y . 2) ...))
```

For a "real world" example, [see SDL2
source](https://github.com/lispgames/cl-sdl2/blob/master/src/sdl2.lisp#L30),
there are quite a few.

For accessing values, see "So I have a bitmask..." above.
