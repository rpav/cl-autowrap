(defpackage :autowrap.test
  (:use #:cl #:autowrap))

(in-package :autowrap.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi-sys:%load-foreign-library
   :libtest (merge-pathnames "libtest.so" (asdf-path 'cl-autowrap-test))))

(c-include '(cl-autowrap-test "test.h")
           :spec-path '(cl-autowrap-test))

(let ((foo (get-foo)))
  (:say foo)
  (:say (foo-t.a foo))
  (:say (foo-t.x[].a foo 0))
  (:say (foo-t.x[].f foo 1))
  (:say (foo-t.x[].b0 foo 0))
  (:say (foo-t.x[].b1 foo 0))
  (:say (cffi-sys:%mem-ref (ptr foo) :unsigned-char (+ 16 24)))
  (setf (foo-t.x[].b0 foo 0) #b01)
  (setf (foo-t.x[].b1 foo 0) #b110)
  (print-bits foo)
  (free-foo foo))
