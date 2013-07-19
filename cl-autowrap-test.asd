(defpackage :cl-autowrap-test.asdf
  (:use #:cl #:asdf))

(in-package :cl-autowrap-test.asdf)

(defsystem :cl-autowrap-test
  :description "Testing for CL-AUTOWRAP, may require manual work to run"
  :author "Ryan Pavlik"
  :license "LLGPL"
  :version "0.0"

  :depends-on ()
  :pathname "t"
  :serial t

  :components
  ((:static-file "test.c")
   (:static-file "test.h")
   (:file "test")))
