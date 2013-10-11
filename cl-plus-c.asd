(defpackage :cl-autowrap.asdf
  (:use #:cl #:asdf))

(in-package :cl-autowrap.asdf)

(defsystem :cl-plus-c
  :description "Convenience and alternative mechanic for C/autowrap"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "1.0"

  :depends-on (:cl-autowrap)
  :pathname "plus-c"
  :serial t

  :components
  ((:file "package")
   (:file "conditions")
   (:file "plus-c")))
