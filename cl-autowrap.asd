(defpackage :cl-autowrap.asdf
  (:use #:cl #:asdf))

(in-package :cl-autowrap.asdf)

(defsystem :cl-autowrap
  :description "Import c2ffi specs and generate CFFI wrappers"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "1.0"

  :depends-on (:alexandria :cffi :cl-json :cl-ppcre
               :external-program :cl-fad :trivial-features)
  :pathname "autowrap"
  :serial t

  :components
  ((:file "package")
   (:file "util")
   (:file "conditions")
   (:file "c2ffi")
   (:file "wrapper")
   (:file "sffi")
   (:file "parse")
   (:file "bitmask")))
