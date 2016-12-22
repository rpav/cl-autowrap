(defpackage+-1:defpackage+ :autowrap
  (:use #:plus-c)
  (:import-except-conflicts #:autowrap.libffi))

 ;; Variables

(in-package :autowrap)

(defvar *libffi-cif* (make-hash-table)
  "Cache function CIFs")

(defvar *libffi-type-map* (make-hash-table)
  "Map autowrap types to ffi_type")

(defvar *libffi-sigs* (make-hash-table :test 'equal)
  "Cached function signatures")
