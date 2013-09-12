(defpackage :autowrap
  (:use #:cl #:alexandria)
  (:export
   ;; Conditions
   #:invalid-wrapper

   ;; Structs
   #:wrapper

   ;; Functions
   #:make-wrapper #:wrapped-ptr #:wrapper-valid-p
   #:ptr #:valid-p #:invalidate
   #:wrap-pointer

   #:parse #:parse-file

   ;; SFFI
   #:foreign-type #:foreign-type-name

   #:foreign-record #:foreign-record-bit-size #:foreign-record-bit-alignment
   #:foreign-record-fields

   #:foreign-field
   #:foreign-record-field #:frf-bitfield-p #:frf-bit-offset #:frf-bit-alignment
   #:frf-bit-size

   #:foreign-enum #:foreign-enum-values
   #:enum-value #:enum-key

   #:foreign-symbol #:foreign-symbol-c-symbol
   #:foreign-extern

   #:foreign-function #:foreign-function-variadic-p

   #:basic-foreign-type #:foreign-scalar-p #:foreign-type-size
   #:find-type #:ensure-type #:find-function #:find-extern

   #:define-foreign-type #:define-foreign-extern #:define-foreign-alias
   #:define-foreign-function

   #:bitfield-mask
   #:define-cfun #:define-cextern
   #:define-accessors #:define-wrapper

   #:alloc-ptr #:alloc #:free #:with-alloc #:with-many-alloc
   #:c-aptr #:c-aref

   ;; Bitmasks
   #:define-bitmask #:find-bitmask #:remove-bitmask #:mask-symbol-value
   #:mask #:mask-apply #:mask-keywords
   #:define-bitmask-from-constants #:define-bitmask-from-enum

   ;; Parsing and input
   #:c-include

   ;; Utility
   #:asdf-path))

(defpackage :autowrap.minimal
  (:documentation "A minimal set of useful symbols for doing common things with autowrap.")
  (:import-from
   :autowrap

   #:ptr #:invalidate #:enum-value #:enum-key #:mask #:mask-apply #:mask-keywords
   #:alloc-ptr #:alloc #:free #:with-alloc #:with-many-alloc)
  (:export #:ptr #:invalidate #:enum-value #:enum-key #:mask #:mask-apply #:mask-keywords
           #:alloc-ptr #:alloc #:with-alloc #:with-many-alloc))
