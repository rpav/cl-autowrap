(defpackage :autowrap
  (:use #:cl #:alexandria)
  (:export
   ;; Conditions
   #:invalid-wrapper

   ;; Structs
   #:ptr-object #:wrapper #:child-wrapper

   ;; Functions
   #:make-wrapper #:wrapped-ptr #:wrapper-valid-p
   #:make-child-wrapper #:child-wrapper-ptr #:child-valid-p
   #:child-wrapper-parent

   #:ptr #:valid-p #:invalidate

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

   #:alloc-ptr #:alloc #:with-alloc

   ;; Bitmasks
   #:define-bitmask #:find-bitmask #:remove-bitmask #:mask-symbol-value
   #:mask #:mask-apply #:mask-keywords
   #:define-bitmask-from-constants #:define-bitmask-from-enum

   ;; Parsing and input
   #:c-include

   ;; Utility
   #:asdf-path))
