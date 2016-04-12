(defpackage :autowrap
  (:use #:cl #:alexandria)
  (:export
   ;; Conditions
   #:autowrap-condition
   #:autowrap-error
   #:autowrap-continuable-error
   #:simple-autowrap-continuable-error
   #:undefined-foreign-type
   #:undefined-foreign-type-contextualised

   #:sffi-condition
   #:sffi-error
   #:sffi-continuable-error
   #:simple-sffi-continuable-error
   #:invalid-wrapper

   ;; Wrapper
   #:wrapper #:anonymous-type #:make-anonymous-type

   #:make-wrapper #:wrapped-ptr #:wrapper-valid-p
   #:ptr #:valid-p #:invalidate
   #:wrap-pointer #:wrapper-null-p

   #:make-wrapper-instance

   #:autocollect #:autocollect-cancel #:with-autocollect-cancel
   #:making-autocollect-instance

   ;; Autowrap itself
   #:parse #:parse-file

   #:*foreign-type-symbol-function*
   #:*foreign-c-to-lisp-function*
   #:default-foreign-type-symbol
   #:default-c-to-lisp

   ;; SFFI
   #:foreign-type #:foreign-type-name #:basic-foreign-type

   #:foreign-record #:foreign-record-bit-size #:foreign-record-bit-alignment
   #:foreign-record-fields #:find-record-field

   #:foreign-pointer #:foreign-alias #:foreign-array #:foreign-array-size
   #:foreign-string

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
   #:unaliased-type #:builtin-type-p

   #:define-foreign-alias
   #:define-foreign-enum
   #:define-foreign-extern
   #:define-foreign-function
   #:define-foreign-record
   #:define-foreign-type

   #:define-enum-from-constants

   #:bitfield-mask
   #:define-cfun #:define-cextern
   #:define-accessors #:define-wrapper #:define-wrapper*

   #:inhibit-string-conversion

   #:alloc-ptr #:alloc #:free #:with-alloc #:with-many-alloc
   #:c-aptr #:c-aref #:sizeof

   #:defcallback #:callback

   #:get-errno-pointer #:errno

   ;; Bitmasks
   #:define-bitmask #:find-bitmask #:remove-bitmask #:mask-symbol-value
   #:mask #:mask-apply #:mask-keywords
   #:define-bitmask-from-constants #:define-bitmask-from-enum

   ;; Parsing and input
   #:c-include #:*c2ffi-program*

   ;; Debug
   #:*trace-c2ffi*

   ;; Utility
   #:asdf-path
   #:string+))

(defpackage :autowrap.minimal
  (:documentation "A minimal set of useful symbols for doing common things with autowrap.")
  (:use)
  (:import-from
   :autowrap

   #:ptr #:invalidate #:enum-value #:enum-key #:mask #:mask-apply #:mask-keywords
   #:alloc-ptr #:alloc #:free #:with-alloc #:with-many-alloc #:defcallback #:callback
   #:inhibit-string-conversion #:autocollect #:wrapper-null-p #:sizeof
   #:autocollect-cancel #:with-autocollect-cancel #:making-autocollect-instance)
  (:export #:ptr #:invalidate #:enum-value #:enum-key #:mask #:mask-apply #:mask-keywords
           #:alloc-ptr #:alloc #:with-alloc #:with-many-alloc #:free #:defcallback #:callback
           #:inhibit-string-conversion #:autocollect #:wrapper-null-p #:sizeof
           #:autocollect-cancel #:with-autocollect-cancel
           #:making-autocollect-instance))
