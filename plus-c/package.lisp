(defpackage #:plus-c
  (:use #:cl #:alexandria #:autowrap)
  (:export #:c-let #:c-with #:c-val
           #:c-fun #:c-ref #:& #:*
           #:c-unknown-function #:c-unknown-field))
