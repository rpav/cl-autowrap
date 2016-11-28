(in-package :autowrap)

(defvar *libffi-cif* (make-hash-table))
(defvar *libffi-types* (make-hash-table))

(defvar *libffi-type-map* nil)

;;; Repurposed from alloc.lisp ;)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *libffi-type-map* nil)
  (let (unsigned signed)
    (map nil
         (lambda (x)
           (push (cons (foreign-type-size x) x) unsigned))
         '(:unsigned-char :unsigned-short :unsigned-int :unsigned-long :unsigned-long-long))
    (map nil
         (lambda (x)
           (push (cons (foreign-type-size x) x) signed))
         '(:char :short :int :long :long-long))
    (push (cons :pointer ffi-type-pointer) *libffi-type-map*)
    (push (cons :void ffi-type-void) *libffi-type-map*)
    (push (cons (aval 1 signed) ffi-type-sint8) *libffi-type-map*)
    (push (cons (aval 1 unsigned) ffi-type-uint8) *libffi-type-map*)
    (push (cons (aval 2 signed) ffi-type-sint16) *libffi-type-map*)
    (push (cons (aval 2 unsigned) ffi-type-uint16) *libffi-type-map*)
    (push (cons (aval 4 signed) ffi-type-sint32) *libffi-type-map*)
    (push (cons (aval 4 unsigned) ffi-type-uint32) *libffi-type-map*)
    (push (cons (aval 8 signed) ffi-type-sint64) *libffi-type-map*)
    (push (cons (aval 8 unsigned) ffi-type-uint64) *libffi-type-map*)
    (nreversef *libffi-type-map*)))

(defgeneric ensure-libffi-type (foreign-type)
  (:documentation "Find or create a libffi equivalent of `FOREIGN-TYPE`."))

(defmethod ensure-libffi-type ((ft symbol))
  (when (keywordp ft)
    (aval ft *libffi-type-map*)))

