(in-package :autowrap)

(defvar *definition-circles* nil
  "Detect circular type members")

 ;; Wrappers

(define-condition invalid-wrapper (error)
  ((object :initarg :object :initform nil :accessor invalid-pointer-object))
  (:report (lambda (c s)
             (with-slots (object) c
               (format s "Error: Invalid/Destroyed Foreign Object: ~A" object)))))

(declaim (inline make-ptr-object ptr-object-ptr))
(defstruct ptr-object
  (ptr (cffi:null-pointer) :type #.(type-of (cffi:null-pointer))))

(declaim (inline make-wrapper wrapper-ptr wrapper-valid-p))
(defstruct (wrapper (:include ptr-object))
  (valid-p t :type boolean))

(declaim (inline make-child-wrapper child-wrapper-ptr child-valid-p))
(defstruct (child-wrapper (:include ptr-object))
  (parent nil :type (or null wrapper)))

(defun child-valid-p (child-wrapper)
  (let ((parent (child-wrapper-parent child-wrapper)))
    (and parent (wrapper-valid-p parent))))

(declaim (inline ptr))
(defun ptr (wrapper)
  (etypecase wrapper
    (#.(type-of (cffi:null-pointer)) wrapper)
    (wrapper
     (if (wrapper-valid-p wrapper)
         (ptr-object-ptr wrapper)
         (error 'invalid-wrapper :object wrapper)))
    (child-wrapper
     (if (child-valid-p wrapper)
         (ptr-object-ptr wrapper)
         (error 'invalid-wrapper :object wrapper)))))

(defun valid-p (wrapper)
  (etypecase wrapper
    (wrapper (wrapper-valid-p wrapper))
    (child-wrapper (child-valid-p wrapper))))

(defun invalidate (wrapper)
  (setf (wrapper-valid-p wrapper) nil)
  (wrapper-ptr wrapper))

(defmethod print-object ((object wrapper) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "{#X~8,'0X}" (cffi:pointer-address (wrapper-ptr object)))))
