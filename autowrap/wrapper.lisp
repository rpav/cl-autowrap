(in-package :autowrap)

(defvar *definition-circles* nil
  "Detect circular type members")

 ;; Wrappers

(declaim (inline make-wrapper wrapper-ptr))
(defstruct wrapper
  #+(or cmucl ecl sbcl clisp)
  (ptr (cffi:null-pointer) :type #.(type-of (cffi:null-pointer)))
  #+(or ccl allegro)
  (ptr #.(cffi:null-pointer) :type #.(type-of (cffi:null-pointer)))
  (validity t))

(defstruct (anonymous-type (:include wrapper)))

(defun wrapper-valid-p (wrapper)
  (let ((v (wrapper-validity wrapper)))
    (etypecase v
      (wrapper (wrapper-valid-p v))
      (t v))))

(declaim (inline ptr valid-p))
(defun ptr (wrapper)
  (etypecase wrapper
    (#.(type-of (cffi:null-pointer)) wrapper)
    (wrapper
     (if (wrapper-valid-p wrapper)
         (wrapper-ptr wrapper)
         (error 'invalid-wrapper :object wrapper)))
    (null (cffi:null-pointer))))

(defun valid-p (wrapper)
  (wrapper-valid-p wrapper))

(defun invalidate (wrapper)
  (setf (wrapper-validity wrapper) nil)
  (wrapper-ptr wrapper))

(defmethod print-object ((object wrapper) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "{#X~8,'0X}" (cffi:pointer-address (wrapper-ptr object)))))

(defun wrap-pointer (pointer type &optional (validity t))
  (let ((child (make-instance type)))
    (setf (wrapper-ptr child) pointer)
    (setf (wrapper-validity child) validity)
    child))
