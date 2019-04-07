(in-package :autowrap)

(defvar *definition-circles* nil
  "Detect circular type members")

(defvar *wrapper-constructors* (make-hash-table))

 ;; Wrappers

(declaim (inline make-wrapper wrapper-ptr))
(defstruct wrapper
  #+(or cmucl clasp ecl sbcl clisp lispworks)
  (ptr (cffi:null-pointer) :type cffi:foreign-pointer)
  #+(or ccl allegro)
  (ptr #.(cffi:null-pointer) :type cffi:foreign-pointer)
  #-(or cmucl clasp ecl sbcl clisp ccl allegro lispworks)
  (error "Unknown Lisp. Fix appropriate condition.")
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
    (cffi:foreign-pointer wrapper)
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

(defun wrapper-null-p (wrapper)
  (cffi-sys:null-pointer-p (ptr wrapper)))

(defmacro autocollect ((&optional (ptr (intern "PTR")))
                       wrapper-form &body body)
  (let* ((tg (find-package "TRIVIAL-GARBAGE"))
         (finalize (when tg (find-symbol "FINALIZE" tg))))
    (if (and tg finalize)
        (once-only (wrapper-form)
          `(let ((,ptr (ptr ,wrapper-form)))
             (,finalize ,wrapper-form
                        (lambda () ,@body))
             ,wrapper-form))
        (error "Trying to use AUTOCOLLECT without TRIVIAL-GARBAGE"))))

(defmacro autocollect-cancel (wrapper)
  (let* ((tg (find-package "TRIVIAL-GARBAGE"))
         (cancel (when tg (find-symbol "CANCEL-FINALIZATION" tg))))
    (if (and tg cancel)
        (once-only (wrapper)
          `(progn
             (,cancel ,wrapper)
             ,wrapper))
        (error "Trying to use CANCEL-FINALIZATION without TRIVIAL-GARBAGE"))))

(defmacro making-autocollect-instance ((ptr-var type-name) pointer-form
                                       &body cleanup-body)
  `(autocollect (,ptr-var)
       (autowrap:make-wrapper-instance ',type-name :ptr ,pointer-form)
     ,@cleanup-body))

(defmacro with-autocollect-cancel ((wrapper &key (invalidate-p t)) &body body)
  "Run `BODY`, and (by default, with `:invalidate-p` as `T`)
invalidate `WRAPPER`.  This is protected; a non-local exit from `BODY`
will still invalidate `WRAPPER`."
  (once-only (wrapper)
    `(unwind-protect
          (progn ,@body)
       (autocollect-cancel ,wrapper)
       ,(when invalidate-p
          `(invalidate ,wrapper)))))

(defun make-wrapper-instance (unqualified-record-name &rest args)
  (let ((fun (gethash unqualified-record-name *wrapper-constructors*)))
    (if fun
        (apply fun args)
        (error "Type ~S is not a wrapped type" unqualified-record-name))))

