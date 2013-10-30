(in-package :autowrap)

 ;; Condition classes

(define-condition autowrap-condition () ())
(define-condition autowrap-error (autowrap-condition error) ())
(define-condition autowrap-continuable-error (autowrap-error) ())
(define-simple-error-for autowrap-continuable-error)

(define-condition undefined-foreign-type (autowrap-continuable-error)
  ((typespec                 :initarg :typespec))
  (:report (lambda (c s)
             (with-slots (typespec) c
               (format s "~@<Undefined foreign type: ~A~:@>" typespec)))))

(define-condition undefined-foreign-type-contextualised (undefined-foreign-type)
  ((context-format-control   :initarg :context-format-control)
   (context-format-arguments :initarg :context-format-arguments))
  (:report (lambda (c s)
             (with-slots (typespec
                          context-format-control
                          context-format-arguments) c
               (apply #'format s (concatenate 'string "~@<Note: skipping " context-format-control " due to undefined foreign type: ~A~:@>")
                      (append context-format-arguments (list typespec)))))))

(define-condition sffi-condition () ())
(define-condition sffi-error (sffi-condition error) ())
(define-condition sffi-continuable-error (sffi-error) ())
(define-simple-error-for sffi-continuable-error)

(define-condition invalid-wrapper (sffi-error)
  ((object :initarg :object :initform nil :accessor invalid-pointer-object))
  (:report (lambda (c s)
             (with-slots (object) c
               (format s "Error: Invalid/Destroyed Foreign Object: ~A" object)))))

 ;; Tooling

(defvar *failed-wraps* nil
  "A list of things we couldn't wrap.")

(defvar *wrap-failers* nil
  "A list of things whose absence caused other things to fail to wrap.")

(defun report-wrap-failures (kind stream)
  (format stream "; Total of ~D ~(~A~) skipped definitions" (length *failed-wraps*) kind)
  (if *failed-wraps*
      (format stream ":~%~@<;   ~@;~{~A ~}~:@>~%" (sort (copy-list *failed-wraps*) #'string<))
      (terpri stream))
  (let ((unique-failers (delete-duplicates (sort (mapcar #'prin1-to-string *wrap-failers*) #'string<) :test #'string=)))
    (format stream "; Total of ~D ~(~A~) missing entities" (length unique-failers) kind)
    (if unique-failers
        (format stream ":~%~@<;   ~@;~{~A ~}~:@>~%" unique-failers)
        (terpri stream))))

(defun clear-wrap-failures ()
  (setf *failed-wraps* nil)
  (setf *wrap-failers* nil))

(defmacro compile-time-report-wrap-failures ()
  (report-wrap-failures 'compile-time *error-output*))

(defun call-with-wrap-attempt (wrappable-name fn format-control format-args)
  (handler-case (funcall fn)
    ((or autowrap-continuable-error sffi-continuable-error) (e)
      (cond
        (format-control
         (apply #'format *error-output*
                (concatenate 'string "~@<; ~@;Unable to " format-control " due to: ~:@>~%")
                format-args)
         (format *error-output* "~@<;   ~@;~A~:@>~%" e))
        (t
         (format *error-output* "~@<; ~@;~A~:@>~%" e)))
      (push wrappable-name *failed-wraps*)
      nil)))

(defmacro with-wrap-attempt ((&optional format-control &rest format-args) wrappable-name &body body)
  `(call-with-wrap-attempt ,wrappable-name (lambda () ,@body) ,format-control (list ,@format-args)))
