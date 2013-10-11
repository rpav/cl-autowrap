(in-package :plus-c)

(define-condition c-unknown-function (error)
  ((name :initarg :name))
  (:report (lambda (c s)
             (with-slots (name) c (format s "Unknown function: ~S" name)))))

(define-condition c-unknown-field (error)
  ((field :initarg :field)
   (type :initarg :type))
  (:report (lambda (c s)
             (with-slots (field type) c
               (format s "Unknown field ~S for foreign-record type:~%~S
Valid fields:~%~{  ~S (~S)~%~}"
                       field type
                       (loop for f in (foreign-record-fields type)
                             collect (foreign-type-name f)
                             collect (foreign-type-name (foreign-type f))))))))
