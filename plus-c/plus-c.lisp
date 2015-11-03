(in-package :plus-c)

(defvar *topmost-parent* nil)
(defvar *final-value-set* nil)

 ;; Function calling

(defmacro c-fun (name &rest args)
  (if-let (fun (find-function name))
    (with-slots ((type autowrap::type)
                 (c-symbol autowrap::c-symbol)
                 (fields autowrap::fields)) fun
      (autowrap::foreign-to-ffi
       (and (car fields) (foreign-type (car fields)))
       (and (car fields) (foreign-type-name (car fields)))
       args fields
       (autowrap::make-foreign-funcall
        fun (when (foreign-function-variadic-p fun)
              (nthcdr (length fields) args)))))
    (error 'c-unknown-function :name name)))

 ;; Refs

;;; Because for some reason CFFI-SYS doesn't:
(declaim (inline mem-ref (setf mem-ref)))
(defun mem-ref (ptr type)
  (cffi-sys:%mem-ref ptr type))

(defun (setf mem-ref) (value ptr type)
  (cffi-sys:%mem-set value ptr type))

;;;
;;; (c:ref TYPE wrapper-or-pointer [FIELD ...] FINAL-FIELD)
;;;
;;; Ref types:
;;;
;;; SYMBOL - reference a field; if this is FINAL-FIELD, return a
;;; wrapper or value.  Multiple SYMBOL in a row will dereference
;;; fields, either by X.Y or X->Y
;;;
;;; INTEGER - array reference prior type; if this is the first field,
;;; array reference of TYPE, e.g., (c:ref :int x 42)
;;;
;;; * (the symbol, *) - deref a pointer; if this is a final field,
;;; return a wrapper or value
;;;
;;; & (the symbol, &) - as FINAL-FIELD only, returns the address of
;;; the last field
(defmacro c-ref (&whole whole-form wrapper type &rest fields)
  (let ((type (or (find-type type)
                  (error "Cannot find FFI type ~S in form ~S" type whole-form))))
    (once-only (wrapper)
      (let ((*topmost-parent* wrapper))
        (build-ref (car fields) type `(autowrap:ptr ,wrapper) (cdr fields))))))

;;; FIXME: now that we have MEM-REF locally with (SETF MEM-REF),
;;; this could be cleaned back up
(define-setf-expander c-ref (wrapper type &rest fields)
  (when-let (type (find-type type))
    (with-gensyms (v)
      (values
       nil nil
       `(,v)
       (let ((*final-value-set* v))
         (build-ref (car fields) type `(autowrap:ptr ,wrapper)
                    (cdr fields)))
       (build-ref (car fields) type `(autowrap:ptr ,wrapper)
                  (cdr fields))))))

(defgeneric build-ref (ref type current-ref rest))

#+(or)
(defmethod build-ref :before (ref type current-ref rest)
  (:say ref type
        :br "   " current-ref rest))

(defmethod build-ref (ref type current-ref rest)
  (error "Error parsing ref: ~S on type ~S" ref type))

(defmethod build-ref (ref (type foreign-alias) current-ref rest)
  (build-ref ref (foreign-type type) current-ref rest))

(defmethod build-ref ((ref integer) (type foreign-alias) current-ref rest)
  (if (typep (foreign-type type) 'foreign-pointer)
      (build-ref (car rest) (foreign-type type)
             (autowrap::make-array-ref type current-ref ref)
             (cdr rest))
      (call-next-method)))

(defmethod build-ref (ref (type foreign-pointer) current-ref rest)
  (if rest
      (build-ref (car rest) type current-ref (cdr rest))
      (if ref
          (build-ref ref (foreign-type type)
                     `(cffi-sys:%mem-ref ,current-ref :pointer) rest)
          (if *final-value-set*
              `(cffi-sys:%mem-set ,*final-value-set* ,current-ref :pointer)
              `(cffi-sys:%mem-ref ,current-ref :pointer)))))

(defmethod build-ref ((ref symbol) (type foreign-record) current-ref rest)
  (if (keywordp ref)
      (if-let (field (find-record-field type ref))
        (if (frf-bitfield-p field)
            (if *final-value-set*
                (once-only (current-ref)
                  `(cffi-sys:%mem-set ,(autowrap::make-bitfield-merge field current-ref *final-value-set*)
                                      ,current-ref ,(basic-foreign-type field)))
                (autowrap::make-bitfield-deref field current-ref))
            (build-ref (car rest) (foreign-type field)
                       (autowrap::make-field-ref field current-ref) (cdr rest)))
        (error 'c-unknown-field :type type :field ref))
      (build-ref (car rest) type
                 `(cffi-sys:inc-pointer ,current-ref
                                        (* ,(foreign-type-size type) ,ref))
                 (cdr rest))))

(defmethod build-ref ((ref (eql '*)) (type foreign-pointer)
                      current-ref rest)
  (let ((child-type (foreign-type type)))
    (build-ref nil child-type current-ref rest)))

(defmethod build-ref ((ref (eql '&)) type current-ref rest)
  (when rest
    (error "& may only be used at the end of a ref"))
  (when (and (typep type 'foreign-record-field)
             (frf-bitfield-p type))
    (error "You may not take the address of a bitfield"))
  current-ref)

(defmethod build-ref ((ref (eql 'string)) type current-ref rest)
  (when rest
    (error "STRING may only be used at the end of a ref"))
  `(cffi:foreign-string-to-lisp ,current-ref))

(defmethod build-ref ((ref integer) (type foreign-pointer) current-ref rest)
  (build-ref (car rest) (foreign-type type)
             (autowrap::make-array-ref type current-ref ref)
             (cdr rest)))

(defmethod build-ref ((ref symbol) (type foreign-pointer) current-ref rest)
  (if (keywordp ref)
      (call-next-method)
      (build-ref (car rest) type
                 (autowrap::make-array-ref :pointer current-ref ref)
                 (cdr rest))))

(defmethod build-ref ((ref integer) (type symbol) current-ref rest)
  (if (keywordp type)
      (build-ref (car rest) type
                 (autowrap::make-array-ref type current-ref ref)
                 (cdr rest))
      (call-next-method)))

(defmethod build-ref ((ref symbol) (type symbol) current-ref rest)
  (if (keywordp type)
      (build-ref (car rest) type
                 (autowrap::make-array-ref type current-ref ref)
                 (cdr rest))
      (call-next-method)))

(defmethod build-ref ((ref symbol) (type foreign-array) current-ref rest)
  (if (keywordp ref)
      (build-ref ref (foreign-type type)
                 (autowrap::make-array-ref type current-ref 0)
                 (cdr rest))
      (build-ref (car rest) (foreign-type type)
                 (autowrap::make-array-ref type current-ref ref)
                 (cdr rest))))

(defmethod build-ref ((ref null) (type symbol) current-ref rest)
  (if (keywordp type)
      (if *final-value-set*
          `(cffi-sys:%mem-set ,*final-value-set* ,current-ref ,(basic-foreign-type type))
          `(cffi-sys:%mem-ref ,current-ref ,(basic-foreign-type type)))
      (error "Not a basic type: ~S" type)))

(defmethod build-ref ((ref null) (type foreign-record) current-ref rest)
  (if *final-value-set*
      (error "You may not set the value of a record (~S)" type)
      (with-gensyms (v)
        `(let ((,v (make-instance ',(let ((name (foreign-type-name type)))
                                      (if (symbol-package name)
                                          name
                                          'autowrap:anonymous-type)))))
           (setf (autowrap::wrapper-ptr ,v) ,current-ref)
           (setf (autowrap::wrapper-validity ,v) ,*topmost-parent*)
           ,v))))

(defmethod build-ref ((ref null) (type foreign-enum) current-ref rest)
  (if *final-value-set*
      `(cffi-sys:%mem-set ,*final-value-set* ,current-ref ,(basic-foreign-type type))
      `(cffi-sys:%mem-ref ,current-ref ,(basic-foreign-type type))))

(defmethod build-ref ((ref null) (type foreign-array) current-ref rest)
  (if *final-value-set*
      `(cffi-sys:%mem-set ,*final-value-set* ,current-ref ,(basic-foreign-type type))
      `(cffi-sys:%mem-ref ,current-ref ,(basic-foreign-type type))))

(defmethod build-ref ((ref null) (type foreign-pointer) current-ref rest)
  (build-ref nil :pointer current-ref rest))

(defmethod build-ref ((ref null) (type foreign-string) current-ref rest)
  (if *final-value-set*
      (call-next-method)
      `(if autowrap::*inhibit-string-conversion*
           (values "" ,current-ref)
           (values
            (cffi:foreign-string-to-lisp ,current-ref)
            ,current-ref))))

 ;; c-let

(defun make-bindings (free-default bindings rest)
  (labels ((maybe-make-macro (bindings rest tmp v c-type value)
             (with-gensyms (r)
               `((macrolet ((,v (&rest ,r)
                              `(c-ref ,',tmp ,',c-type ,@,r)))
                   (symbol-macrolet ((,v ,(if (keywordp (basic-foreign-type (find-type c-type)))
                                              `(mem-ref ,tmp ,(basic-foreign-type c-type))
                                              tmp)))
                     ,@(when value `((setf ,v ,value)))
                     ,@(rec (cdr bindings) rest))))))
           (rec (bindings rest)
             (if bindings
                 (with-gensyms (tmp)
                   (destructuring-bind (v c-type &key (count 1) (free free-default) ptr from value)
                       (car bindings)
                     (if (or ptr from)
                         (if (keywordp c-type)
                             `((let ((,tmp ,ptr))
                                 ,@(maybe-make-macro bindings rest tmp v c-type nil)))
                             `((let ((,tmp
                                       ,(if from
                                            from
                                            `(let ((,tmp (make-instance ',c-type)))
                                               (setf (autowrap::wrapper-ptr ,tmp) ,ptr)
                                               ,tmp))))
                                 ,@(maybe-make-macro bindings rest tmp v c-type nil))))
                         (if free
                             `((with-alloc (,tmp ',c-type ,count)
                                 ,@(maybe-make-macro bindings rest tmp v c-type value)))
                             `((let ((,tmp (autowrap:alloc ',c-type ,count)))
                                 ,@(maybe-make-macro bindings rest tmp v c-type value)))))))
                 rest)))
    (first (rec bindings rest))))

(defmacro c-let (bindings &body body)
  (make-bindings nil bindings body))

(defmacro c-with (bindings &body body)
  (make-bindings t bindings body))
