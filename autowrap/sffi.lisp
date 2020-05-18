(in-package :autowrap)

;;; Simplified FFI layer, built on CFFI-SYS.

 ;; Global

(defvar *foreign-types* (make-hash-table :test 'equal))
(defvar *foreign-functions* (make-hash-table))
(defvar *foreign-externs* (make-hash-table))

(defvar *inhibit-string-conversion* nil)

 ;; Temporary record indexing

(defvar *foreign-record-index* nil)

(defmacro with-anonymous-indexing (&body body)
  `(let ((*foreign-record-index* (make-hash-table)))
     ,@body))

(defun foreign-anonymous (id)
  (if *foreign-record-index*
      (gethash id *foreign-record-index*)
      (error "No current anonymous record index")))

(defun (setf foreign-anonymous) (v id)
  (if *foreign-record-index*
      (setf (gethash id *foreign-record-index*) v)
      (error "No current anonymous record index")))

 ;; Types

(defclass foreign-type ()
  ((name :initarg :name :initform nil :type symbol)
   (type :initarg :type :initform nil :accessor foreign-type :type (not null))))

(defgeneric foreign-type-name (object))
(defmethod foreign-type-name ((object foreign-type))
  (with-slots (name) object
     (if (and name (eq (symbol-package name) #.(find-package :cl)))
       ;; when a symbol belongs to `:cl' package, we have to rename it to avoid name conflcit.
       ;; for example symbol `t' is not a legal symbol name for function/structure argument in common lisp.
       (intern (format nil "C-~A" (symbol-name name)))
       name)))

(defmethod foreign-type-name ((object symbol))
  (if (keywordp object)
      object
      (error "Invalid type: ~S" object)))

(defmethod foreign-type-name ((object wrapper))
  (foreign-type-name (find-type (class-name (class-of object)))))

(defmethod print-object ((o foreign-type) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A" (foreign-type-name o))))

(defclass foreign-pointer (foreign-type) ())
(defclass foreign-alias (foreign-type) ())
(defclass foreign-array (foreign-pointer)
  ((size :initarg :size :initform 0 :accessor foreign-array-size :type integer)))

(defmethod print-object ((o foreign-pointer) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "TO-TYPE:~A" (foreign-type-name (foreign-type o)))))

(defclass foreign-string (foreign-pointer)
  ((type :initform :char)))

(defclass foreign-record (foreign-type)
  ((bit-size :initarg :bit-size :initform nil :accessor foreign-record-bit-size)
   (bit-alignment :initarg :bit-alignment :initform nil :accessor foreign-record-bit-alignment)
   (fields :initarg :fields :initform nil :accessor foreign-record-fields)))

(defclass foreign-field (foreign-type) ())
(defclass foreign-record-field (foreign-field)
  ((bitfield-p :initarg :bitfield-p :initform nil
               :accessor frf-bitfield-p
               :type (or null t))
   (bit-offset :initarg :bit-offset
               :initform 0 :accessor frf-bit-offset
               :type integer)
   (bit-alignment :initarg :bit-alignment
                  :initform 0 :accessor frf-bit-alignment
                  :type integer)
   (bit-size :initarg :bit-size
             :initform 0 :accessor frf-bit-size
             :type integer)
   ;; bit-width is the bitfield width if bitfield-p is t .. this differs
   ;; from bit-size, because bit-size is the full width of the field
   (bit-width :initarg :bit-width
              :initform 0 :accessor frf-bit-width
              :type (or null integer))))

(defclass foreign-enum (foreign-type)
  ((values :initarg :values :initform nil :accessor foreign-enum-values)))

(defclass foreign-symbol (foreign-type)
  ((c-symbol :initarg :c-symbol :initform nil :accessor foreign-symbol-c-symbol)))

(defclass foreign-extern (foreign-symbol) ())
(defclass foreign-function (foreign-record foreign-symbol)
  ((variadic-p :initarg :variadic-p
               :initform nil :accessor foreign-function-variadic-p)))

(defun find-type (typespec)
  (etypecase typespec
    ((or keyword foreign-type) typespec)
    (wrapper (find-type (type-of typespec)))
    (t (if (or (keywordp typespec)
               (typep typespec 'foreign-type))
           typespec
           (let ((type (gethash typespec *foreign-types*)))
             (if (and (not type)
                      (listp typespec)
                      (eq :pointer (car typespec)))
                 (when-let (child-type (find-type (cadr typespec)))
                   (create-type-from-complex-typespec typespec ":pointer alias for ~S" typespec))
                 type))))))

(defun undefined-enum-value (value)
  (push `(:enum ,value) *wrap-failers*)
  (autowrap-continuable-error "Undefined enum value: ~S" value))

(defun %undefined-type-error-no-context (typespec)
  (push typespec *wrap-failers*)
  (error 'undefined-foreign-type
         :typespec typespec))

(defun %undefined-type-error (typespec context-format-control context-format-arguments)
  (push typespec *wrap-failers*)
  (error 'undefined-foreign-type-contextualised
         :typespec typespec
         :context-format-control context-format-control
         :context-format-arguments context-format-arguments))

(defun undefined-type-error-no-context (typespec)
  (%undefined-type-error-no-context typespec))

(defun undefined-type-error (typespec context-format-control &rest context-format-arguments)
  (%undefined-type-error typespec context-format-control context-format-arguments))

(defun require-type-no-context (typespec)
  (or (find-type typespec)
      (%undefined-type-error-no-context typespec)))

(defun require-type (typespec context-format-control &rest context-format-arguments)
  (or (find-type typespec)
      (%undefined-type-error typespec context-format-control context-format-arguments)))

(defmethod foreign-enum-values ((object foreign-alias))
  (foreign-enum-values (foreign-type object)))

(defun enum-value (foreign-enum key)
  (ctypecase key
    (symbol
     (let ((enum (if (or (symbolp foreign-enum) (listp foreign-enum))
                     (require-type foreign-enum "map key ~S of enum ~S to value" key foreign-enum)
                     foreign-enum)))
       (aval key (foreign-enum-values enum))))
    (number key)))

(defun enum-key (foreign-enum val)
  (let ((enum (if (or (symbolp foreign-enum) (listp foreign-enum))
                  (require-type foreign-enum "map value ~S of enum ~S to value" val foreign-enum)
                  foreign-enum)))
    (akey val (foreign-enum-values enum))))

(defgeneric basic-foreign-type (foreign-type)
  (:documentation "Return the most basic type for `FOREIGN-TYPE`"))

(defmethod basic-foreign-type ((type foreign-type))
  (if (keywordp type)
      type
      (basic-foreign-type (foreign-type type))))

(defmethod basic-foreign-type ((type symbol))
  (if (keywordp type)
      type
      (basic-foreign-type (require-type type "determine the basic type of foreign type ~S" type))))

(defmethod basic-foreign-type ((type foreign-enum))
  (foreign-type type))

(defmethod basic-foreign-type ((type foreign-function))
  :pointer)

(defmethod basic-foreign-type ((type foreign-pointer))
  :pointer)

(defmethod basic-foreign-type ((type foreign-string))
  :pointer)

(defmethod basic-foreign-type ((type foreign-record))
  (if (foreign-scalar-p type)
      (let ((largest-field (reduce (lambda (x y)
                                     (if (> (frf-bit-size x) (frf-bit-size y)) x y))
                                   (foreign-record-fields type))))
        (basic-foreign-type largest-field))
      type))

(defmethod basic-foreign-type ((type null)) nil)

(defgeneric foreign-scalar-p (type)
  (:documentation "Return `T` if `TYPE` is a scalar type, or `NIL` (e.g., a record type)"))

(defmethod foreign-scalar-p ((type symbol))
  (let ((actual-type (find-type type)))
    (cond
      ((keywordp actual-type) t)
      ((typep actual-type 'foreign-type)
       (foreign-scalar-p actual-type))
      (t nil))))

(defmethod foreign-scalar-p (type) nil)
(defmethod foreign-scalar-p ((type foreign-pointer)) t)
(defmethod foreign-scalar-p ((type foreign-enum)) t)
(defmethod foreign-scalar-p ((type (eql ':void))) t)
(defmethod foreign-scalar-p ((type foreign-alias))
  (foreign-scalar-p (basic-foreign-type type)))

(defmethod foreign-scalar-p ((field foreign-record-field))
  (foreign-scalar-p (foreign-type field)))

(defmethod foreign-scalar-p ((type foreign-record))
  (when (and (eq :union (foreign-type type))
             (foreign-record-fields type))
    (every #'foreign-scalar-p (foreign-record-fields type))))

(defmethod foreign-scalar-p ((type list))
  (let ((specifier (car type)))
    (or (eq specifier :pointer)
        (eq specifier :enum))))

(defgeneric foreign-type-size (type)
  (:documentation "The size for type `TYPE`, in bytes."))

(defmethod foreign-type-size (type)
  (cffi-sys:%foreign-type-size type))

(defmethod foreign-type-size ((type foreign-record))
  (if-let ((bit-size (foreign-record-bit-size type)))
    (truncate bit-size 8)
    (error 'incomplete-type :type type)))

(defmethod foreign-type-size ((type foreign-enum))
  (foreign-type-size (foreign-type type)))

(defmethod foreign-type-size ((type foreign-function))
  (foreign-type-size :pointer))

(defmethod foreign-type-size ((type foreign-alias))
  (foreign-type-size (foreign-type type)))

(defmethod foreign-type-size ((type foreign-array))
  (* (foreign-type-size (foreign-type type))
     (foreign-array-size type)))

(defmethod foreign-type-size ((type foreign-pointer))
  (foreign-type-size :pointer))

(defmethod foreign-type-size ((type symbol))
  (if (keywordp type)
      (call-next-method)
      (foreign-type-size (require-type type "determine the size of foreign type ~S" type))))

(defgeneric unaliased-type (type)
  (:documentation "Find the unaliased type for TYPE"))

(defmethod unaliased-type (type) type)

(defmethod unaliased-type ((type foreign-alias))
  (unaliased-type (foreign-type type)))

(defmethod unaliased-type ((type symbol))
  (if (keywordp type)
      (call-next-method)
      (unaliased-type (require-type type "determine the unaliased type of ~S" type))))

(defun builtin-type-p (type)
  "Determine if TYPE is a builtin type (e.g., :char, :int, etc) or alias,
vs anything else (including enums)."
  (keywordp (unaliased-type type)))

(defgeneric foreign-qualified-name (type)
  (:documentation "Return the qualified name (e.g., `(:struct (FOO))` of `TYPE`."))

(defmethod foreign-qualified-name (type)
  (foreign-type-name type))

(defmethod foreign-qualified-name ((type foreign-record))
  `(,(foreign-type type) (,(foreign-type-name type))))

(defmethod foreign-qualified-name ((type foreign-enum))
  `(:enum (,(foreign-type-name type))))

(defmethod foreign-qualified-name ((type foreign-pointer))
  `(:pointer ,(foreign-qualified-name (foreign-type type))))

 ;; defining things

(defun define-foreign-type (name type)
  "Trivially define a foreign type given `NAME` and a `foreign-type` object,
`TYPE`"
  (setf (gethash name *foreign-types*) type))

(defun define-foreign-extern (name c-symbol type)
  "Define symbol `NAME` to be an extern of type `TYPE`."
  (with-wrap-attempt () name
    (let ((extern (make-instance 'foreign-extern
                                 :name name
                                 :c-symbol c-symbol
                                 :type (ensure-type type "extern ~S of type ~S" name type))))
      (setf (gethash name *foreign-externs*) extern))))

(defun define-foreign-record (name type bit-size bit-alignment field-list)
  "Define a foreign record (struct or union) given `NAME`, a symbol,
`TYPE`, either :struct or :union, and a list of fields.  The actual
name for the type will be `(:struct NAME)` or `(:union NAME)`, as
appropriate."
  (assert (member type '(:struct :union)))
  (with-wrap-attempt ("record (~A (~S))" type name) name
    (let* ((record (gethash `(,type (,name)) *foreign-types*)))
      (unless record
        (setf record (make-instance 'foreign-record
                                    :name name
                                    :type type))
        (define-foreign-type `(,type (,name)) record))
      (unless (= 0 bit-size)
        (setf (foreign-record-bit-size record) bit-size))
      (unless (= 0 bit-alignment)
        (setf (foreign-record-bit-alignment record) bit-alignment))
      (when (car field-list)
        (setf (foreign-record-fields record)
              (parse-record-fields type name field-list)))
      record)))

(defun define-foreign-enum (name id value-list)
  "Define a foreign enum given `NAME`, a symbol, and a list of
symbol-to-integer mappings, `VALUE-LIST`.  ID should be 0 unless
anonymous-indexing is enabled and this will be later typedef'd
by ID."
  ;; Type is somewhat irrelevant here; enums are always :int-sized and
  ;; a foreign-enum is always an :enum.
  (loop for value in value-list do
    (assert (and (symbolp (car value))
                 (integerp (cdr value)))))
  (when (and *foreign-record-index* (> id 0))
    (setf (foreign-anonymous id) name))
  (let ((enum (make-instance 'foreign-enum :name name :type :int
                                           :values value-list)))
    (define-foreign-type `(:enum (,name)) enum)))

(defmacro define-enum-from-constants ((name &optional regex) &body values)
  "Define an enum *and alias* `NAME` from a list of constants
`VALUES`.  Each value should evaluate to actual values, e.g. actual
`+CONSTANTS+`, or be a list in the form `'(SYMBOL VALUE)`.  If a
symbol is given alone, it is by default pruned of any common prefix
and made into a keyword.  If a list is specified, the symbol given is
used exactly."
  (let* ((just-symbols (remove-if #'consp values))
         (just-alist (remove-if #'symbolp values))
         (symbol-values (trim-symbols-to-alist just-symbols regex)))
    `(progn
       (define-foreign-enum ',name 0 (list ,@symbol-values ,@just-alist))
       (define-foreign-alias ',name '(:enum (,name))))))

(defun define-foreign-alias (name type)
  (with-wrap-attempt () name
    (define-foreign-type name
        (make-instance 'foreign-alias
                       :name name
                       :type (ensure-type type "alias of type ~S to ~S" type name)))))

(defun define-foreign-function (name-and-options return-type params)
  "=> FOREIGN-FUNCTION

Define a foreign function given a lisp symbol, C symbol (as a string),
return type and parameters.  Note this just defines to SFFI what the
foreign function looks like .. it doesn't actually DEFUN something to
call it.  "
  (destructuring-bind (name c-symbol &key variadic-p &allow-other-keys)
      name-and-options
    (with-wrap-attempt () name
      (let* ((return-type (ensure-type return-type "function ~S (nee ~S) with return type ~S" name c-symbol return-type))
             (fun (make-instance 'foreign-function
                                 :name name
                                 :c-symbol c-symbol
                                 :type return-type
                                 :variadic-p variadic-p)))
        (setf (foreign-record-fields fun)
              (loop for param in params
                 collect (make-instance 'foreign-field :name (car param)
                                        :type (ensure-type (cadr param) "function ~S (nee ~S) due to parameter ~S of type ~S"
                                                           name c-symbol (car param) (cadr param)))))
        (setf (gethash name *foreign-functions*) fun)))))

(defun parse-one-field (record-type record-type-name pre-offset
                        name type &key bitfield-p bit-size bit-offset bit-alignment
                        bit-width)
  (handler-case
      (make-instance 'foreign-record-field
        :name name
        :type (ensure-type type "record ~(~S~) ~S field ~S of type ~S"
                           record-type record-type-name name type)
        :bitfield-p bitfield-p
        :bit-size bit-size
        :bit-offset (+ pre-offset bit-offset)
        :bit-alignment bit-alignment
        :bit-width bit-width)
    (undefined-foreign-type (e)
      (unless *mute-reporting-p*
        (format *error-output* "~@<; ~@;~A~:@>~%" e))
      nil)))

(defun parse-record-fields (record-type record-type-name field-list &optional (pre-offset 0))
  (loop with record-fields
        for field in field-list
        do (destructuring-bind (field-name field-typespec &key bit-offset &allow-other-keys)
               field
             (if (symbol-package field-name)
                 (progn
                   (push (apply #'parse-one-field record-type record-type-name pre-offset field)
                         record-fields))
                 (destructuring-bind (anon-record-type &optional anon-params &rest anon-field-list)
                     (ensure-list field-typespec)
                   (declare (ignore anon-params))
                   (let* ((anonymous-fields (parse-record-fields anon-record-type nil
                                                                 anon-field-list bit-offset)))
                     (loop for i in anonymous-fields
                           do (push i record-fields))))))
        finally (return (delete-if #'null record-fields))))

(defun struct-or-union-p (name)
  (or (find-type `(:struct (,name)))
      (find-type `(:union (,name)))))

(defun cbv-return-p (fun)
  (and (foreign-function-cbv-p fun)
       (not (foreign-scalar-p (foreign-type fun)))))

(defun find-function (name-or-function)
  (if (typep name-or-function 'foreign-function)
      name-or-function
      (gethash name-or-function *foreign-functions*)))

(defun find-extern (name)
  (gethash name *foreign-externs*))

(defun find-record-field (name-or-type field-name)
  "=> FOREIGN-RECORD-FIELD

Given `NAME-OR-TYPE`, return the field object called `FIELD-NAME`."
  (let ((rec (etypecase name-or-type
               (foreign-record name-or-type)
               (symbol (require-type name-or-type "obtain the type of the field ~S of structure type ~S" field-name name-or-type)))))
    (loop for field in (foreign-record-fields rec) do
      (when (eq field-name (foreign-type-name field))
        (return field)))))

(defun %ensure-type (typespec context-format-control context-format-args)
  (or (find-type typespec)
      (when (atom typespec)
        (%undefined-type-error typespec context-format-control context-format-args))
      (create-type-from-complex-typespec typespec context-format-control context-format-args)
      (%undefined-type-error typespec context-format-control context-format-args)))

(defun ensure-type (typespec context-format-control &rest context-format-args)
  (%ensure-type typespec context-format-control context-format-args))

(defun parse-record-name (type-name)
  (destructuring-bind (name &key id &allow-other-keys)
      type-name
    (or name
        (foreign-anonymous id)
        (let ((sym (gensym "ANON-TYPE-")))
          (setf (foreign-anonymous id) sym)
          sym))))

(defun looks-like-a-string (typespec)
  (or (equal '(:string) typespec)
      (and (eq :pointer (car typespec))
           (atom (cadr typespec))
           (find (basic-foreign-type (cadr typespec))
                 '(:char :unsigned-char)))))

(defun create-type-from-complex-typespec (typespec context-format-control context-format-args)
  "=> TYPE

Create a type from `TYPESPEC` and return the `TYPE` structure representing it."
  (declare (cons typespec))
  (let ((type (car typespec)))
    (ecase type
      (enum
       (let ((name (parse-record-name (cadr typespec))))
         (define-foreign-enum name 0 (cddr typespec))))
      ((struct union)
       (let ((name (parse-record-name (cadr typespec))))
         (destructuring-bind (_ &key bit-size bit-alignment &allow-other-keys)
             (cadr typespec)
           (declare (ignore _))
           (define-foreign-record name (make-keyword type) bit-size bit-alignment (cddr typespec)))))
      ((:enum :struct :union)
       (let ((name (list type (list (parse-record-name (cadr typespec))))))
         (find-type name)))
      (:array
       (let ((type (%ensure-type (cadr typespec) context-format-control context-format-args))
             (size (caddr typespec)))
         (make-instance 'foreign-array :type type :size size)))
      (:string
       (define-foreign-type '(:string) (make-instance 'foreign-string)))
      (:pointer
       (define-foreign-type
           `(:pointer ,(cadr typespec))
           (if-let (basic-type (looks-like-a-string typespec))
             (make-instance 'foreign-string :type basic-type)
             (make-instance 'foreign-pointer :type (%ensure-type (cadr typespec) context-format-control context-format-args)))))
      (:void :void))))

 ;; Making Things

(defvar *signed-types* '(:char :short :int :long :long-long))

(defun foreign-type-to-lisp (type)
  (let* ((basic-type (basic-foreign-type type))
         (type-size (if (or (not (foreign-scalar-p type))
                            (eq :void type)
                            (typep type 'foreign-string))
                        nil
                        (cffi-sys:%foreign-type-size basic-type))))
    (cond
      ((typep type 'foreign-enum)
       '(or integer symbol))
      ((typep type 'foreign-string)
       `(or cffi:foreign-pointer
            string))
      ((typep type 'foreign-pointer)
       (if (and (foreign-type type)
                (typep (basic-foreign-type (foreign-type type)) 'foreign-record))
           (foreign-type-name (foreign-type type))
           'cffi:foreign-pointer))
      ((and (typep type 'foreign-alias)
            (eq basic-type :pointer))
       (if (find-class (foreign-type-name type) nil)
           (foreign-type-name type)
           'cffi:foreign-pointer))
      ((typep basic-type 'foreign-record)
       'wrapper)
      ((member type *signed-types*) `(signed-byte ,type-size))
      ((eq :float type) 'single-float)
      ((eq :double type) 'double-float)
      ((eq :pointer basic-type) 'cffi:foreign-pointer)
      ((eq :void type) 'null)
      (t `(unsigned-byte ,type-size)))))

(defgeneric foreign-to-ffi (type names params fields body)
  (:documentation ""))

(defmacro next-ffi ()
  (with-gensyms (next)
    `(let ((,next (cadr fields)))
       (foreign-to-ffi
        (and ,next (foreign-type ,next))
        (cdr names)
        (cdr params)
        (cdr fields)
        body))))

(defmethod foreign-to-ffi ((type null) names params fields body)
  (declare (ignore type names params fields))
  body)

(defmethod foreign-to-ffi (type names params fields body)
  `(let ((,(car names) ,(car params)))
     (declare (ignorable ,(car names)))
     ,(next-ffi)))

;;; Record by value.. just pass through the pointer
(defmethod foreign-to-ffi ((type foreign-record) names params fields body)
  `(let ((,(car names) ,(car params)))
     (declare (ignorable ,(car names)))
     ,(next-ffi)))

(defmethod foreign-to-ffi ((type foreign-enum) names params fields body)
  (let* ((name (car names))
         (value (car params))
         (type-name (foreign-type-name (foreign-type (car fields))))
         (expansion
           (cond
             ((keywordp value)
              (or (enum-value type-name value)
                  (undefined-enum-value value)))
             ((integerp value) value)
             (t `(let ((,name ,value))
                   (etypecase ,name
                     (symbol (enum-value ',type-name ,name))
                     (integer ,name)))))))
    `(let ((,name ,expansion))
       (declare (ignorable ,name))
       ,(next-ffi))))

(defmethod foreign-to-ffi ((type foreign-alias) names params fields body)
  (if (eq :pointer (basic-foreign-type type))
      `(let ((,(car names) (autowrap:ptr ,(car params))))
         (declare (ignorable ,(car names)))
         ,(next-ffi))
      (foreign-to-ffi (foreign-type type) names params fields body)))

(defmethod foreign-to-ffi ((type foreign-pointer) names params fields body)
  `(let ((,(car names) (ptr ,(car params))))
     (declare (ignorable ,(car names)))
     ,(next-ffi)))

(defmethod foreign-to-ffi ((type foreign-string) names params fields body)
  (let ((name (car names)))
    (with-gensyms (own-p)
      (let ((string-alloc
              `(progn
                 (setf ,name (cffi:foreign-string-alloc ,name))
                 (setf ,own-p t)))
            (constant-string-p
              (and (constantp (car params))
                   (stringp (car params)))))
        `(let ((,name (or ,(car params)
                          (cffi-sys:null-pointer)))
               (,own-p))
           (unwind-protect
                (progn
                  ,(if constant-string-p
                       string-alloc
                       `(when (stringp ,name)
                          ,string-alloc))
                  ,(next-ffi))
             ,(if constant-string-p
                  `(when ,name (cffi:foreign-string-free ,name))
                  `(when (and ,own-p ,name)
                     (cffi:foreign-string-free ,name)))))))))

(defgeneric foreign-wrap-up (type function body)
  (:documentation "Write code that wraps the result of `BODY`, which
is the return of a function `FUNCTION`, based on its type `TYPE`.
Specialize on `TYPE`."))

(defmethod foreign-wrap-up (type function body)
  "By default, just return the result .. this works for all the basic
types."
  (declare (ignore type function))
  body)

(defmethod foreign-wrap-up ((type foreign-string) function body)
  "For strings, return both a lisp string and the pointer"
  (declare (ignore type function))
  (with-gensyms (ptr)
    `(let ((,ptr ,body))
       (if *inhibit-string-conversion*
           (values "" ,ptr)
           (values
            ;; FOREIGN-STRING-TO-LISP will return NIL for a null pointer
            (cffi:foreign-string-to-lisp ,ptr)
            ,ptr)))))

(defmacro inhibit-string-conversion (&body body)
  `(let ((*inhibit-string-conversion* t))
     ,@body))

(defmethod foreign-wrap-up ((type foreign-pointer) function body)
  (let ((basic-type (basic-foreign-type (foreign-type type))))
    (typecase basic-type
      (foreign-record
       (let ((*package* (symbol-package
                         (foreign-type-name (foreign-type type)))))
         `(,(symbolicate "MAKE-" (foreign-type-name (foreign-type type)))
           :ptr ,body)))
      (otherwise body))))

(defmethod foreign-wrap-up ((type foreign-alias) function body)
  (if (find-class (foreign-type-name type) nil)
      (let ((*package* (symbol-package (foreign-type-name type))))
        `(,(symbolicate "MAKE-" (foreign-type-name type)) :ptr ,body))
      (foreign-wrap-up (foreign-type type) function body)))

(defun foreign-function-cbv-p (fun)
  (with-slots (fields) fun
    (or (not (foreign-scalar-p (foreign-type fun)))
        (some (lambda (p) (not (foreign-scalar-p (foreign-type p)))) fields))))

(defvar *build-libffi-call*
  (lambda (fun &rest r)
    (declare (ignore r))
    `(error "Call-by-value not implemented without loading cl-autowrap/libffi
  (trying to call ~S)"
            ',(foreign-type-name fun))))

(defun make-foreign-funcall (fun return-value param-names vargs)
  (with-slots (c-symbol fields) fun
    (if (foreign-function-cbv-p fun)
        (funcall *build-libffi-call* fun return-value param-names vargs)
        (foreign-wrap-up (foreign-type fun) fun
                         `(cffi-sys:%foreign-funcall ,c-symbol
                                                     (,@(loop for f in fields
                                                              for s in param-names
                                                              collect (basic-foreign-type f)
                                                              collect s)
                                                      ,@vargs
                                                      ,(basic-foreign-type
                                                        (foreign-type fun)))
                                                     :convention :cdecl)))))

(defvar *build-libffi-definition*
  (lambda (&rest r)
    (declare (ignore r))))

(defmacro define-cfun (name-or-function &optional (package *package*))
  (when-let ((fun (find-function name-or-function)))
    (with-wrap-attempt ("function ~S" name-or-function) name-or-function
      (with-slots (name type c-symbol fields) fun
        (let* ((fun-name (intern (symbol-name name) package))
               (param-names (mapcar #'foreign-type-name fields))
               (param-syms (mapcar (lambda (x) (gensym (symbol-name x))) param-names))
               (make-function-p (not (foreign-function-variadic-p fun)))
               (maybe-cbv-return
                 (when (cbv-return-p fun)
                   (list 'return-value))))
          (with-gensyms (!fun !fields rest)
            (let ((form
                    (if make-function-p
                        `(defun ,fun-name (,@maybe-cbv-return
                                           ,@param-names ,@(when (foreign-function-variadic-p fun) `(&rest ,rest)))
                           ,@(when maybe-cbv-return `((declare (ignorable ,@maybe-cbv-return))))
                           ,(let ((!fun (find-function name-or-function)))
                              (with-slots ((!fields fields)) !fun
                                (foreign-to-ffi
                                 (and (car !fields) (foreign-type (car !fields)))
                                 param-syms
                                 param-names
                                 !fields
                                 (make-foreign-funcall !fun (and maybe-cbv-return 'return-value)
                                                       param-syms (when (foreign-function-variadic-p fun) rest))))))
                        `(defmacro ,fun-name (,@maybe-cbv-return
                                              ,@param-names ,@(when (foreign-function-variadic-p fun) `(&rest ,rest)))
                           ,@(when maybe-cbv-return `((declare (ignorable ,@maybe-cbv-return))))
                           (let ((,!fun (find-function ',name-or-function)))
                             (with-slots ((,!fields fields)) ,!fun
                               (foreign-to-ffi
                                (and (car ,!fields) (foreign-type (car ,!fields)))
                                ',param-syms
                                (list ,@param-names)
                                ,!fields
                                (make-foreign-funcall ,!fun ,(and maybe-cbv-return 'return-value)
                                                      ',param-syms ,(when (foreign-function-variadic-p fun) rest)))))))))
              `(progn
                 ,@(when (foreign-function-cbv-p fun)
                     (list (funcall *build-libffi-definition* fun)))
                 ,form))))))))

(defmacro define-cextern (name &optional (package *package*))
  (with-wrap-attempt ("extern ~S" name) name
    (let* ((*package* package)
           (extern (find-extern name))
           (ptr-or-error
            `(or (cffi-sys:%foreign-symbol-pointer ,(foreign-symbol-c-symbol extern) :default)
                 (error "Foreign extern symbol not found: ~S" ',name))))
      (if (foreign-scalar-p (foreign-type extern))
          `(define-symbol-macro ,name
               (cffi-sys:%mem-ref ,ptr-or-error
                                  ,(basic-foreign-type (foreign-type extern))))
          `(define-symbol-macro ,name ,ptr-or-error)))))

 ;; Bitfields

;;; No this is not _necessarily_ correct; in theory an implementation
;;; may do whatever it pleases.  However, it appears correct in
;;; applicable instances.  Since the primary goal is accessing
;;; existing C data, and not using bitfields for new implementation,
;;; this is deemed sufficient.

;;; If this is discovered _not_ to work for a particular architecture,
;;; OS, or implementation, please report it with full details.

#+big-endian
(defun bitfield-mask (bit-offset bit-size bit-alignment field-width)
  (let* ((field-offset (truncate (- bit-offset (mod bit-offset bit-alignment)) 8))
         (right-offset (- bit-size (mod bit-offset bit-alignment) field-width))
         (mask (ash (1- (ash 1 field-width)) right-offset)))
    (values
     ;; offset: (inc-pointer ptr OFFSET) gets address of `value`
     field-offset
     ;; mask: (logand MASK value) gets the field's bits
     mask
     ;; back-shift: (ash (logand MASK value) BACK-SHIFT) gets the field value
     (- right-offset))))

#+little-endian
(defun bitfield-mask (bit-offset bit-size bit-alignment field-width)
  (declare (ignore bit-size))
  (let* ((field-offset (truncate (- bit-offset (mod bit-offset bit-alignment)) 8))
         (right-offset (mod bit-offset bit-alignment))
         (mask (ash (1- (ash 1 field-width)) right-offset)))
    (values
     ;; offset: (inc-pointer ptr OFFSET) gets address of `value`
     field-offset
     ;; mask: (logand MASK value) gets the field's bits
     mask
     ;; back-shift: (ash (logand MASK value) BACK-SHIFT) gets the field value
     (- right-offset))))

 ;; Accessors

(defvar *accessor-forms* nil)
(defvar *accessor-record-name* nil)
(defvar *accessor-params* nil)
(defvar *accessor-seen-types* nil)
(defvar *accessor-recursive-max-depth* 5)
(defvar *accessor-index* 0)
(defvar *accessor-declare* nil)

(defvar *wrapped-type-names* nil)

(declaim (inline accessor-params))
(defun accessor-params ()
  (reverse *accessor-params*))

(defun make-field-ref (field ref &optional index)
  (let* ((offset (truncate (frf-bit-offset field) 8)))
    (if (and (= 0 offset) (not index))
        ref
        `(cffi-sys:inc-pointer ,ref ,offset))))

(defun make-array-ref (field-or-type ref index)
  "`REF` must already refer to the field.  Use `MAKE-FIELD-REF` first."
  (let ((size (foreign-type-size (basic-foreign-type
                                  (if (symbolp field-or-type)
                                      field-or-type
                                      (foreign-type field-or-type))))))
    (if (or (symbolp index) (and (integerp index) (> index 0)))
        `(cffi-sys:inc-pointer ,ref ,(if (= size 1) index `(* ,size ,index)))
        ref)))

(defun make-field-deref (field ref)
  (let* ((type (or (basic-foreign-type (foreign-type field))
                   (undefined-type-error (foreign-type field) "dereference field ~S" field))))
    `(cffi-sys:%mem-ref ,ref ,type)))

(defun make-field-setter (field ref value)
  (let* ((type (basic-foreign-type (foreign-type field))))
    `(cffi-sys:%mem-set ,value ,ref ,type)))

(defun make-bitfield-deref (field ref)
  (multiple-value-bind (offset mask back-shift)
      (bitfield-mask (frf-bit-offset field)
                     (frf-bit-size field)
                     (frf-bit-alignment field)
                     (frf-bit-width field))
    ;; In this case, REF should already contain the field, obviating
    ;; OFFSET
    (declare (ignore offset))
    (if (= 0 back-shift)
        `(logand ,mask ,(make-field-deref field ref))
        `(ash (logand ,mask ,(make-field-deref field ref)) ,back-shift))))

(defun make-bitfield-merge (field ref val)
  (multiple-value-bind (offset mask back-shift)
      (bitfield-mask (frf-bit-offset field)
                     (frf-bit-size field)
                     (frf-bit-alignment field)
                     (frf-bit-width field))
    (declare (ignore offset))
    `(logior (ash ,val (- ,back-shift))
             (logand ,(make-field-deref field ref)
                     (logand (lognot ,mask)
                             (1- (ash 1 ,(frf-bit-size field))))))))

(defun make-accessor-name (field prefix)
  (let ((name (foreign-type-name field)))
    (setf name (if prefix (symbolicate prefix "." name) name))
    (if (typep (foreign-type field) 'foreign-array)
        (symbolicate name "[]")
        name)))

(defun make-bitfield-accessor (field accessor ref)
  (push `(defun ,accessor (,@(accessor-params))
           ,*accessor-declare* ,(make-bitfield-deref field ref))
        *accessor-forms*)
  (push `(defun (setf ,accessor) (,(intern "V") ,@(accessor-params))
           ,*accessor-declare*
           (cffi-sys:%mem-set ,(make-bitfield-merge field ref (intern "V")) ,ref
                              ,(basic-foreign-type field)))
        *accessor-forms*)
  (push `(export ',accessor ,*package*) *accessor-forms*))

(defun make-array-accessor (field type accessor ref)
  (let* ((index (symbolicate "I" (princ-to-string *accessor-index*)))
         (ref (make-array-ref field ref index))
         (*accessor-index* (1+ *accessor-index*))
         (*accessor-params* (list* index *accessor-params*)))
    (if (or (typep (basic-foreign-type type) 'foreign-record)
            (eq (basic-foreign-type type) :pointer))
        (make-normal-type-accessor field (foreign-type type)
                                   accessor ref)
        (progn
          (push `(defun ,accessor (,@(accessor-params))
                   ,*accessor-declare* ,(make-field-deref field ref))
                *accessor-forms*)
          (push `(defun (setf ,accessor) (,(intern "V") ,@(accessor-params))
                   ,*accessor-declare* ,(make-field-setter field ref (intern "V")))
                *accessor-forms*)
          (push `(export ',accessor ,*package*) *accessor-forms*)))))

(defun make-simple-accessor (field accessor ref)
  (push `(defun ,accessor (,@(accessor-params)) ,*accessor-declare* ,(make-field-deref field ref))
        *accessor-forms*)
  (push `(defun (setf ,accessor) (,(intern "V") ,@(accessor-params)) ,*accessor-declare*
           ,(make-field-setter field ref (intern "V")))
        *accessor-forms*)
  (push `(export ',accessor ,*package*) *accessor-forms*))

(defun make-child-accessor (accessor type parent ref)
  (with-gensyms (v)
    (push `(defun ,accessor (,@(accessor-params))
             ,*accessor-declare*
             (let ((,v (make-instance ',(foreign-type-name type))))
               (setf (wrapper-ptr ,v) ,ref)
               (setf (wrapper-validity ,v) ,parent)
               ,v))
          *accessor-forms*))
  (push `(export ',accessor ,*package*) *accessor-forms*))

(defun make-normal-type-accessor (field type accessor ref)
  (cond
    ((typep type 'foreign-array)
     (make-array-accessor field type accessor ref))
    ((typep type 'foreign-pointer)
     (make-simple-accessor field accessor ref)
     (cond
       ((not (eq :void (foreign-type type)))
        (make-normal-type-accessor field (foreign-type type)
                                   (symbolicate accessor "*")
                                   (make-field-deref field ref)))
       ((typep (foreign-type type) 'foreign-record)
        (%make-accessors (foreign-type type)
                         :prefix accessor
                         :ref (make-field-deref field ref)))))
    ((typep (basic-foreign-type type) 'foreign-record)
     (make-child-accessor accessor type *accessor-record-name* ref)
     (%make-accessors (basic-foreign-type type) :prefix accessor :ref ref))
    (t (make-simple-accessor field accessor ref))))

(defun make-normal-accessor (field accessor ref)
  (let ((accessor-ptr (symbolicate accessor "&")))
    (make-normal-type-accessor field (foreign-type field) accessor ref)
    (push `(defun ,accessor-ptr (,@(accessor-params)) ,*accessor-declare* ,ref)
          *accessor-forms*)
    (push `(export ',accessor-ptr ,*package*) *accessor-forms*)))

(defun %make-accessors (foreign-record &key prefix ref)
  (unless (or (member (foreign-type-name foreign-record)
                      *accessor-seen-types*)
              (= 0 *accessor-recursive-max-depth*))
    (let* ((*accessor-seen-types* (list* (foreign-type-name foreign-record)
                                         *accessor-seen-types*))
           (*accessor-recursive-max-depth* (1- *accessor-recursive-max-depth*))
           (*accessor-declare* `(declare (type ,*accessor-record-name*
                                               ,*accessor-record-name*)))
           (ref (or ref `(ptr ,*accessor-record-name*))))
      (loop for field in (foreign-record-fields foreign-record)
            as accessor = (make-accessor-name field prefix)
            as field-ref = (make-field-ref field ref)
            as bitfield-p = (frf-bitfield-p field) do
              (if bitfield-p
                  (make-bitfield-accessor field accessor field-ref)
                  (make-normal-accessor field accessor field-ref))))))

(defmacro define-accessors (foreign-record &optional (package *package*))
  (with-wrap-attempt ("accessors for structure ~S" foreign-record) foreign-record
    (let ((*package* (find-package package))
          (foreign-record (etypecase foreign-record
                            (foreign-record foreign-record)
                            ((or symbol cons) (require-type-no-context foreign-record)))))
      (unless (and (typep foreign-record 'foreign-alias)
                   (struct-or-union-p (foreign-type-name foreign-record)))
        (let* ((*accessor-forms*)
               (*accessor-seen-types*)
               (*accessor-params* (list (foreign-type-name foreign-record)))
               (*accessor-record-name* (foreign-type-name foreign-record))
               (actual-foreign-record (basic-foreign-type foreign-record)))
          (when (typep actual-foreign-record 'foreign-record)
            (%make-accessors actual-foreign-record :prefix *accessor-record-name*)
            `(progn ,@(nreverse *accessor-forms*))))))))

(defmacro define-wrapper (type &optional (package *package*))
  (let* ((*package* package)
         (type (etypecase type
                 (foreign-type type)
                 ((or symbol cons) (require-type-no-context type))))
         (name (foreign-type-name type)))
    `(define-wrapper* ,type ,name)))

(defmacro define-wrapper* (type wrapper-name &key constructor conc-name)
  (with-wrap-attempt ("lisp structure for foreign structure ~S" type) type
    (let* ((type (etypecase type
                   (foreign-type type)
                   ((or symbol cons) (require-type-no-context type))))
           ;; Note this is a bit of a hack because the following is valid
           ;; in C:
           ;;
           ;;     typedef struct x { ... } *x;
           ;;
           ;; Thus in this case we make an exception and define x* for
           ;; the alias
           (existing (find-class (foreign-type-name type) nil))
           (star-p (and existing (typep type 'foreign-pointer)))
           ;; FIXME: These should be given a separate name, but this has
           ;; to be deduced _everywhere_.
           (constructor-name
             (or constructor
                 (intern (string+ "MAKE-" wrapper-name)
                         (symbol-package wrapper-name))))
           (conc-name
             (or conc-name
                 (string+ wrapper-name "-"))))
      (when (or (not existing) star-p)
        `(progn
           (setf (gethash ',(foreign-type-name type) *wrapper-constructors*)
                 ',constructor-name)
           (defstruct (,wrapper-name
                       (:constructor ,constructor-name)
                       (:conc-name ,conc-name)
                       (:include ,(if (or (keywordp (foreign-type type))
                                          (anonymous-p (foreign-type type))
                                          (null (foreign-type-name (foreign-type type))))
                                      'wrapper
                                      (foreign-type-name (foreign-type type)))))))))))


 ;; Functions

(defun foreign-function (function-name)
  "Return a lambda which calls function `FUNCTION-NAME`.  This is
useful if you need to funcall foreign functions (which is quite handy!),
since CFFI-SYS and thus SFFI use macros for ordinary calls."
  (if-let ((fun (find-function function-name)))
    (with-slots (type c-symbol fields) fun
      (let ((names (mapcar #'foreign-type-name fields))
            (args (mapcar (lambda (x)
                            (intern (symbol-name (foreign-type-name x))))
                          fields))
            (maybe-cbv-return (when (cbv-return-p fun)
                                'return-value)))
        (eval
         `(lambda ,(if maybe-cbv-return
                       (list* maybe-cbv-return args)
                       args)
            ,(foreign-to-ffi
              (and (car fields) (foreign-type (car fields)))
              names args fields
              (autowrap::make-foreign-funcall
               fun maybe-cbv-return names
               (when (foreign-function-variadic-p fun)
                 (nthcdr (length fields) args))))))))
    (error "Unknown function: ~S" function-name)))

 ;; Callbacks

(defmacro defcallback (name return-type lambda-list &body body)
  "This acts much like CFFI:DEFCALLBACK except it allows SFFI type
aliases to be specified."
  (let ((param-names (mapcar #'car lambda-list)))
    `(cffi-sys:%defcallback
      ,name ,(basic-foreign-type return-type)
      ,param-names
      ,(mapcar (lambda (x) (basic-foreign-type (ensure-type (cadr x)
                                                            "callback ~S, due to argument ~S of type ~S"
                                                            name (car x) (cadr x))))
               lambda-list)
      (funcall (lambda ,param-names ,@body) ,@param-names)
      :convention :cdecl)))

(defmacro callback (name)
  `(cffi-sys:%callback ,name))
