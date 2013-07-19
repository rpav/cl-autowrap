(in-package :autowrap)

(defvar *foreign-type-symbol-function* 'default-foreign-type-symbol)
(defvar *foreign-record-list* nil)
(defvar *foreign-function-list* nil)
(defvar *foreign-extern-list* nil)
(defvar *foreign-symbol-exceptions* nil)

 ;; Collecting symbols

(defmacro collecting-symbols (&body body)
  `(let (*foreign-record-list* *foreign-function-list* *foreign-extern-list*)
     ,@body))

 ;; Types and symbols

(defun default-c-to-lisp (string)
  (let ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2")))
    (let ((string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2")))
      (if (eq #\_ (aref string 0))
          (nstring-upcase string)
          (nstring-upcase (nsubstitute #\- #\_ string))))))

(defun default-foreign-type-symbol (string type package)
  (let ((string
          (or (and *foreign-symbol-exceptions*
                   (gethash string *foreign-symbol-exceptions*))
              (default-c-to-lisp string))))
    (if (eq #\: (aref string 0))
        (alexandria:make-keyword (subseq string 1))
        (cond
          ((eq type :cconst)
           (intern (format nil "+~A+" string) package))
          ((eq type :cenumfield)
           (alexandria:make-keyword string))
          (t (intern string package))))))

(defun foreign-type-symbol (string type package)
  (if (string= "" string)
      (case type
        (:cparam (gensym "P"))
        (:cfield (gensym "FIELD-"))
        (otherwise (gensym "ANON-TYPE-")))
      (funcall *foreign-type-symbol-function* string type package)))

(defun make-record-ref (form)
  (alist-bind (tag name id) form
    (let (symbol-type)
      (cond
        ((string= tag ":struct")
         (setf tag :struct)
         (setf symbol-type :cstruct))
        ((string= tag ":union")
         (setf tag :union)
         (setf symbol-type :cunion))
        ((string= tag ":enum")
         (setf tag :enum)
         (setf symbol-type :cenum))
        (t (error "Unknown tag for MAKE-RECORD-REF: ~S" tag)))
      `((,tag (,(foreign-type-symbol name symbol-type *package*)
               ,@(when (> id 0)
                   `(:id ,id))))))))

(defun record-form-p (form)
  "Return whether FORM describes a struct or union"
  (let ((tag (aval :tag form)))
   (or (string= "struct" tag)
       (string= "union" tag)
       (string= ":struct" tag)
       (string= ":union" tag))))

(defun pointer*-to-record-form-p (form)
  "If `FORM` describes a type which is a record, or one or more levels
of pointer-to-record"
  (let ((tag (aval :tag form)))
    (if (string= ":pointer" tag)
        (pointer*-to-record-form-p (aval :type form))
        (record-form-p form))))

 ;; Parsing

(defgeneric parse-type (form tag)
  (:documentation "Parse FORM describing a type, tagged by TAG.
Return the appropriate CFFI name."))

(defmethod parse-type (form tag)
  (list (foreign-type-symbol (aval :tag form) :ctype *package*)))

(defmethod parse-type :around (form tag)
  (etypecase tag
    (symbol (call-next-method))
    (string
     (parse-type form (if (eq #\: (aref tag 0))
                          (make-keyword (substr* (string-upcase tag) 1))
                          (intern (string-upcase tag) 'autowrap))))))

(defmethod parse-type (form (tag (eql :struct)))
  (make-record-ref form))

(defmethod parse-type (form (tag (eql :union)))
  (make-record-ref form))

(defmethod parse-type (form (tag (eql :enum)))
  (make-record-ref form))

(defmethod parse-type (form (tag (eql :pointer)))
  (alist-bind (type) form
    (alist-bind ((type-tag :tag)) type
      (cond
        ((or (string= ":char" type-tag)
             (string= ":unsigned-char" type-tag))
         '((:string)))
        (t `((:pointer ,@(parse-type type type-tag))))))))

(defmethod parse-type (form (tag (eql :function-pointer)))
  '((:pointer (:void))))

(defmethod parse-type (form (tag (eql :signed-char)))
  '(:char))

(defmethod parse-type (form (tag (eql :array)))
  (alist-bind (type size) form
    `((:array ,@(parse-type type (aval :tag type)) ,size))))

(defmethod parse-type (form (tag (eql :bitfield)))
  (alist-bind (type width) form
    `(,@(parse-type type (aval :tag type))
      :bitfield-p t
      :bit-width ,width)))

(defun make-foreign-record-name (form type)
  (alist-bind (name id) form
    (let ((size (list :bit-size (aval :bit-size form)
                      :bit-alignment (aval :bit-alignment form)))
          (symbol-type (ecase type (:struct :cstruct) (:union :cunion))))
      (if (anonymous-p form)
          (list* nil :id id size)
          (let ((sym (foreign-type-symbol name symbol-type *package*)))
            (pushnew `(,type (,sym)) *foreign-record-list* :test #'equal)
            (list* sym size))))))

(defmethod parse-type (form (tag (eql 'struct)))
  (alist-bind (fields) form
    `((struct ,(make-foreign-record-name form :struct)
              ,@(parse-fields fields)))))

(defmethod parse-type (form (tag (eql 'union)))
  (alist-bind (fields) form
    `((union ,(make-foreign-record-name form :union)
             ,@(parse-fields fields)))))

(defmethod parse-type (form (tag (eql 'enum)))
  (alist-bind (name id fields) form
    `((enum ,(if (anonymous-p form)
                 (list nil :id id)
                 (foreign-type-symbol name :cenum *package*))
            ,@(parse-enum-fields fields)))))

(defgeneric parse-form (form tag)
  (:documentation "Parse FORM tagged as TAG; specialize on (eql 'symbol)"))

(defmethod parse-form :around (form tag)
  (etypecase tag
    (symbol (call-next-method))
    (string
     (parse-form form (if (eq #\: (aref tag 0))
                          (make-keyword (substr* (string-upcase tag) 1))
                          (intern (string-upcase tag) 'autowrap))))))

(defmethod parse-form (form tag)
  (warn "Unhandled form: ~S for input:~%  ~S" tag form))

(defmethod parse-form (form (tag (eql 'typedef)))
  (alist-bind (name type) form
    (let ((sym (foreign-type-symbol name :ctype *package*)))
      (when (pointer*-to-record-form-p type)
        (pushnew sym *foreign-record-list* :test #'equal))
      `(define-foreign-alias
           ',sym
           ',@(parse-type type (aval :tag type))))))

(defun parse-fields (fields &optional (field-type :cfield))
  (loop for field in fields
        collect
        (alist-bind (name type bit-size bit-offset bit-alignment) field
          (list* (foreign-type-symbol name field-type *package*)
                 `(,@(parse-type type (aval :tag type))
                   ,@(when (eq field-type :cfield)
                       `(:bit-size ,bit-size
                         :bit-offset ,bit-offset
                         :bit-alignment ,bit-alignment)))))))

(defun parse-enum-fields (fields)
  (let* ((sorted-fields (sort (map 'vector (lambda (x) (aval :name x)) fields)
                              #'string<))
         (first (elt sorted-fields 0))
         (last (elt sorted-fields (1- (or (length sorted-fields) 1))))
         (prefix-end (when (and first last) (mismatch first last))))
    (loop for field in fields
          as name = (aval :name field)
          collect (cons (foreign-type-symbol (if prefix-end (subseq name prefix-end) name)
                                             :cenumfield *package*)
                        (aval :value field)))))

(defmethod parse-form (form (tag (eql 'struct)))
  (alist-bind (name fields) form
    (let ((sym (foreign-type-symbol name :cstruct *package*)))
      (let ((cstruct-fields (parse-fields fields)))
        (when (symbol-package sym)
          (pushnew `(:struct (,sym)) *foreign-record-list*
                   :test #'equal))
        `(define-foreign-record ',sym :struct
           ,(aval :bit-size form)
           ,(aval :bit-alignment form)
           ',cstruct-fields)))))

(defmethod parse-form (form (tag (eql 'union)))
  (alist-bind (name fields) form
    (let ((sym (foreign-type-symbol name :cunion *package*)))
      (let ((cunion-fields (parse-fields fields)))
        (when (symbol-package sym)
          (pushnew `(:union (,sym)) *foreign-record-list*
                   :test #'equal))
        `(define-foreign-record ',sym :union
           ,(aval :bit-size form)
           ,(aval :bit-alignment form)
           ',cunion-fields)))))

(defmethod parse-form (form (tag (eql 'enum)))
  (alist-bind (name fields) form
    (let ((sym (foreign-type-symbol name :cenum *package*)))
      `(define-foreign-enum ',sym ',(parse-enum-fields fields)))))

(defmethod parse-form (form (tag (eql 'function)))
  (alist-bind (name parameters return-type variadic) form
    (let ((sym (foreign-type-symbol name :cfun *package*)))
      (let ((cfun-fields (parse-fields parameters :cparam)))
        (push sym *foreign-function-list*)
        `(define-foreign-function '(,sym ,name
                                    ,@(when variadic '(:variadic-p t)))
             ',@(parse-type return-type (aval :tag return-type))
           ',cfun-fields)))))

(defmethod parse-form (form (tag (eql 'const)))
  (alist-bind (name value) form
    (let ((sym (foreign-type-symbol name :cconst *package*)))
      (if (stringp value)
          `(defvar ,sym ,value)
          `(defconstant ,sym ,value)))))

(defmethod parse-form (form (tag (eql 'extern)))
  (alist-bind (name type) form
    (let ((sym (foreign-type-symbol name :cextern *package*)))
      (push sym *foreign-extern-list*)
      `(define-foreign-extern ',sym ,name ',@(parse-type type (aval :tag type))))))

 ;; Exported API

(defmacro c-include (h-file &key (spec-path *default-pathname-defaults*)
                     symbol-exceptions exclude-definitions exclude-sources
                     exclude-arch)
  (let ((*foreign-symbol-exceptions* (alist-hash-table symbol-exceptions :test 'equal))
        (h-file (path-or-asdf (eval h-file)))
        (spec-path (path-or-asdf (eval spec-path))))
    (multiple-value-bind (h-name m-name)
        (ensure-local-spec h-file spec-path exclude-arch)
      (with-open-file (in-h h-name)
        (with-open-file (in-m m-name)
          (collecting-symbols
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               (with-anonymous-indexing
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   ,@(loop for form in (json:decode-json in-h)
                           unless (or (excluded-p (aval :name form) exclude-definitions)
                                      (excluded-p (aval :location form) exclude-sources))
                             collect (parse-form form (aval :tag form)))))
               ,@(loop for form in (json:decode-json in-m)
                       collect (parse-form form (aval :tag form)))
               ,@(loop for record in (reverse *foreign-record-list*)
                       collect `(define-wrapper ,record))
               ,@(loop for record in (reverse *foreign-record-list*)
                       collect `(define-accessors ,record))
               ,@(loop for symbol in (reverse *foreign-function-list*)
                       collect `(define-cfun ,symbol))
               ,@(loop for symbol in (reverse *foreign-extern-list*)
                       collect `(define-cextern ,symbol))
               ,(when *foreign-record-list*
                  `(export '(,@(mapcar (lambda (x) (etypecase x (symbol x) (cons (caadr x))))
                                *foreign-record-list*))))
               ,(when *foreign-function-list*
                  `(export ',*foreign-function-list*))
               ,(when *foreign-extern-list*
                  `(export ',*foreign-extern-list*)))))))))
