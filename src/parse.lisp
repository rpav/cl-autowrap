(in-package :autowrap)

(defvar *foreign-type-symbol-function* 'default-foreign-type-symbol)
(defvar *foreign-record-list* nil)
(defvar *foreign-alias-list* nil)
(defvar *foreign-function-list* nil)
(defvar *foreign-extern-list* nil)
(defvar *foreign-constant-list* nil)
(defvar *foreign-raw-constant-list* nil)
(defvar *foreign-other-exports-list* nil)
(defvar *foreign-symbol-exceptions* nil)
(defvar *foreign-symbol-regex* nil)

 ;; Collecting symbols

(defmacro collecting-symbols (&body body)
  `(let (*foreign-record-list* *foreign-function-list* *foreign-extern-list*
         *foreign-constant-list* *foreign-other-exports-list* *foreign-alias-list*
         *foreign-raw-constant-list*)
     ,@body))

 ;; Types and symbols

(defun apply-regexps (string regex-list)
  (loop for r in regex-list do
    (multiple-value-bind (match matches)
        (ppcre:scan-to-strings (car r) string)
      (when (and match (functionp (cdr r)))
        (setf string (funcall (cdr r) string matches (car r))))))
  string)

(defun default-c-to-lisp (string)
  (let ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2")))
    (let ((string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2")))
      (if (eq #\_ (aref string 0))
          (nstring-upcase string)
          (nstring-upcase (nsubstitute #\- #\_ string))))))

(defun default-foreign-type-symbol (string type package)
  (let ((string (if *foreign-symbol-regex*
                    (apply-regexps string *foreign-symbol-regex*)
                    string)))
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
            (t (intern string package)))))))

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
      `((,tag (,(if (> id 0) nil (foreign-type-symbol name symbol-type *package*))
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

(defun pointer-alias-form-p (form)
  "If `FORM` is an alias to a pointer."
  (let ((tag (aval :tag form)))
    (cond
      ((string= tag "typedef") (pointer-alias-form-p (aval :type form)))
      ((string= tag ":pointer") t)
      (t nil))))

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

(defgeneric parse-form (form tag &key &allow-other-keys)
  (:documentation "Parse FORM tagged as TAG; specialize on (eql 'symbol)"))

(defmethod parse-form :around (form tag &rest keys &key &allow-other-keys)
  (etypecase tag
    (symbol (call-next-method))
    (string
     (apply #'parse-form form (if (eq #\: (aref tag 0))
                                  (make-keyword (substr* (string-upcase tag) 1))
                                  (intern (string-upcase tag) 'autowrap))
            keys))))

(defmethod parse-form (form tag &key &allow-other-keys)
  (warn "Unhandled form: ~S for input:~%  ~S" tag form))

(defmethod parse-form (form (tag (eql 'typedef)) &key &allow-other-keys)
  (alist-bind (name type) form
    (let ((sym (foreign-type-symbol name :ctype *package*)))
      (if (pointer*-to-record-form-p type)
          (pushnew sym *foreign-record-list* :test #'equal)
          (if (pointer-alias-form-p type)
              (pushnew sym *foreign-alias-list* :test #'equal)
              (pushnew sym *foreign-other-exports-list*)))
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

(defun parse-enum-to-const (fields)
  (loop for field in fields
        as name = (aval :name field)
        as sym = (foreign-type-symbol name :cconst *package*)
        collect `(defconstant ,sym ,(aval :value field))
        do (pushnew sym *foreign-constant-list*)))

(defmethod parse-form (form (tag (eql 'struct)) &key &allow-other-keys)
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

(defmethod parse-form (form (tag (eql 'union)) &key &allow-other-keys)
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

(defmethod parse-form (form (tag (eql 'enum)) &key &allow-other-keys)
  (alist-bind (name id fields) form
    (let ((sym (foreign-type-symbol name :cenum *package*)))
      (when (symbol-package sym)
        (pushnew sym *foreign-other-exports-list*))
      `(progn
         ,@(parse-enum-to-const fields)
         (define-foreign-enum ',sym ,id ',(parse-enum-fields fields))))))

(defmethod parse-form (form (tag (eql 'function)) &key &allow-other-keys)
  (alist-bind (name parameters return-type variadic) form
    (let ((sym (foreign-type-symbol name :cfun *package*)))
      (let ((cfun-fields (parse-fields parameters :cparam)))
        (push sym *foreign-function-list*)
        `(define-foreign-function '(,sym ,name
                                    ,@(when variadic '(:variadic-p t)))
             ',@(parse-type return-type (aval :tag return-type))
           ',cfun-fields)))))

(defmethod parse-form (form (tag (eql 'const)) &key &allow-other-keys)
  (alist-bind (name value) form
    (let ((sym (foreign-type-symbol name :cconst *package*)))
      (pushnew sym *foreign-constant-list*)
      (if (stringp value)
          `(defvar ,sym ,value)
          `(defconstant ,sym ,value)))))

(defmethod parse-form (form (tag (eql 'extern)) &key &allow-other-keys)
  (alist-bind (name type) form
    (let ((sym (foreign-type-symbol name :cextern *package*)))
      (push sym *foreign-extern-list*)
      `(define-foreign-extern ',sym ,name ',@(parse-type type (aval :tag type))))))

(defun read-json (file)
  (let ((*read-default-float-format* 'double-float))
    (json:decode-json file)))

 ;; Exported API

(defmacro c-include (h-file &key (spec-path *default-pathname-defaults*)
                     symbol-exceptions symbol-regex
                     exclude-definitions exclude-sources exclude-arch
                     include-sources
                     sysincludes
                     (definition-package *package*)
                     (function-package definition-package)
                     (wrapper-package definition-package)
                     (accessor-package wrapper-package)
                     (constant-package definition-package)
                     (extern-package accessor-package)
                     constant-accessor
                     trace-c2ffi)
  (let ((*foreign-symbol-exceptions* (alist-hash-table symbol-exceptions :test 'equal))
        (*foreign-symbol-regex* (mapcar (lambda (x)
                                          (cons (apply #'ppcre:create-scanner (car x) (cadr x))
                                                (eval (caddr x))))
                                        symbol-regex))
        (exclude-definitions (mapcar #'ppcre:create-scanner exclude-definitions))
        (exclude-sources (mapcar #'ppcre:create-scanner exclude-sources))
        (*package* (find-package definition-package))
        (h-file (path-or-asdf (eval h-file)))
        (spec-path (path-or-asdf (eval spec-path)))
        (sysincludes (eval sysincludes))
        (definition-package (find-package definition-package))
        (function-package (find-package function-package))
        (wrapper-package (find-package wrapper-package))
        (accessor-package (find-package accessor-package))
        (constant-package (find-package constant-package))
        (extern-package (find-package extern-package))
        (constant-name-value-map (gensym "CONSTANT-NAME-VALUE-MAP"))
        (constant-accessor-internal (gensym)))
    (multiple-value-bind (spec-name)
        (let ((*trace-c2ffi* trace-c2ffi))
          (ensure-local-spec h-file
                             :spec-path spec-path
                             :arch-excludes exclude-arch
                             :sysincludes sysincludes))
      (with-open-file (in-spec spec-name)
        (collecting-symbols
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (setf *failed-wraps* nil)
               ,@(when constant-accessor
                   `((defvar ,constant-name-value-map)
                     (defun ,constant-accessor-internal (name)
                       (declare (string name))
                       (multiple-value-bind (value presentp) (gethash name ,constant-name-value-map)
                         (if presentp
                             value
                             (error "~@<Unknown constant: ~S~%~:@>" name))))
                     (defun ,constant-accessor (name)
                       (,constant-accessor-internal name))
                     ;; I wonder if we really must break this loop..
                     (define-compiler-macro ,constant-accessor (&whole whole name)
                       (if (stringp name)
                           (,constant-accessor-internal name)
                           whole))
                     (export '(,constant-accessor))))
               #-sbcl
               (with-anonymous-indexing
                 ,@(loop for form in (read-json in-spec)
                         unless (or (included-p (aval :name form) exclude-definitions)
                                    (included-p (aval :location form) exclude-sources))
                           collect (parse-form form (aval :tag form))))
               #+sbcl
               (progn
                 (setf *foreign-record-index* (make-hash-table))
                 ,@(loop for form in (read-json in-spec)
                         unless (or (included-p (aval :name form) exclude-definitions)
                                    (and (included-p (aval :location form) exclude-sources)
                                         (not (included-p (aval :location form) include-sources))))
                           collect (parse-form form (aval :tag form)))
                 (setf *foreign-record-index* nil))
               ,@(when constant-accessor
                   `((setf ,constant-name-value-map (make-hash-table :test 'equal :size ,(length *foreign-constant-list*)))
                     (loop for (name . value) in ',*foreign-raw-constant-list*
                           do
                              (setf (gethash name ,constant-name-value-map) value))))
               ,@(loop for record in (reverse *foreign-record-list*)
                       collect `(define-wrapper ,record ,wrapper-package))
               ,@(loop for alias in (reverse *foreign-alias-list*)
                       collect `(define-wrapper ,alias ,wrapper-package))
               ,@(loop for record in (reverse *foreign-record-list*)
                       collect `(define-accessors ,record ,accessor-package))
               ,@(loop for symbol in (reverse *foreign-function-list*)
                       collect `(define-cfun ,symbol ,function-package))
               ,@(loop for symbol in (reverse *foreign-extern-list*)
                       collect `(define-cextern ,symbol ,extern-package))
               (compile-time-report-wrap-failures)
               ,(when *foreign-record-list*
                  `(export '(,@(mapcar (lambda (x) (etypecase x (symbol x) (cons (caadr x))))
                                *foreign-record-list*))))
               ,(when *foreign-function-list*
                  `(export ',(mapcar (lambda (x) (intern (symbol-name x) function-package))
                                     *foreign-function-list*)
                           ,function-package))
               ,(when *foreign-extern-list*
                  `(export ',(mapcar (lambda (x) (intern (symbol-name x) extern-package))
                                     *foreign-extern-list*) ,extern-package))
               ,(when *foreign-constant-list*
                  `(export ',(mapcar (lambda (x) (intern (symbol-name x) constant-package))
                                     *foreign-constant-list*) ,constant-package))
               ,(when *foreign-other-exports-list*
                  `(export ',*foreign-other-exports-list* ,definition-package)))
             (eval-when (:load-toplevel :execute)
               (report-wrap-failures 'load-time *standard-output*)
               (clear-wrap-failures))))))))
