(in-package :autowrap)

(defvar *foreign-type-symbol-function* 'default-foreign-type-symbol)
(defvar *foreign-c-to-lisp-function* 'default-c-to-lisp)
(defvar *foreign-record-list* nil)
(defvar *foreign-alias-list* nil)
(defvar *foreign-function-list* nil)
(defvar *foreign-extern-list* nil)
(defvar *foreign-constant-list* nil)
(defvar *foreign-raw-constant-list* nil)
(defvar *foreign-constant-excludes* nil)
(defvar *foreign-other-exports-list* nil)
(defvar *foreign-symbol-exceptions* nil)
(defvar *foreign-symbol-regex* nil)

(declaim (special *filter-spec-p*))

 ;; Collecting symbols

(defmacro collecting-symbols (&body body)
  `(let (*foreign-record-list* *foreign-function-list* *foreign-extern-list*
         *foreign-constant-list* *foreign-other-exports-list* *foreign-alias-list*
         *foreign-raw-constant-list*)
     ,@body))

 ;; Types and symbols

(defun apply-regexps (string regex-list)
  (loop for r in regex-list do
    (cond
      ((functionp (cdr r))
       (multiple-value-bind (match matches)
           (ppcre:scan-to-strings (car r) string)
         (when match
           (setf string (funcall (cdr r) string matches (car r))))))
      ((stringp (cdr r))
       (setf string (ppcre:regex-replace-all (car r) string (cdr r))))))
  string)

(defun default-c-to-lisp (string)
  (let ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2")))
    (let ((string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2")))
      (if (ppcre:all-matches "^(:_|_)" string)
          (nstring-upcase string)
          (nstring-upcase (nsubstitute #\- #\_ string))))))

(defun foreign-symbol-exception-p (string)
  (and *foreign-symbol-exceptions*
       (nth-value 1 (gethash string *foreign-symbol-exceptions*))))

(defun default-foreign-type-symbol (string type package)
  (let ((string (or (and *foreign-symbol-exceptions*
                         (gethash string *foreign-symbol-exceptions*))
                    (funcall *foreign-c-to-lisp-function*
                             (if *foreign-symbol-regex*
                                 (apply-regexps string *foreign-symbol-regex*)
                                 string)))))
    (if (eq #\: (aref string 0))
        (alexandria:make-keyword (subseq string 1))
        (cond
          ((eq type :cconst)
           (intern (format nil "+~A+" string) package))
          ((eq type :cenumfield)
           (alexandria:make-keyword string))
          ((eq type :cfield)
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
      `((,tag (,(if (and (string= name "") (> id 0))
                    nil
                    (foreign-type-symbol name symbol-type *package*))
               ,@(when (and (string= name "") (> id 0))
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

(defun maybe-add-constant (name value)
  (push (cons name value) *foreign-raw-constant-list*)
  (unless (included-p name *foreign-constant-excludes*)
    (let ((sym (foreign-type-symbol name :cconst *package*)))
      (pushnew sym *foreign-constant-list*)
      `(defparameter ,sym ,value))))

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

(defmethod parse-type (form (tage (eql :long-double)))
  '(long-double))

(defmethod parse-type (form (tag (eql :_bool)))
  (case (aval :bit-size form)
    ((8 nil) '(:unsigned-char))
    (32 '(:unsigned-int))))

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
          (let ((symbol (foreign-type-symbol name field-type *package*)))
            (list* symbol
                   `(,@(parse-type type (aval :tag type))
                     ,@(when (eq field-type :cfield)
                         `(:bit-size ,bit-size
                           :bit-offset ,bit-offset
                           :bit-alignment ,bit-alignment))))))))

(defun parse-enum-fields (fields)
  (let* ((type-symbol-fields
           (map 'vector
                (lambda (x)
                  (symbol-name
                   (foreign-type-symbol (aval :name x)
                                        :cenumfield
                                        *package*)))
                fields))
         (prefix-end (find-prefix type-symbol-fields)))
    (loop for field in fields
          as name = (foreign-type-symbol (aval :name field)
                                         :cenumfield *package*)
          as string = (symbol-name name)
          as truncated = (if (foreign-symbol-exception-p (aval :name field))
                             string
                             (substr* string prefix-end))
          collect (cons (intern truncated
                                (symbol-package name))
                        (aval :value field)))))

(defun parse-enum-to-const (fields)
  (loop for field in fields
        as name = (aval :name field)
        collect (maybe-add-constant name (aval :value field))
          into constants
        finally (return (remove-if #'null constants))))

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
  (alist-bind (name inline parameters return-type variadic) form
    (unless inline
      (let ((sym (foreign-type-symbol name :cfun *package*)))
        (let ((cfun-fields (parse-fields parameters :cparam)))
          (push sym *foreign-function-list*)
          `(define-foreign-function '(,sym ,name
                                      ,@(when variadic '(:variadic-p t)))
               ',@(parse-type return-type (aval :tag return-type))
             ',cfun-fields))))))

(defmethod parse-form (form (tag (eql 'const)) &key &allow-other-keys)
  (alist-bind (name value) form
    (maybe-add-constant name value)))

(defmethod parse-form (form (tag (eql 'extern)) &key &allow-other-keys)
  (alist-bind (name type) form
    (let ((sym (foreign-type-symbol name :cextern *package*)))
      (push sym *foreign-extern-list*)
      `(define-foreign-extern ',sym ,name ',@(parse-type type (aval :tag type))))))

(defun read-json (file)
  (let ((*read-default-float-format* 'double-float))
    (json:decode-json file)))

 ;; c-include utility

(defun make-scanners (list)
  (mapcar (lambda (x)
            (cons (apply #'ppcre:create-scanner (car x) (cadr x))
                  (eval (caddr x))))
          list))

(defun read-parse-forms (in-spec)
  (loop for form in (read-json in-spec)
        as name = (aval :name form)
        as location = (aval :location form)
        when (or *filter-spec-p* (not (excluded-p name location)))
        collect (parse-form form (aval :tag form)) into forms
        finally (return (remove-if #'null forms))))


(defun make-define-list (def-symbol list package)
 (loop for x in (reverse list)
       collect `(,def-symbol ,x ,package)))

(defun make-export-list (list package &optional sym-fun)
  `(export '(,@(mapcar (or sym-fun
                        (lambda (x) (intern (symbol-name x) package)))
                list))
           ,package))

(defun make-constant-accessor (name value-map-name)
  (with-gensyms (internal-name)
    `((defvar ,value-map-name)
      (defun ,internal-name (name)
        (declare (string name))
        (multiple-value-bind (value presentp) (gethash name ,value-map-name)
          (if presentp
              value
              (error "~@<Unknown constant: ~S~%~:@>" name))))
      (defun ,name (name)
        (,internal-name name))
      ;; I wonder if we really must break this loop..
      (define-compiler-macro ,name (&whole whole name)
        (if (stringp name)
            (,internal-name name)
            whole))
      (export '(,name)))))

 ;; Exported API

(defmacro c-include (h-file &key (spec-path *default-pathname-defaults*)
                     symbol-exceptions symbol-regex
                     exclude-definitions exclude-sources exclude-arch
                     include-definitions include-sources
                     sysincludes
                     (definition-package *package*)
                     (function-package definition-package)
                     (wrapper-package definition-package)
                     (accessor-package wrapper-package)
                     (constant-package definition-package)
                     (extern-package accessor-package)
                     constant-accessor exclude-constants
                     (trace-c2ffi *trace-c2ffi*) no-accessors no-functions
                     release-p version filter-spec-p
                     type-symbol-function c-to-lisp-function)
  (let ((*foreign-symbol-exceptions* (alist-hash-table symbol-exceptions :test 'equal))
        (*foreign-symbol-regex* (make-scanners symbol-regex))
        (*foreign-constant-excludes* (mapcar #'ppcre:create-scanner exclude-constants))
        (*foreign-raw-constant-list*)
        (*foreign-type-symbol-function* (or (and type-symbol-function
                                                 (eval type-symbol-function))
                                            *foreign-type-symbol-function*))
        (*foreign-c-to-lisp-function* (or (and c-to-lisp-function
                                               (eval c-to-lisp-function))
                                          *foreign-c-to-lisp-function*))
        (*include-definitions* include-definitions)
        (*include-sources* include-sources)
        (*exclude-definitions* (mapcar #'ppcre:create-scanner exclude-definitions))
        (*exclude-sources* (mapcar #'ppcre:create-scanner exclude-sources))
        (*package* (find-package definition-package))
        (*filter-spec-p* filter-spec-p)
        (h-file (path-or-asdf (eval h-file)))
        (spec-path (path-or-asdf (eval spec-path)))
        (sysincludes (eval sysincludes))
        (definition-package (find-package definition-package))
        (function-package (find-package function-package))
        (wrapper-package (find-package wrapper-package))
        (accessor-package (find-package accessor-package))
        (constant-package (find-package constant-package))
        (extern-package (find-package extern-package))
        (constant-name-value-map (gensym "CONSTANT-NAME-VALUE-MAP-"))
        (old-mute-reporting *mute-reporting-p*))
    (multiple-value-bind (spec-name)
        (let ((*trace-c2ffi* trace-c2ffi))
          (ensure-local-spec h-file
                             :spec-path spec-path
                             :arch-excludes exclude-arch
                             :sysincludes sysincludes
                             :version version
                             :spec-processor (if *filter-spec-p*
                                                 #'squash-unrelated-definitions
                                                 #'pass-through-processor)))
      (with-open-file (in-spec spec-name)
        (collecting-symbols
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (setf *failed-wraps* nil)
               (setf *mute-reporting-p* ,release-p)
               ,@(when constant-accessor
                   (make-constant-accessor constant-accessor constant-name-value-map))
               ;; Read and parse the JSON
               ;;
               ;; Note that SBCL seems to have issues with this not
               ;; being a toplevel form as of 1.1.9 and will crash.
               #-sbcl
               (with-anonymous-indexing
                 ,@(read-parse-forms in-spec))
               #+sbcl
               (progn
                 (setf *foreign-record-index* (make-hash-table))
                 ,@(read-parse-forms in-spec)
                 (setf *foreign-record-index* nil))
               ;; Map constants
               ,@(when constant-accessor
                   `((setf ,constant-name-value-map
                           (make-hash-table :test 'equal :size ,(length *foreign-constant-list*)))
                     (loop for (name . value) in ',*foreign-raw-constant-list*
                           do (setf (gethash name ,constant-name-value-map) value))))
               ;; Definitions
               ,@(make-define-list 'define-wrapper *foreign-record-list* wrapper-package)
               ,@(make-define-list 'define-wrapper *foreign-alias-list* wrapper-package)
               ,@(unless no-accessors
                   (make-define-list 'define-accessors *foreign-record-list* accessor-package))
               ,@(unless no-functions
                   (make-define-list 'define-cfun *foreign-function-list* function-package))
               ,@(make-define-list 'define-cextern *foreign-extern-list* extern-package)
               ;; Report on anything missing
               (compile-time-report-wrap-failures)
               ;; Exports
               ,(when *foreign-record-list*
                  (make-export-list *foreign-record-list* *package*
                                    (lambda (x) (etypecase x (symbol x) (cons (caadr x))))))
               ,(when *foreign-function-list*
                  (make-export-list *foreign-function-list* function-package))
               ,(when *foreign-extern-list*
                  (make-export-list *foreign-extern-list* extern-package))
               ,(when *foreign-constant-list*
                  (make-export-list *foreign-constant-list* constant-package))
               ,(when *foreign-other-exports-list*
                  `(export ',*foreign-other-exports-list* ,definition-package))
               (setf *mute-reporting-p* ,old-mute-reporting))
             (let ((*mute-reporting-p* ,release-p))
               (eval-when (:load-toplevel :execute)
                 (report-wrap-failures 'load-time *standard-output*)
                 (clear-wrap-failures)))))))))
