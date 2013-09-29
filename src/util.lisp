(in-package :autowrap)

 ;; misc

(defun substr* (str start &optional end)
  "Make a shared substring of STR using MAKE-ARRAY :displaced-to"
  (let* ((end (or end (length str)))
         (len (- end start)))
    (make-array len :element-type (array-element-type str)
                    :displaced-to str
                    :displaced-index-offset start)))

(declaim (inline string+))
(defun string+ (string &rest strings)
  (apply #'concatenate 'string string strings))

(defun prefix-trim (list &key (pred 'string) regex)
  (if regex
      (let ((scanner (ppcre:create-scanner regex)))
        (mapcar (lambda (x) (ppcre:regex-replace-all scanner (funcall pred x) "")) list))
      (let* ((sorted-fields (sort (map 'vector pred list) #'string<))
             (first (elt sorted-fields 0))
             (last (elt sorted-fields (1- (or (length sorted-fields) 1))))
             (prefix-end (when (and first last) (mismatch first last))))
        (map 'list (lambda (x) (subseq (funcall pred x) prefix-end)) list))))

 ;; alists

(declaim (inline akey aval))
(defun akey (val alist) (car (rassoc val alist)))
(defun aval (key alist) (cdr (assoc key alist)))

(defmacro alist-bind ((&rest vars) alist &body body)
  "Inefficient but doesn't really matter here"
  (once-only (alist)
    `(let (,@(mapcar (lambda (x)
                       (if (consp x)
                           `(,(car x) (aval ,(cadr x) ,alist))
                           `(,x (aval ,(make-keyword x) ,alist))))
                     vars))
       ,@body)))

 ;; output

(defun write-nicely (stream object)
  (write object
         :stream stream
         :case :downcase
         :circle t
         :pretty t
         :readably t)
  (format stream "~%~%"))

 ;; testing

(defun included-p (thing includes)
  (when thing
    (loop for scanner in includes do
      (when (cl-ppcre:scan scanner thing)
        (return t)))))

(defun anonymous-p (form)
  (etypecase form
    (foreign-type
     (null (symbol-package (foreign-type-name form))))
    (cons
     (or (string= "" (aval :name form))
         (and (string= ":array" (aval :tag form))
              (string= "" (aval :name (aval :type form))))))))


 ;; files

(defun find-file-for-paths (file paths)
  (loop for path in paths
        as filename = (merge-pathnames file path)
        do (when (probe-file filename)
             (return filename))))

 ;; ASDF paths

(defun asdf-path (system &rest path)
  (asdf:component-pathname
   (or (asdf:find-component (asdf:find-system system t) path)
       (error "System ~S path not found: ~S" system path))))

(defun path-or-asdf (form)
  (etypecase form
    ((or string pathname) form)
    (list (apply #'asdf-path (car form) (cdr form)))))

 ;; Conditions

;; from pergamum
(defun report-simple-condition (condition stream)
  (apply #'format stream (simple-condition-format-control condition) (simple-condition-format-arguments condition)))

;; from pergamum
(defmacro define-simple-condition-for (base-type &key object-initarg (simple-condition-type 'simple-error) (signaler 'error)
                                                   (name (format-symbol t "SIMPLE-~A" base-type)))
  `(progn
     (define-condition ,name (,base-type ,simple-condition-type)
       ()
       (:report report-simple-condition))
     (defun ,base-type (,@(when object-initarg `(o)) format-control &rest format-arguments)
       (,signaler ',name ,@(when object-initarg `(,object-initarg o)) :format-control format-control :format-arguments format-arguments))))

;; from pergamum
(defmacro define-simple-error-for (base-type &key name object-initarg)
  "Define a simple error subclassing from BASE-TYPE and a corresponding
function, analogous to ERROR, but also optionally taking the object 
against which to err, and passing it to ERROR via the OBJECT-INITARG
keyword. The name of the simple error is constructed by prepending
'SIMPLE-' to BASE-TYPE.
Whether or not the error signaller will require and pass the
object is specified by OBJECT-INITARG being non-NIL."
  `(define-simple-condition-for ,base-type :object-initarg ,object-initarg :simple-condition-type simple-error :signaler error
                                ,@(when name `(:name ,name))))
