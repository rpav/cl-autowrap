(in-package :autowrap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let (unsigned signed)
    (map nil
         (lambda (x)
           (push (cons (foreign-type-size x) x) unsigned))
         '(:unsigned-char :unsigned-int :unsigned-long))
    (map nil
         (lambda (x)
           (push (cons (foreign-type-size x) x) signed))
         '(:char :int :long))
    (define-foreign-type 'int8 (aval 1 signed))
    (define-foreign-type 'uint8 (aval 1 unsigned))
    (define-foreign-type 'int16 (aval 2 signed))
    (define-foreign-type 'uint16 (aval 2 unsigned))
    (define-foreign-type 'int32 (aval 4 signed))
    (define-foreign-type 'uint32 (aval 4 unsigned))
    (define-foreign-type 'int64 (aval 8 unsigned))
    (define-foreign-type 'uint64 (aval 8 unsigned))
    #+x86-64
    (define-foreign-type 'size-t 'uint64)
    #+x86
    (define-foreign-type 'size-t 'uint32)
    #+arm
    (define-foreign-type 'size-t 'uint32))

  (define-foreign-function '(c-malloc "malloc") :pointer
    '((size size-t)))

  (define-foreign-function '(c-calloc "calloc") :pointer
    '((nmemb size-t)
      (size size-t)))

  (define-foreign-function '(c-free "free") :void
    '((ptr :pointer)))

  (define-foreign-function '(c-realloc "realloc") :pointer
    '((ptr :pointer)
      (size size-t)))

  (define-foreign-function '(c-memset "memset") :pointer
    '((s :pointer)
      (c :int)
      (n size-t)))

  (define-foreign-function '(c-memcpy "memcpy") :pointer
    '((dest :pointer)
      (src :pointer)
      (n size-t))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-cfun c-malloc)
  (define-cfun c-calloc)
  (define-cfun c-free)
  (define-cfun c-realloc)
  (define-cfun c-memset)
  (define-cfun c-memcpy))

 ;; Allocating things

(defun alloc-ptr (type &optional (count 1))
  "Return a pointer allocated to the size of `TYPE`"
  (c-malloc
   (* count (foreign-type-size
             (require-type type "allocate an instance of foreign type ~S" type)))))

(defun calloc-ptr (type &optional (count 1))
  "Return a pointer allocated to the size of `TYPE`, initialized to zero"
  (c-calloc
   count
   (foreign-type-size
    (require-type type "allocate an instance of foreign type ~S" type))))

(defun alloc (type &optional (count 1))
  "Return a foreign wrapper for `TYPE` with its pointer allocated.
Freeing is up to you!"
  (if (foreign-scalar-p type)
      (alloc-ptr type count)
      (let ((wrapper (make-wrapper-instance (foreign-type-name (require-type type "allocate a wrapper for an instance of foreign type ~S" type)))))
        (setf (wrapper-ptr wrapper)
              (alloc-ptr type count))
        wrapper)))

(defun calloc (type &optional (count 1))
  "Return a foreign wrapper for `TYPE` with its pointer allocated, and
its contents initialized to zero.  Freeing is up to you!"
  (if (foreign-scalar-p type)
      (calloc-ptr type count)
      (let ((wrapper (make-wrapper-instance (foreign-type-name (require-type type "allocate a wrapper for an instance of foreign type ~S" type)))))
        (setf (wrapper-ptr wrapper)
              (calloc-ptr type count))
        wrapper)))

(defun realloc (ptr type count)
  (let ((size (foreign-type-size type)))
    (if (cffi:pointerp ptr)
        (c-realloc ptr (* count size))
        (setf (wrapper-ptr ptr)
              (c-realloc (ptr ptr) (* count size))))
    ptr))

(defun free (object)
  "Free WRAPPER via FOREIGN-FREE and invalidate."
  (c-free (ptr object))
  (when (wrapper-p object)
    (invalidate object))
  (values))

(declaim (inline sizeof))
(defun sizeof (type)
  (foreign-type-size (find-type type)))

(defmacro with-alloc ((name type &optional (count 1)) &body body)
  `(let ((,name (alloc ,type ,count)))
     (unwind-protect (progn ,@body)
       (free ,name))))

(defmacro with-calloc ((name type &optional (count 1)) &body body)
  `(let ((,name (calloc ,type ,count)))
     (unwind-protect (progn ,@body)
       (free ,name))))

(defmacro with-many-alloc ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (bind) `(,(car bind) (alloc ,@(cdr bind))))
          bindings)
     (unwind-protect (progn ,@body)
       ,@(mapcar #'(lambda (bind) `(free ,(car bind))) bindings))))

(defun c-aptr (wrapper index &optional (type (foreign-type-name wrapper)))
  (let ((size (foreign-type-size (require-type type "index into array of elements of type ~S" type))))
    (cffi-sys:inc-pointer (ptr wrapper) (* index size))))

(define-compiler-macro c-aptr (&whole whole wrapper index
                                      &optional type)
  (if (constantp type)
      (let ((size (foreign-type-size (require-type (eval type) "index into array of elements of type ~S" type))))
        `(cffi-sys:inc-pointer (ptr ,wrapper) (* ,index ,size)))
      whole))

(defun c-aref (wrapper index &optional (type (foreign-type-name wrapper)))
  (etypecase type
    (keyword
     (cffi-sys:%mem-ref (c-aptr wrapper index type) type))
    (t (wrap-pointer (c-aptr wrapper index type) type wrapper))))

(define-compiler-macro c-aref (&whole whole wrapper index
                                      &optional type)
  (if (constantp type)
      (etypecase (eval type)
        (keyword
         `(cffi-sys:%mem-ref (c-aptr ,wrapper ,index ,type) ,type))
        (t `(once-only (wrapper index)
              (wrap-pointer (c-aptr ,wrapper ,index ,type) ,type ,wrapper))))
      whole))

(defun (setf c-aref) (v ptr index type)
  (etypecase type
    (keyword
     (cffi-sys:%mem-set v (c-aptr ptr index type) type)
     v)))

(define-compiler-macro (setf c-aref) (&whole whole v ptr index type)
  (if (constantp type)
      (etypecase (eval type)
        (keyword (once-only (v)
                   `(progn
                      (cffi-sys:%mem-set ,v (c-aptr ,ptr ,index ,type) ,type)
                      ,v))))
      whole))

(defun memcpy (dest src &key (n 1) type)
  (let ((size (* n (if type
                       (sizeof type)
                       (sizeof src)))))
    (c-memcpy (ptr dest) (ptr src) size)))

(defun alloc-string (string)
  (let ((ptr (calloc :char (1+ (length string)))))
    (cffi:lisp-string-to-foreign string ptr (1+ (length string)))
    ptr))
