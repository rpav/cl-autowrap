(in-package :autowrap)

;;; Repurposed from alloc.lisp ;)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (> (hash-table-count *libffi-type-map*) 0)
    (let (unsigned signed)
      (map nil
           (lambda (x)
             (push (cons (foreign-type-size x) x) unsigned))
           '(:unsigned-char :unsigned-short :unsigned-int :unsigned-long :unsigned-long-long))
      (map nil
           (lambda (x)
             (push (cons (foreign-type-size x) x) signed))
           '(:char :short :int :long :long-long))
      (setf (gethash :pointer *libffi-type-map*) ffi-type-pointer)
      (setf (gethash :void *libffi-type-map*) ffi-type-void)
      (setf (gethash :float *libffi-type-map*) ffi-type-float)
      (setf (gethash :double *libffi-type-map*) ffi-type-double)
      (setf (gethash :long-double *libffi-type-map*) ffi-type-longdouble)
      (setf (gethash (aval 1 signed) *libffi-type-map*)  ffi-type-sint8)
      (setf (gethash (aval 1 unsigned) *libffi-type-map*)  ffi-type-uint8)
      (setf (gethash (aval 2 signed) *libffi-type-map*)  ffi-type-sint16)
      (setf (gethash (aval 2 unsigned) *libffi-type-map*)  ffi-type-uint16)
      (setf (gethash (aval 4 signed) *libffi-type-map*)  ffi-type-sint32)
      (setf (gethash (aval 4 unsigned) *libffi-type-map*)  ffi-type-uint32)
      (setf (gethash (aval 8 signed) *libffi-type-map*)  ffi-type-sint64)
      (setf (gethash (aval 8 unsigned) *libffi-type-map*)  ffi-type-uint64))))

(defgeneric ensure-libffi-type (foreign-type)
  (:documentation "Find or create a libffi equivalent of `FOREIGN-TYPE`."))

(defmethod ensure-libffi-type :around (ft)
  (or (gethash ft *libffi-type-map*)
      (call-next-method)))

(defmethod ensure-libffi-type ((ft symbol))
  (when (keywordp ft)
    (gethash ft *libffi-type-map*)))

(defmethod ensure-libffi-type ((ft foreign-pointer))
  ffi-type-pointer)

(defmethod ensure-libffi-type ((ft foreign-array))
  ;; Note this doesn't get cached.  May need better handling.
  (let ((size (foreign-array-size ft))
        (type (ensure-libffi-type (foreign-type ft))))
    (c-let ((array autowrap.libffi:ffi-type :calloc t)
            (elements :pointer
                      :count (1+ size) ;; NULL TERMINATED!
                      :calloc t))
      (setf (array :type) autowrap.libffi:+ffi-type-struct+
            (array :elements) (elements &))
      (loop for i from 0 below size
            do (setf (elements i) type))
      array)))

(defmethod ensure-libffi-type ((ft foreign-enum))
  (ensure-libffi-type (basic-foreign-type ft)))

(defmethod ensure-libffi-type ((ft foreign-function))
  ffi-type-pointer)

(defmethod ensure-libffi-type ((ft foreign-alias))
  (ensure-libffi-type (foreign-type ft)))

(defmethod ensure-libffi-type ((ft foreign-record))
  (if (eq :struct (foreign-type ft))
      (ensure-libffi-struct ft)
      (ensure-libffi-union ft)))

(defun ensure-libffi-struct (ft)
  (let* ((fields (foreign-record-fields ft))
         (field-count (length fields)))
    (c-let ((type autowrap.libffi:ffi-type :calloc t)
            ;; FIXME: allocates too many in the case of duplicate fields
            (elements :pointer
                      :count (1+ field-count) ;; NULL TERMINATED!
                      :calloc t))
      (setf (type :type) autowrap.libffi:+ffi-type-struct+)
      (setf (type :elements) (elements &))
      (loop as offset = -1 then (frf-bit-offset field)
            for i from 0 below field-count
            for field in (reverse fields)
            do (unless (<= (frf-bit-offset field) offset)
                 (setf (elements i)
                       (ensure-libffi-type (foreign-type field)))))
      (setf (gethash ft *libffi-type-map*) type)
      type)))

(defun ensure-libffi-union (ft)
  (unless (every #'foreign-scalar-p (foreign-record-fields ft))
    (error "Complex union type: ~S" ft))
  (let ((largest-field (reduce (lambda (x y)
                                 (if (> (frf-bit-size x) (frf-bit-size y)) x y))
                               (foreign-record-fields ft))))
    (c-let ((type autowrap.libffi:ffi-type :calloc t)
            (elements :pointer :count 1 :calloc t))
      (setf (type :type) autowrap.libffi:+ffi-type-struct+)
      (setf (type :elements) (elements &))
      (setf (elements 0) (ensure-libffi-type (foreign-type largest-field))))))


