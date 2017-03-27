(in-package :autowrap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (> (hash-table-count *libffi-type-map*) 0)
    (let (signed unsigned signed-sizes unsigned-sizes)
      (map nil
           (lambda (x y z)
             (push (cons x y) signed)
             (push (cons x z) unsigned))
           '(1 2 4 8)
           (list ffi-type-sint8 ffi-type-sint16 ffi-type-sint32 ffi-type-sint64)
           (list ffi-type-uint8 ffi-type-uint16 ffi-type-uint32 ffi-type-uint64))

      (map nil
           (lambda (x)
             (push (cons x (foreign-type-size x)) signed-sizes))
           '(:char :short :int :long :long-long))
      (map nil
           (lambda (x)
             (push (cons x (foreign-type-size x)) unsigned-sizes))
           '(:unsigned-char :unsigned-short :unsigned-int :unsigned-long :unsigned-long-long))

      (loop for (type . size) in signed-sizes
            do (setf (gethash type *libffi-type-map*) (aval size signed)))
      (loop for (type . size) in unsigned-sizes
            do (setf (gethash type *libffi-type-map*) (aval size unsigned)))

      (progn
        (setf (gethash :pointer *libffi-type-map*) ffi-type-pointer)
        (setf (gethash :void *libffi-type-map*) ffi-type-void)
        (setf (gethash :float *libffi-type-map*) ffi-type-float)
        (setf (gethash :double *libffi-type-map*) ffi-type-double)
        (setf (gethash :long-double *libffi-type-map*) ffi-type-longdouble)))))

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
  (let ((largest-field (reduce (lambda (x y)
                                 (if (> (frf-bit-size x) (frf-bit-size y)) x y))
                               (foreign-record-fields ft))))
    (c-let ((type autowrap.libffi:ffi-type :calloc t)
            (elements :pointer :count 1 :calloc t))
      (setf (type :type) autowrap.libffi:+ffi-type-struct+)
      (setf (type :elements) (elements &))
      (setf (elements 0) (ensure-libffi-type (foreign-type largest-field))))))


