(in-package :autowrap)


(defun make-descriptor (&rest pairs &key &allow-other-keys)
  (plist-alist pairs))

(defun make-stub-struct (descriptor)
  (flet ((%val (name)
           (aval name descriptor)))
    (let ((bit-size (%val :bit-size))
          (bit-alignment (%val :bit-alignment)))

      (multiple-value-bind (byte-size remainder) (floor (/ bit-size 8))
        (unless (= remainder 0)
          (error "Unexpected bitsize: expected multiple of 8, but got ~a" bit-size))
        (make-descriptor
         :tag "struct"
         :ns (%val :ns)
         :name (%val :name)
         :id (%val :id)
         :location (%val :location)
         :bit-size bit-size
         :bit-alignment bit-alignment
         :fields (vector (make-descriptor
                          :tag "field"
                          :name "_data"
                          :bit-offset 0
                          :bit-size bit-size
                          :bit-alignment bit-alignment
                          :type (make-descriptor
                                 :tag ":array"
                                 :size byte-size
                                 :type (make-descriptor
                                        :tag ":char"
                                        :bit-size 8
                                        :bit-alignment 8)))))))))


(defun extract-field-type (descriptor)
  (switch ((aval :tag descriptor) :test #'equal)
    (":pointer" (list (extract-pointer-type (aval :type descriptor))))
    (":struct" (list (aval :name descriptor)))
    ("union" (extract-union-types descriptor))
    (t (list (aval :tag descriptor)))))

(defun extract-union-types (descriptor)
  (loop for field in (aval :fields descriptor)
     append (extract-field-type (aval :type field))))

(defun extract-typedef-type (descriptor)
  (switch ((aval :tag descriptor) :test #'equal)
    ("struct" (list (aval :name descriptor)))
    (":struct" (list (aval :name descriptor)))
    ("union" (extract-union-types descriptor))
    (":enum" (list (aval :name descriptor)))
    (t (list (aval :tag descriptor)))))

(defun extract-pointer-type (descriptor)
  (switch ((aval :tag descriptor) :test #'equal)
    ("struct" (aval :name descriptor))
    (":struct" (aval :name descriptor))
    (":pointer" (extract-pointer-type (aval :type descriptor)))
    (t (aval :tag descriptor))))

(defun extract-function-parameter-type (descriptor)
  (switch ((aval :tag descriptor) :test #'equal)
    (":pointer" (extract-pointer-type (aval :type descriptor)))
    (t (aval :tag descriptor))))

(defun extract-dependent-types (descriptor)
  (switch ((aval :tag descriptor) :test #'equal)
    ("typedef" (extract-typedef-type (aval :type descriptor)))
    ("struct"
     (loop for field in (aval :fields descriptor)
        append (extract-field-type (aval :type field))))
    ("function"
     (cons (extract-function-parameter-type (aval :return-type descriptor))
           (loop for parameter in (aval :parameters descriptor)
              collect (extract-function-parameter-type (aval :type parameter)))))))

(defun fill-name-set (raw-spec name-set)
  (loop for form in raw-spec
       as name = (aval :name form)
       as location = (aval :location form)
       unless (excluded-p name location)
       do (loop for name in (append (list name) (extract-dependent-types form))
             unless (or (emptyp name) (starts-with #\: name))
             do (setf (gethash name name-set) t))))

(defun follow-typedefs (raw-spec name-set)
  (loop with unregistered-typedef-found-p = nil
     for descriptor in raw-spec
     as tag = (aval :tag descriptor)
     as name = (aval :name descriptor)
     when (and (equal "typedef" tag) (gethash name name-set))
     do (loop for type in (extract-typedef-type (aval :type descriptor))
             unless (gethash type name-set)
             do (setf unregistered-typedef-found-p t
                      (gethash type name-set) t))
     finally (return unregistered-typedef-found-p)))

(defun extract-included-name-set (raw-spec)
  (let ((name-set (make-hash-table :test 'equal)))
    (fill-name-set raw-spec name-set)
    (loop while (follow-typedefs raw-spec name-set))
    name-set))

(defun process-descriptor (descriptor)
  (let ((type (aval :tag descriptor)))
    (if (and (excluded-p (aval :name descriptor) (aval :location descriptor))
             (not (or (equal "typedef" type)
                      (equal "enum" type)
                      (equal "function" type))))
        (make-stub-struct descriptor)
        descriptor)))

(defun squash-unrelated-definitions (input-spec-stream output-spec-stream)
  (let* ((raw-spec (read-json input-spec-stream))
         (name-set (extract-included-name-set raw-spec))
         (descriptors (loop for descriptor in raw-spec
                         when (gethash (aval :name descriptor) name-set)
                         collect (process-descriptor descriptor))))
    (json:encode-json descriptors output-spec-stream)))
