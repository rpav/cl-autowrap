(in-package :autowrap)


(defun make-stub-struct (descriptor)
  "Copies bit information from descriptor and returns a struct descriptor that have single field
of array type taking all the struct available space"
  (flet ((%val (name)
           (aval name descriptor))
         (make-descriptor (&rest pairs &key &allow-other-keys)
           (plist-alist pairs)))
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


(defun extract-field-types (descriptor)
  (loop for field in (aval :fields descriptor)
     append (extract-type (aval :type field))))

(defun extract-type (type-descriptor)
  (switch ((aval :tag type-descriptor) :test #'equal)
    (":pointer" (list (extract-type (aval :type type-descriptor))))
    ("struct" (list (aval :name type-descriptor)))
    (":struct" (list (aval :name type-descriptor)))
    ("union" (extract-field-types type-descriptor))
    (":enum" (list (aval :name type-descriptor)))
    (t (list (aval :tag type-descriptor)))))

(defun extract-function-types (descriptor)
  (cons (extract-type (aval :return-type descriptor))
        (loop for parameter in (aval :parameters descriptor)
           collect (extract-type (aval :type parameter)))))

(defun extract-dependent-types (descriptor)
  (switch ((aval :tag descriptor) :test #'equal)
    ("typedef" (extract-type (aval :type descriptor)))
    ("struct" (extract-field-types descriptor))
    ("function" (extract-function-types descriptor))))

(defun fill-name-set (raw-spec name-set)
  "Extracts included names and their first-level dependencies into a set"
  (loop for form in raw-spec
       as name = (aval :name form)
       as location = (aval :location form)
       unless (excluded-p name location)
       do (loop for name in (cons name (extract-dependent-types form))
             unless (or (emptyp name) (starts-with #\: name))
             do (setf (gethash name name-set) t))))

(defun follow-typedefs (raw-spec name-set)
  "Adds typedef'ed type names into provided name set. Only existing typedef names are
added. E.g.

typedef original_t dependent_t;

Here 'original_t' name would be added only if 'dependent_t' already present in the name set"
  (loop with unregistered-typedef-found-p = nil
     for descriptor in raw-spec
     as tag = (aval :tag descriptor)
     as name = (aval :name descriptor)
     when (and (equal "typedef" tag) (gethash name name-set))
     do (loop for type in (extract-type (aval :type descriptor))
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
             (equal "struct" type))
        (make-stub-struct descriptor)
        descriptor)))

(defun squash-unrelated-definitions (input-spec-stream output-spec-stream)
  "Filters descriptors leaving only included ones and their dependencies. Stubs low-level
excluded structures essentially making them opaque bit-blobs."
  (let* ((raw-spec (read-json input-spec-stream))
         (name-set (extract-included-name-set raw-spec))
         (descriptors (loop for descriptor in raw-spec
                         when (gethash (aval :name descriptor) name-set)
                         collect (process-descriptor descriptor))))
    (json:encode-json descriptors output-spec-stream)))
