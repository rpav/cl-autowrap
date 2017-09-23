(in-package :autowrap)


;;;
;;; Dependency graph
;;;
(defstruct (type-dependency
             (:type list))
  (weak-p nil)
  (dependent-name "" :type string))

(defun make-dependency-graph ()
  (make-hash-table :test 'equal))

(defun name-exist-p (graph name)
  (nth-value 1 (gethash name graph)))

(defun register-type (graph name)
  (unless (name-exist-p graph name)
    (setf (gethash name graph) nil)))

(defun register-dependency (graph type-name dependent-type-name &key weak-p)
  (pushnew (make-type-dependency :weak-p weak-p
                                 :dependent-name dependent-type-name)
           (gethash type-name graph)
           :test 'equal)
  (unless (nth-value 1 (gethash dependent-type-name graph))
    (setf (gethash dependent-type-name graph) nil)))

(defun exclude-type (graph name)
  (let ((children (gethash name graph)))
    (remhash name graph)
    (loop for child in children
       unless (type-dependency-weak-p child)
       do (exclude-type graph child))))

(defun traverse-dependencies (graph root-name probe)
  (labels ((%traverse-children (root-name extended-path)
             (loop for dependent in (gethash root-name graph)
                as name = (type-dependency-dependent-name dependent)
                as weak-dep = (type-dependency-weak-p dependent)
                as (result stop-traverse-p) =
                  (if weak-dep
                      (list nil nil)
                      (multiple-value-list (%traverse-graph name extended-path)))
                until stop-traverse-p
                finally (return (values result stop-traverse-p))))
           (%traverse-graph (root-name path)
             (if (member root-name path :test 'equal)
                 (values nil nil)
                 (multiple-value-bind (result stop-p) (funcall probe root-name)
                   (if stop-p
                       (values result t)
                       (%traverse-children root-name (cons root-name path)))))))
    (%traverse-graph root-name nil)))

;;;
;;; Inclusion rules
;;;
(defun explicitly-included-p (name location)
  (or (included-p name *include-definitions*)
      (and (included-p location *include-sources*)
           (not (included-p name *exclude-definitions*)))))

(defun explicitly-excluded-p (name location)
  (or (included-p name *exclude-definitions*)
      (and (included-p location *exclude-sources*)
           (not (included-p name *include-definitions*)))))

(defun finally-included-p (name location)
  (and (explicitly-included-p name location)
       (not (explicitly-excluded-p name location))))

;;;
;;; Type extraction
;;;
(defun extract-field-types (descriptor)
  (loop for field in (aval :fields descriptor)
     append (extract-type (aval :type field))))

(defun extract-function-types (descriptor)
  (append (extract-type (aval :return-type descriptor))
          (loop for parameter in (aval :parameters descriptor)
               append (extract-type (aval :type parameter)))))

(defun extract-struct-types (descriptor)
  (cons (aval :name descriptor) (extract-field-types descriptor)))

(defun extract-type (type-descriptor)
  (switch ((aval :tag type-descriptor) :test #'equal)
    (":array" (extract-type (aval :type type-descriptor)))
    (":pointer" (extract-type (aval :type type-descriptor)))
    ("struct" (extract-struct-types type-descriptor))
    (":struct" (extract-struct-types type-descriptor))
    ("union" (extract-field-types type-descriptor))
    (":enum" (list (aval :name type-descriptor)))
    ("enum" (list (aval :name type-descriptor)))
    (t (list (aval :tag type-descriptor)))))

(defun extract-dependencies (descriptor)
  (switch ((aval :tag descriptor) :test #'equal)
    ("typedef" (extract-type (aval :type descriptor)))
    ("struct" (extract-field-types descriptor))
    ("function" (extract-function-types descriptor))))

(defun extract-types (raw-spec)
  "Extracts names and their first-level dependencies into a graph."
  (loop with dependency-graph = (make-dependency-graph)
     with types = (make-hash-table :test 'equal)
     for form in raw-spec
     as name = (aval :name form)
     as tag = (aval :tag form)
     as location = (aval :location form)
     do (let ((weak-dependency-p (equal "struct" tag)))
          (setf (gethash name types) form)
          (register-type dependency-graph name)
          (loop for dependency in (extract-dependencies form)
             unless (or (emptyp dependency)
                        (starts-with #\: dependency))
                 do (register-dependency dependency-graph dependency name :weak-p weak-dependency-p)))
     finally (return (values dependency-graph types))))

(defun filter-types (types dependencies)
  "Leaves only included names and their dependencies unless explicitly excluded."
  (flet ((name-included (name)
           (let ((location (aval :location (gethash name types))))
             (if (explicitly-included-p name location)
                 (values t t)
                 (values nil nil)))))
    (loop for name being the hash-key of types
       as location = (aval :location (gethash name types))
         when (or (explicitly-excluded-p name location)
                  (not (traverse-dependencies dependencies name #'name-included)))
         do (exclude-type dependencies name)
       finally (return dependencies))))

;;;
;;; Spec filtering
;;;
(defun filter-struct-fields (descriptor dependencies)
  (let ((fields (loop for field in (aval :fields descriptor)
                   as type-descriptor = (aval :type field)
                   when (loop for name in (extract-type type-descriptor)
                           thereis (or (starts-with #\: name)
                                       (name-exist-p dependencies name)))
                   collect (let ((inner-descriptor (process-descriptor type-descriptor dependencies)))
                             (prog1 field
                               (setf (aval :type field) inner-descriptor))))))
    (setf (aval :fields descriptor) fields)
    descriptor))

(defun process-descriptor (descriptor dependencies)
  (let ((type (and descriptor (aval :tag descriptor))))
    (cond
      ((or (equal "struct" type)
           (equal "union" type))
       (filter-struct-fields descriptor dependencies))
      (t descriptor))))

(defun squash-unrelated-definitions (input-spec-stream output-spec-stream)
  "Filters descriptors leaving only included ones and their dependencies."
  (let ((raw-spec (read-json input-spec-stream)))
    (multiple-value-bind (dependencies types) (extract-types raw-spec)
      (let* ((filtered-dependencies (filter-types types dependencies))
             (descriptors (loop for descriptor in raw-spec
                             as name = (aval :name descriptor)
                             when (or (emptyp name)
                                      (name-exist-p filtered-dependencies name))
                             collect (process-descriptor descriptor filtered-dependencies))))
        (json:encode-json descriptors output-spec-stream)))))
