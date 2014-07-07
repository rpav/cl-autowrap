(in-package :autowrap)

;;; Basic invocation for c2ffi with some architecture-related
;;; stuff.

 ;; Arch

;;; Note this is rather untested and not very extensive at the moment;
;;; it should probably work on linux/win/osx though.  Patches welcome.

(defun local-cpu ()
  #+x86-64 "x86_64"
  #+(and (not (or x86-64 freebsd)) x86) "i686"
  #+(and (not x86-64) x86 freebsd) "i386"
  #+arm "arm")

(defun local-vendor ()
  #+(or linux windows) "-pc"
  #+darwin "-apple"
  #+(not (or linux windows darwin)) "-unknown")

(defun local-os ()
  #+linux "-linux"
  #+(and x86-64 windows) "-win64"
  #+(and x86 (not x86-64) windows) "-win32"
  #+darwin "-darwin9"
  #+freebsd "-freebsd")

(defun local-environment ()
  #+linux "-gnu"
  #-linux "")

(defun local-arch ()
  (string+ (local-cpu) (local-vendor) (local-os) (local-environment)))

(defparameter *known-arches*
  '("i686-pc-linux-gnu"
    "x86_64-pc-linux-gnu"
    "i686-pc-win32"
    "x86_64-pc-win64"
    "i686-apple-darwin9"
    "x86_64-apple-darwin9"
    "i386-unknown-freebsd"
    "x86_64-unknown-freebsd"))

 ;; c2ffi

(defvar *c2ffi-program* "c2ffi")

(defvar *trace-c2ffi* nil)

(defun run-check (program args &key output)
  (when *trace-c2ffi*
    (format *debug-io* "; ~A~{ ~A~}~%" program args))
  (= 0 (nth-value 1 (external-program:run program args :output output))))

(defun c2ffi-p ()
  "This is a hack to determine if c2ffi exists; it assumes if it
doesn't exist, we will get a return code other than 0."
  (= 0 (nth-value 1 (external-program:run *c2ffi-program* '("-h")))))

(defun write-c2ffi-tempfile (input-file macro-file)
  "Write a header including the given input-file and the macro-file
c2ffi will output.  Return the filename."
  (with-open-file (stream macro-file :direction :output :if-exists :supersede))
  (namestring
   (fad:with-output-to-temporary-file (stream)
     (format stream "#include \"~A\"~%" input-file)
     (format stream "#include \"~A\"~%" macro-file))))

(defun run-c2ffi (input-file output-basename &key arch sysincludes)
  "Run c2ffi on `INPUT-FILE`, outputting to `OUTPUT-FILE` and
`MACRO-OUTPUT-FILE`, optionally specifying a target triple `ARCH`."
  (let* ((output-h (string+ output-basename ".h"))
         (output-spec (string+ output-basename ".spec"))
         (include-file (write-c2ffi-tempfile input-file output-h))
         (arch (when arch (list "-A" arch)))
         (sysincludes (loop for dir in sysincludes
                            append (list "-i" dir))))
    (when (run-check *c2ffi-program* (list* include-file
                                    "-D" "null"
                                    "-M" output-h
                                    (append arch sysincludes))
                     :output *standard-output*)
      (prog1
          (run-check *c2ffi-program* (list* include-file "-o" output-spec
                                    (append arch sysincludes))
                     :output *standard-output*)
        (delete-file output-h)
        (delete-file include-file)))))

 ;; Specs and Loading

(defun find-local-spec (name &optional (spec-path *default-pathname-defaults*))
  "Return the path of the SPEC for this machine's architecture, or NIL
if the file does not exist."
  (let* ((arch (local-arch))
         (name (pathname-name name))
         (h-name (make-pathname :defaults spec-path
                                :name (string+ name "." arch)
                                :type "spec")))
    (when (probe-file h-name) h-name)))

(defun ensure-local-spec (name &key
                          (spec-path *default-pathname-defaults*)
                          arch-excludes
                          sysincludes
                          version)
  (flet ((spec-path (arch) (string+ (namestring spec-path)
                                    (pathname-name name)
                                    (if version
                                        (string+ "-" version)
                                        "")
                                    "." arch)))
    (multiple-value-bind (h-name m-name) (find-local-spec name spec-path)
      (if h-name
          (values h-name m-name)
          (progn
            (unless (c2ffi-p)
              (error "No spec for ~S on arch '~A' and c2ffi not found"
                     name (local-arch)))
            (let ((arch (local-arch)))
              (unless (run-c2ffi name (spec-path arch)
                                 :arch arch
                                 :sysincludes sysincludes)
                (error "Error running c2ffi on ~S" name)))
            (loop with local-arch = (local-arch)
                  for arch in *known-arches* do
                    (unless (or (string= local-arch arch)
                                (member arch arch-excludes :test #'string=))
                      (unless (run-c2ffi name (spec-path arch)
                                         :arch arch
                                         :sysincludes sysincludes)
                        (warn "Error generating spec for other arch: ~S" arch))))
            (if-let (h-name (find-local-spec name spec-path))
              h-name
              (error "Error finding spec for ~S after running c2ffi" name)))))))

