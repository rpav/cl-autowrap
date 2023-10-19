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
  #+(and ppc64 big-endian) "powerpc64"
  #+(and ppc64 little-endian) "powerpc64le"
  #+arm "arm"
  #+arm64 "aarch64")

(defun local-vendor ()
  #+(or linux windows) "-pc"
  #+darwin "-apple"
  #+(not (or linux windows darwin)) "-unknown")

(defun local-os ()
  #+(or linux android) "-linux"
  #+windows "-windows-msvc"
  #+darwin "-darwin9"
  #+freebsd "-freebsd"
  #+openbsd "-openbsd")

(defun local-environment ()
  #+linux "-gnu"
  #+(and arm android) "-androideabi"
  #+(and (not arm) android) "-android"
  #+(not (or linux android)) "")

(defun local-arch ()
  (string+ (local-cpu) (local-vendor) (local-os) (local-environment)))

(defparameter *known-arches*
  '("i686-pc-linux-gnu"
    "x86_64-pc-linux-gnu"
    "i686-pc-windows-msvc"
    "x86_64-pc-windows-msvc"
    "i686-apple-darwin9"
    "x86_64-apple-darwin9"
    "i386-unknown-freebsd"
    "x86_64-unknown-freebsd"
    "i386-unknown-openbsd"
    "x86_64-unknown-openbsd"
    "arm-pc-linux-gnu"
    "aarch64-pc-linux-gnu"
    "arm-unknown-linux-androideabi"
    "aarch64-unknown-linux-android"
    "powerpc64-pc-linux-gnu"
    "powerpc64le-pc-linux-gnu"
    "i686-unknown-linux-android"
    "x86_64-unknown-linux-android"))

 ;; c2ffi

(defvar *autowrap-arch-override* nil)

(defvar *c2ffi-program* "c2ffi")

(defvar *trace-c2ffi* nil)

(defun run-check (program args &key output ignore-error-status)
  (when *trace-c2ffi*
    (format *debug-io* "~&; Invoking: ~A~{ ~A~}~%" program args))
  (zerop (nth-value 2 (uiop:run-program (list* program args) :output output :ignore-error-status ignore-error-status))))

(defun c2ffi-p ()
  "This is a hack to determine if c2ffi exists; it assumes if it
doesn't exist, we will get a return code other than 0."
  (zerop (nth-value 2 (uiop:run-program `(,*c2ffi-program* "-h") :ignore-error-status t))))

;;; UIOP:WITH-TEMPORARY-FILE does not seem to compile below as of asdf
;;; 3.1.5, and that's what SBCL is distributed with, so.
(defmacro with-temporary-file ((&key pathname keep (stream (gensym "STREAM") streamp)) &body body)
  `(uiop:call-with-temporary-file
    (lambda (,stream ,pathname)
      (unless ,streamp (close ,stream))
      ,@body)
    :keep ,keep
    :type "c"))

(defun run-c2ffi (input-file output-basename &key arch sysincludes ignore-error-status)
  "Run c2ffi on `INPUT-FILE`, outputting to `OUTPUT-FILE` and
`MACRO-OUTPUT-FILE`, optionally specifying a target triple `ARCH`."
  (with-temporary-file (:pathname tmp-macro-file
                        :keep *trace-c2ffi*)
    (let* ((output-spec (string+ output-basename ".spec"))
           (arch (when arch (list "-A" arch)))
           (sysincludes (loop for dir in sysincludes
                              append (list "-i" dir))))
      ;; Invoke c2ffi to emit macros into TMP-MACRO-FILE
      (when (run-check *c2ffi-program* (list* (namestring input-file)
                                              "-D" "null"
                                              "-M" (namestring tmp-macro-file)
                                              (append arch sysincludes))
                       :output *standard-output*
                       :ignore-error-status ignore-error-status)
        ;; Write a tmp header file that #include's the input file and the macros file.
        (with-temporary-file (:stream tmp-include-file-stream
                              :pathname tmp-include-file
                              :keep *trace-c2ffi*)
          (format tmp-include-file-stream "#include \"~A\"~%" input-file)
          (format tmp-include-file-stream "#include \"~A\"~%" tmp-macro-file)
          (close tmp-include-file-stream)
          ;; Invoke c2ffi again to generate the final output.
          (run-check *c2ffi-program* (list* (namestring tmp-include-file) "--lang" "c" "--output" output-spec
                                            (append arch sysincludes))
                     :output *standard-output*
                     :ignore-error-status ignore-error-status))))))

 ;; Specs and Loading

(defun find-local-spec (name &optional (spec-path *default-pathname-defaults*))
  "Return the path of the SPEC for this machine's architecture, or NIL
if the file does not exist."
  (let* ((arch (or *autowrap-arch-override* (local-arch)))
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
              (run-c2ffi name (spec-path arch)
                         :arch arch
                         :sysincludes sysincludes))
            (loop with local-arch = (local-arch)
                  for arch in *known-arches* do
                    (unless (or (string= local-arch arch)
                                (member arch arch-excludes :test #'string=))
                      (unless (run-c2ffi name (spec-path arch)
                                         :arch arch
                                         :sysincludes sysincludes
                                         :ignore-error-status t)
                        (warn "Error generating spec for other arch: ~S" arch))))
            (if-let (h-name (find-local-spec name spec-path))
              h-name
              (error "Error finding spec for ~S after running c2ffi" name)))))))
