(in-package :autowrap)

 ;; Errno

;;; Note that I realize this may not work for FreeBSD 7 (?) .. but
;;; there needs to be some way to determine that's what we're on.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ (or linux freebsd)
  (progn
    (define-foreign-function '(__errno_location "__errno_location") :pointer nil)
    (define-cfun __errno_location))

  #+windows
  (progn
    (define-foreign-function '(_errno "_errno") :pointer nil)
    (define-cfun _errno))

  #+darwin
  (progn
    (define-foreign-function '(__error "__error") :pointer nil)
    (define-cfun __error)))

(defun get-errno-pointer ()
  #+(or linux freebsd)
  (__errno_location)

  #+windows
  (_errno)

  #+darwin
  (__error))

(define-symbol-macro errno
    (cffi-sys:%mem-ref (get-errno-pointer) :int))
