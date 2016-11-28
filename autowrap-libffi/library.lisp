(in-package :autowrap)

(cffi:define-foreign-library libffi
  (:darwin (:or (:framework "libffi") (:default "libffi")))
  (:unix "libffi.so")
  (:windows "libffi.dll")
  (t (:default "libffi")))

(cffi:use-foreign-library libffi) 
