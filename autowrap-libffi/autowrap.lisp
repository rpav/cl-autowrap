(cl:in-package :autowrap.libffi)

(autowrap:c-include '(#:cl-autowrap/libffi #:autowrap-spec "libffi.h")
  :spec-path '(#:cl-autowrap/libffi #:autowrap-spec)
  :sysincludes '("/usr/lib64/libffi-3.2.1/include")

  :no-accessors cl:t
  :release-p cl:t)
