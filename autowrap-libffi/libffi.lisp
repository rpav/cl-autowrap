(in-package :autowrap)

(setq *build-libffi-call*
      (lambda (fun param-names vargs)
        (build-libffi-call fun param-names vargs)))

(defun build-libffi-call (fun param-names vargs)
  (with-slots (c-symbol fields) fun
    (let ((argc (length fields)))
      )))
