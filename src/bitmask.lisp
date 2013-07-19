(in-package :autowrap)

(defvar *foreign-bitmasks* (make-hash-table))

 ;; Masks

;;; Bitmasks aren't a type so we're not tying into foreign-type
(defun define-bitmask (name mask)
  (setf (gethash name *foreign-bitmasks*) mask)
  name)

(defun find-bitmask (name)
  (multiple-value-bind (mask exists-p)
      (gethash name *foreign-bitmasks*)
    (unless exists-p
      (error "Bitmask not found: ~S" name))
    mask))

(defun remove-bitmask (name)
  (remhash name *foreign-bitmasks*))

(defun mask-symbol-value (mask symbol)
  (or (aval symbol mask)
      (progn
        (warn "Unknown mask symbol ~S, treated as 0" symbol)
        0)))

(defun mask (name &rest symbols)
  "Create a mask by `LOGIOR` using the *list* of symbols `SYMBOLS`
from the bitmask `NAME`."
  (let ((mask (find-bitmask name)))
    (apply #'logior (mapcar (lambda (x) (mask-symbol-value mask x))
                            symbols))))

(define-compiler-macro mask (&whole whole name &rest symbols)
  (if (and (constantp name)
           (constantp symbols))
      (let ((symbols (eval symbols)))
        (let ((mask (find-bitmask (eval name))))
          (apply #'logior (mapcar (lambda (x) (mask-symbol-value mask x))
                                  symbols))))
      whole))

 ;; Utility

(defmacro define-bitmask-from-constants ((name &optional regex) &body values)
  "Define a bitmask `NAME` from a list of constants `VALUES`.  Each
value should evaluate to actual values, e.g. actual `+CONSTANTS+`, or
be a list in the form `'(SYMBOL VALUE)`.  If a symbol is given alone, it is
by default pruned of any common prefix and made into a keyword.  If a list
is specified, the symbol given is used exactly."
  (let ((just-symbols (remove-if #'consp values))
        (just-alist (remove-if #'symbolp values)))
    (let* ((scanner (ppcre:create-scanner "(\\W)(.*?)\\1"))
           (trimmed-symbols
             (mapcar (lambda (x)
                       (ppcre:regex-replace scanner (string x) "\\2"))
                     just-symbols))
           (keyword-symbols (mapcar #'make-keyword (prefix-trim trimmed-symbols :regex regex)))
           (symbol-values (loop for symbol in just-symbols
                                for keyword in keyword-symbols
                                collect ``(,',keyword . ,,symbol))))
      `(define-bitmask ',name (list ,@symbol-values ,@just-alist)))))
