(in-package :autowrap)

(defvar *foreign-bitmasks* (make-hash-table))

 ;; Masks

;;; Bitmasks aren't a type so we're not tying into foreign-type
(defun define-bitmask (name mask)
  (setf (gethash name *foreign-bitmasks*)
        (sort (copy-list mask) (lambda (a b) (< (cdr a) (cdr b)))))
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

(defun mask-apply (name symbols)
  (apply #'mask name symbols))

 ;; Utility

(defconstant +debruijn64+ #x22fdd63cc95386d)
(defvar *bit-table*
  (let ((a (make-array 64 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 64 do
      (setf (aref a (ash (logand #xFFFFFFFFFFFFFFFF
                                 (ash +debruijn64+ i)) -58)) i))
    a))

(defun ctz (x)
  (declare (type (unsigned-byte 64) x))
  (aref *bit-table* (ash (logand #xFFFFFFFFFFFFFFFF
                                 (* +debruijn64+ (logand x (- x)))) -58)))

(defun mask-keywords (name value)
  "Return the list of keywords which describe the integer mask `VALUE`
for the bitmask called `NAME`.  Limited to 64 bits at the moment."
  (let ((bit (ash 1 (ctz value)))
        (bobs (find-bitmask name)))
    (loop while (and bobs (/= 0 value))
          if (= bit (cdar bobs)) collect (caar bobs) end
            do (when (>= bit (cdar bobs)) (pop bobs))
               (when (and bobs (< bit (cdar bobs)))
                 (setf value (logxor bit value))
                 (setf bit (ash 1 (ctz value)))))))

(defun bitmask-symbols-to-alist (list &optional regex)
  (let* ((scanner (ppcre:create-scanner "(\\W)(.*?)\\1"))
         (trimmed-symbols
           (mapcar (lambda (x)
                     (ppcre:regex-replace scanner (string x) "\\2"))
                   list))
         (keyword-symbols (mapcar #'make-keyword (prefix-trim trimmed-symbols :regex regex))))
    (loop for symbol in list
          for keyword in keyword-symbols
          collect ``(,',keyword . ,,symbol))))

(defmacro define-bitmask-from-constants ((name &optional regex) &body values)
  "Define a bitmask `NAME` from a list of constants `VALUES`.  Each
value should evaluate to actual values, e.g. actual `+CONSTANTS+`, or
be a list in the form `'(SYMBOL VALUE)`.  If a symbol is given alone,
it is by default pruned of any common prefix and made into a keyword.
If a list is specified, the symbol given is used exactly."
  (let* ((just-symbols (remove-if #'consp values))
         (just-alist (remove-if #'symbolp values))
         (symbol-values (bitmask-symbols-to-alist just-symbols regex)))
    `(define-bitmask ',name (list ,@symbol-values ,@just-alist))))

(defmacro define-bitmask-from-enum ((name enum-name) &body values)
  "Define a bitmask `NAME` from the values specified in the
previously-defined foreign-enum named `ENUM-NAME` and any additional
values, `VALUES`.  (A foreign-alias of an enum may also be
specified.)"
  (let ((enum-values (foreign-enum-values (find-type enum-name))))
    `(define-bitmask ',name `(,@',enum-values ,,@values))))

