(in-package :autowrap)

(setq *build-libffi-call*
      (lambda (fun param-names vargs)
        (build-libffi-call fun param-names vargs)))

(defun build-libffi-call (fun param-names vargs)
  (declare (ignore vargs))
  (with-slots (c-symbol name fields) fun
    (ensure-cif fun)
    (let (alloc-params
          set-params
          (voidp (eq :void (foreign-type fun))))
      (with-gensyms (cif ret args function-pointer)
        (loop for name in param-names
              for field in fields
              for i from 0
              as c-name = (gensym)
              as type = (foreign-type field)
              as scalarp = (foreign-scalar-p type)
              if scalarp
              do (push `(,c-name ,type :value ,name) alloc-params)
                 (push `(setf ,c-name ,name) set-params)
                 (push `(setf (,args ,i) (,c-name &)) set-params)
              else
              do (push `(setf (,args ,i) (ptr ,name)) set-params)
              end
              finally
                 (nreversef alloc-params)
                 (nreversef set-params))
        `(let ((,cif (gethash ',name *libffi-cif*)))
           (c-with ((,args :pointer :count ,(length param-names) :calloc t)
                    ,@(unless voidp
                        `((,ret ,(foreign-type-name (foreign-type fun)) :calloc t :free nil)))
                    ,@alloc-params)
             ,@set-params
             (if-let ((,function-pointer (cffi-sys:%foreign-symbol-pointer ,c-symbol :default)))
               (progn
                 (autowrap.libffi:ffi-call ,cif
                                           ,function-pointer
                                           ,(unless voidp `(print (ptr ,ret)))
                                           (,args &))
                 ,(unless voidp ret))
               (error "Calling foreign function via libffi:~%Symbol not loaded: ~S" ,c-symbol))
             ))))))

(defun ensure-cif (fun)
  (with-slots (name fields (return-type type)) fun
    (or (gethash name *libffi-cif*)
        (let* ((sig (ensure-sig fields)))
          (c-let ((cif autowrap.libffi:ffi-cif :calloc t))
            (let ((status (autowrap.libffi:ffi-prep-cif cif
                                                        autowrap.libffi:+ffi-default-abi+
                                                        (length fields)
                                                        (ensure-libffi-type return-type)
                                                        sig)))
              (if (= status autowrap.libffi:+ffi-ok+)
                  (progn
                    (setf (gethash name *libffi-cif*) cif)
                    cif)
                  (progn
                    (free cif)
                    (error "Error creating CIF for ~S: ~S"
                           (foreign-type-name fun)
                           (enum-key 'autowrap.libffi:ffi-status status))))))))))

(defun ensure-sig (fields)
  (let ((type-names (mapcar (lambda (x)
                              (foreign-type-name (foreign-type x)))
                            fields)))
    (or (gethash type-names *libffi-sigs*)
        (c-let ((args :pointer :count (length fields)))
          (loop for i from 0 below (length fields)
                for field in fields
                do (setf (args i) (ensure-libffi-type (foreign-type field))))
          (setf (gethash type-names *libffi-sigs*) (args &))
          (args &)))))
