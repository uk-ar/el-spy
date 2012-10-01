
(require 'advice)
(require 'cl)
;;for should in el-spec:make-args-keylist
(require 'ert)

(defun el-spec:setup-mock (funcsym)
  (el-spec:put-args funcsym nil)
  (when (fboundp funcsym)
    (put funcsym 'el-spec:original-func (symbol-function funcsym))))

(defun el-spec:get-original-func (symbol)
  (get symbol 'el-spec:original-func))

(defun el-spec:teardown-mock (funcsym)
  (el-spec:put-args funcsym nil)
  (let ((func (el-spec:get-original-func funcsym)))
    (if func
        (ad-safe-fset funcsym func)
      (fmakunbound funcsym))))

(defmacro with-mock2 (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     (let ((el-spec:original-func nil))
       (unwind-protect
           (progn ,@body)
         (mapc #'el-spec:teardown-mock el-spec:original-func)
         ))))

(defun el-spec:get-args (symbol)
  (get symbol 'el-spec:args))
(defalias 'el-spec:args-for-call 'el-spec:get-args)

(defun el-spec:put-args (symbol args)
  (put symbol 'el-spec:args args))

(defun el-spec:append-args (symbol arglist)
  (el-spec:put-args
   symbol
   (append (get symbol 'el-spec:args) arglist)))

(defun el-spec:not-called (symbol)
  (eq (length (el-spec:get-args symbol)) 0))

(defun el-spec:called-count (symbol)
  (length (el-spec:get-args symbol)))

(defvar el-spec:func-name nil)

(defmacro defmock (symbol arglist &rest body)
  (declare (indent defun) (debug t))
  `(progn
     (unless (boundp 'el-spec:original-func)
       (error "not in with-mock"))
     (push ',symbol el-spec:original-func)
     (el-spec:setup-mock ',symbol)
     (ad-safe-fset
      ',symbol
      (lambda ,arglist
        (setq el-spec:func-name ',symbol)
        (el-spec:append-args ',symbol (list (mapcar 'symbol-value ',arglist)))
        ;; Even if body include interactive, it works well.
        ;; Undocumented behavior?
        ,@body)
      )))

(defun el-spec:make-returns-keylist (list default)
  (append
   (let ((i 0))
     (mapcar (lambda (elem) (list (incf i) elem)) list))
   `((t ,default))))

(defun el-spec:make-args-keylist (arg list default)
  (el-spec:make-returns-keylist
   (mapcar (lambda (elem) (list 'should `(equal ,arg ,elem))) list) 6)
  )

(defmacro el-spec:returns (list default)
  `(case (el-spec:called-count el-spec:func-name)
     ,@(el-spec:make-returns-keylist list default)))

(defmacro el-spec:args (symbol list default)
  `(case (el-spec:called-count el-spec:func-name)
     ,@(el-spec:make-args-keylist symbol list default)))

;; el-mock limitation
;; (with-mock
;;   (mock (test1) :times 0)
;;   (call-interactively 'test1);; error
;;  )
(provide 'test-double)
