
(require 'advice)

(defun el-spec:setup-mock (funcsym)
  (el-spec:put-args funcsym nil)
  (when (fboundp funcsym)
    (put funcsym 'el-spec:original-func (symbol-function funcsym))))

(defun el-spec:teardown-mock (funcsym)
  (el-spec:put-args funcsym nil)
  (let ((func (get funcsym 'el-spec:original-func)))
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

(defmacro defmock (symbol arglist &rest body)
  (declare (indent 0) (debug t))
  `(progn
     (push ',symbol el-spec:original-func)
     (el-spec:setup-mock ',symbol)
     (ad-safe-fset
      ',symbol
      (lambda ,arglist
        (el-spec:append-args ',symbol (list (mapcar 'symbol-value ',arglist)))
        ;; If body include interactive, it works well.
        ;; Undocumented behavior?
        ,@body)
      )
    ))

;;; how to use
;; (with-mock2
;;  (defmock test3 (a b)
;;    (message "%d:%d" a b))
;;  (test3 1 2)
;;  (test3 3 4)
;;  (should (eq (el-spec:called-count 'test3) 2))
;;  (should (equal (el-spec:args-for-call 'test3) '((1 2) (3 4))))
;;  )

(provide 'test-double)
