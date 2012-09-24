
(defmacro with-mock2 (&rest body)
  `(progn
     (let ((el-spec:args nil))
       ,@body
       )))

(defun el-spec:args-for-call (symbol)
  (assoc-default symbol el-spec:args)
  )

(defun el-spec:called-count (symbol)
  (length (assoc-default symbol el-spec:args))
  )

(defmacro defmock (symbol arglist &rest body)
  `(ad-safe-fset
    ',symbol
    (lambda ,arglist
      (push
       (cons ',symbol
             (append (assoc-default ',symbol el-spec:args)
                     (list (mapcar 'symbol-value ',arglist))))
       el-spec:args)
      ,@body)
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
