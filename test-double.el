
(require 'advice)
(require 'cl)
;;for should in el-spec:make-args-keylist
(require 'ert)

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
     (push ',symbol el-spec:original-func)
     (el-spec:setup-mock ',symbol)
     (ad-safe-fset
      ',symbol
      (lambda ,arglist
        (setq el-spec:func-name ',symbol)
        (el-spec:append-args ',symbol (list (mapcar 'symbol-value ',arglist)))
        ;; If body include interactive, it works well.
        ;; Undocumented behavior?
        ,@body)
      )
    ))

;; test for test-double
(defun test1 (a b) 'test1-original)
(defun test2 (a b) 'test2-original)

(ert-deftest simple ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spec:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b) 'test1-mock)

    (should (eq (test1 1 2) 'test1-mock))
    (should (eq (el-spec:called-count 'test1) 1))
    (should (equal (el-spec:get-args 'test1) '((1 2))))

    (should (eq (test1 3 4) 'test1-mock))
    (should (eq (el-spec:called-count 'test1) 2))
    (should (equal (el-spec:get-args 'test1) '((1 2) (3 4))))
    )
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spec:get-args 'test1) nil))
  )

(ert-deftest interactive ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spec:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
             (interactive)
             'test1-mock)
    (defmock test2 (a b)
             'test2-mock)

    (should (commandp 'test1))
    (should-not (commandp 'test2))

    (should (eq (test1 1 2) 'test1-mock))
    (should (eq (test2 3 4) 'test2-mock))
    (should (equal (el-spec:get-args 'test1) '((1 2))))
    (should (equal (el-spec:get-args 'test2) '((3 4))))

    (should (eq (test1 3 4) 'test1-mock))
    (should (eq (test2 5 6) 'test2-mock))
    (should (equal (el-spec:get-args 'test1) '((1 2) (3 4))))
    (should (equal (el-spec:get-args 'test2) '((3 4) (5 6))))
    )
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spec:get-args 'test1) nil))
  )

(ert-deftest simple-error ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spec:get-args 'test1) nil))
  (should-error
   (with-mock2
     (defmock test1 (a b) 'test1-mock)

     (should (eq (test1 1 2) 'test1-mock))
     (should (equal (el-spec:get-args 'test1) '((1 2))))

     (should (eq (test1 3 4) 'test1-mock))
     (should (equal (el-spec:get-args 'test1) '((1 2) (3 4))))
     ))
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spec:get-args 'test1) nil))
  )

;; el-mock limitation
;; (with-mock
;;   (mock (test1) :times 0)
;;   (call-interactively 'test1);; error
;;  )

(provide 'test-double)
