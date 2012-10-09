(require 'ert)
(require 'el-spy)

;; test for el-spy
(defun test1 (a b) 'test1-original)
(defun test2 (a b) 'test2-original)

(defadvice test1 (around test1-ad () disable)
  (setq ad-return-value 'test1-advice))

(ert-deftest simple ();; spy
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b) 'test1-mock)

    (should (eq (test1 1 2) 'test1-mock))
    (should (eq (el-spy:called-count 'test1) 1))
    (should (equal (el-spy:get-args 'test1) '((1 2))))

    (should (eq (test1 3 4) 'test1-mock))
    (should (eq (el-spy:called-count 'test1) 2))
    (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
    )
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  )

(ert-deftest proxy ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
      (funcall (el-spy:get-original-func 'test1) a b))

    (should (eq (test1 1 2) 'test1-original))
    (should (eq (el-spy:called-count 'test1) 1))
    (should (equal (el-spy:get-args 'test1) '((1 2))))

    (should (eq (test1 3 4) 'test1-original))
    (should (eq (el-spy:called-count 'test1) 2))
    (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
    )
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  )

(ert-deftest advice1 ()
  (should (eq (test1 1 1) 'test1-original))
  (ad-enable-advice 'test1 'around 'test1-ad)
  (ad-activate 'test1)
  (unwind-protect
      (progn
        (should (eq (test1 2 2) 'test1-advice))
        (with-mock2

          (defmock test1 (a b) 'test1-mock)

          (should (eq (test1 1 2) 'test1-mock))
          (should (eq (el-spy:called-count 'test1) 1))
          (should (equal (el-spy:get-args 'test1) '((1 2))))

          (should (eq (test1 3 4) 'test1-mock))
          (should (eq (el-spy:called-count 'test1) 2))
          (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
          )
        (should (eq (test1 1 2) 'test1-advice))
        )
    (ad-disable-advice 'test1 'around 'test1-ad)
    (ad-activate 'test1)
    (should (eq (test1 3 2) 'test1-original))
    (should (equal (el-spy:get-args 'test1) nil))
    )
  )

(ert-deftest advice2 ()
  (should (eq (test1 1 1) 'test1-original))
  (ad-enable-advice 'test1 'around 'test1-ad)
  (ad-activate 'test1)
  (unwind-protect
      (progn
        (should (eq (test1 2 2) 'test1-advice))
        (with-mock2

          (defmock test1 (a b) 'test1-mock)

          (should (eq (test1 1 2) 'test1-mock))
          (should (eq (el-spy:called-count 'test1) 1))
          (should (equal (el-spy:get-args 'test1) '((1 2))))

          (ad-disable-advice 'test1 'around 'test1-ad)
          ;; (ad-activate 'test1);limitation of ad-safe-fset?

          (should (eq (test1 3 4) 'test1-mock))
          (should (eq (el-spy:called-count 'test1) 2))
          (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
          )
        (should (eq (test1 1 2) 'test1-advice))
        )
    (ad-disable-advice 'test1 'around 'test1-ad)
    (ad-activate 'test1)
    (should (eq (test1 3 2) 'test1-original))
    (should (equal (el-spy:get-args 'test1) nil))
    )
  )

(ert-deftest advice3 ()
  (should (eq (test1 1 1) 'test1-original))
  (unwind-protect
      (progn
        (with-mock2

          (defmock test1 (a b) 'test1-mock)

          (should (eq (test1 1 2) 'test1-mock))
          (should (eq (el-spy:called-count 'test1) 1))
          (should (equal (el-spy:get-args 'test1) '((1 2))))

          ;; (ad-enable-advice 'test1 'around 'test1-ad)
          ;; (ad-activate 'test1);fails in 2nd time

          ;; ;; adviced
          ;; (should (eq (test1 3 4) 'test1-advice))
          ;; ;; not record
          ;; (should (eq (el-spy:called-count 'test1) 1))
          ;; (should (equal (el-spy:get-args 'test1) '((1 2))))
          )
        (ad-disable-advice 'test1 'around 'test1-ad)
        (ad-activate 'test1)
        )
    (should (eq (test1 3 2) 'test1-original))
    (should (equal (el-spy:get-args 'test1) nil))
    ))

(ert-deftest interactive ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
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
    (should (equal (el-spy:get-args 'test1) '((1 2))))
    (should (equal (el-spy:get-args 'test2) '((3 4))))

    (should (eq (test1 3 4) 'test1-mock))
    (should (eq (test2 5 6) 'test2-mock))
    (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
    (should (equal (el-spy:get-args 'test2) '((3 4) (5 6))))
    )
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  )

(ert-deftest assert-fail1 ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (should-error
   (with-mock2
     (defmock test1 (a b) 'test1-mock)

     (should (eq (test1 1 3) 'test1-mock))
     (should (equal (el-spy:get-args 'test1) '((1 2))))

     (should (eq (test1 3 4) 'test1-mock))
     (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
     )
   :type 'ert-test-failed)
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  )

(ert-deftest assert-fail2 ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (should-error
   (with-mock2
     (defmock test1 (a b) 'test1-mock)

     (should (eq (test1 1 2) 'test1-mock))
     (should (equal (el-spy:get-args 'test1) '((1 2))))

     (should (eq (test1 3 3) 'test1-mock))
     (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
     )
   :type 'ert-test-failed)
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  )

(ert-deftest wrong-arg-error ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (should-error
   (with-mock2
     (defmock test1 (a b) 'test1-mock)

     (should (eq (test1 1 2) 'test1-mock))
     (should (equal (el-spy:get-args 'test1) '((1 2))))

     (should (eq (test1 3 4) 'test1-mock))
     (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))

     (test1 2 3 4)
     )
   :type 'wrong-number-of-arguments)
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  )

(ert-deftest get-func-name ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
      el-spy:func-name)
    (should (eq (test1 3 4) 'test1)))
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  )

(ert-deftest precheck-args ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
      (should (eq a 1)))
    (should (eq (test1 1 2) t))
    (should-error (test1 3 4) :type 'ert-test-failed)
    (should (eq (el-spy:called-count 'test1) 2))
    (should (equal (el-spy:get-args 'test1) '((1 2) (3 4))))
    )
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil)))

(ert-deftest sequensial-return1 ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
      (case (el-spy:called-count el-spy:func-name)
        (1 "a")
        (2 "b")
        (t "c")))
    (should (equal (test1 1 2) "a"))
    (should (equal (test1 1 2) "b"))
    (should (equal (test1 1 2) "c"))
    (should (equal (test1 1 2) "c")))
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil)))

(ert-deftest test-make-args-keylist ()
  (should (equal (el-spy:make-args-keylist 'x '(3 4) 6)
                 '((1 (should (equal x 3))) (2 (should (equal x 4))) (t 6)))))

(ert-deftest test-make-returns-keylist ()
  (should (equal (el-spy:make-returns-keylist '(4 5) 6)
                 '((1 4) (2 5) (t 6)))))

(ert-deftest sequensial-args ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
      (el-spy:args a (1 3 5) "c")
      )
    (should (test1 1 2))
    (should (test1 3 2))
    (should-error (test1 6 2) :type 'ert-test-failed)
    )
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil)))

(ert-deftest sequensial-return2 ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
      (el-spy:returns ("a" "b") "c"))
    (should (equal (test1 1 2) "a"))
    (should (equal (test1 1 2) "b"))
    (should (equal (test1 1 2) "c"))
    (should (equal (test1 1 2) "c")))
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil)))

(ert-deftest sequensial-return-not-over ()
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil))
  (with-mock2
    (defmock test1 (a b)
      (el-spy:returns ("a" "b") (should nil)))
    (should (equal (test1 1 2) "a"))
    (should (equal (test1 1 2) "b"))
    (should-error (test1 1 2) :type 'ert-test-failed))
  (should (eq (test1 1 2) 'test1-original))
  (should (equal (el-spy:get-args 'test1) nil)))

