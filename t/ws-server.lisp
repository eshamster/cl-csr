(defpackage cl-csr/t/ws-server
  (:use :cl
        :rove
        :cl-csr/ws-server))
(in-package :cl-csr/t/ws-server)

(deftest test-same-target-client-list-p
  (ok (same-target-client-list-p '(1 2) '(1 2)))
  (ok (same-target-client-list-p '(1 2) '(2 1)))
  (ok (same-target-client-list-p :all :all))
  (ok (not (same-target-client-list-p '(1 2) '(2 3))))
  (ok (not (same-target-client-list-p '(1 2) '(1 2 3))))
  (ok (not (same-target-client-list-p :all '(1 2))))
  (ok (not (same-target-client-list-p '(1 2) :all))))

(deftest test-calc-common-target
  (ok (equalp (calc-common-target :all '(1 2))
              '(1 2)))
  (ok (equalp (calc-common-target '(1 2) :all)
              '(1 2)))
  (ok (equalp (calc-common-target '(1 2) '(2 3))
              '(2)))
  (ok (equalp (calc-common-target '(1 2) '(3 4))
              nil)))
