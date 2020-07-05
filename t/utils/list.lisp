(defpackage cl-csr/t/utils/list
  (:use :cl
        :rove
        :cl-csr/utils/list))
(in-package :cl-csr/t/utils/list)

(deftest test-make-plist-by-param
  (ok (equal (let ((a 1)
                   (b 2)
                   (c 3))
               (make-plist-by-param a b c))
             '(:a 1 :b 2 :c 3))))

(deftest test-plist-to-nested-hash-table
  (let ((res (plist-to-nested-hash-table '(:a 0 :b 1 :c 2))))
    (ok (= (gethash :b res) 1)))
  (let ((res (plist-to-nested-hash-table '(:a 0 :b (:b1 10 :b2 20) :c 2))))
    (ok (= (gethash :b2 (gethash :b res))
           20))))
