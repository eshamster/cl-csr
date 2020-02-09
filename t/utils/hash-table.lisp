(defpackage cl-csr/t/utils/hash-table
  (:use :cl
        :rove
        :cl-csr/utils/hash-table))
(in-package :cl-csr/t/utils/hash-table)

(deftest test-downcase-hash-keys
  (let ((table (make-hash-table))
        (inner-table (make-hash-table)))
    (setf (gethash :a table) 0)
    (setf (gethash :b table) inner-table)
    (setf (gethash :b1 inner-table) 10)
    (setf (gethash :b2 inner-table) 20)
    (let ((res (downcase-hash-keys table)))
      (ok (not (gethash :a res)))
      (ok (= (gethash :|a| res) 0))
      (ok (= (gethash :|b1| (gethash :|b| res)) 10)))))
