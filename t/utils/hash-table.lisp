(defpackage cl-csr/t/utils/hash-table
  (:use :cl
        :rove
        :cl-csr/utils/hash-table))
(in-package :cl-csr/t/utils/hash-table)

(deftest test-downcase-hash-keys-and-values
  (let ((table (make-hash-table))
        (inner-table (make-hash-table)))
    (setf (gethash :a1 table) 0)
    (setf (gethash :a2 table) :val-a2)
    (setf (gethash :b table) inner-table)
    (setf (gethash :b1 inner-table) 10)
    (setf (gethash :b2 inner-table) :val-b2)
    (let ((res (downcase-hash-keys-and-values table)))
      (ok (not (gethash :a1 res)))
      (ok (= (gethash :|a1| res) 0))
      (ok (eq (gethash :|a2| res) :|val-a2|))
      (ok (= (gethash :|b1| (gethash :|b| res)) 10))
      (ok (eq (gethash :|b2| (gethash :|b| res)) :|val-b2|)))))
