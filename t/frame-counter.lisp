(defpackage cl-csr/t/frame-counter
  (:use :cl
        :rove
        :cl-csr/frame-counter))
(in-package :cl-csr/t/frame-counter)

(defmacro with-reset-counter (&body body)
  `(unwind-protect
        (progn (reset-frame-count)
               ,@body)
     (reset-frame-count)))

(deftest test-frame-counter
  (with-reset-counter
    (testing "first frame"
      (ok (= (get-frame-count) 0))
      (ok (= (get-frame-count) 0))
      (ok (= (incf-index-in-frame) 0))
      (ok (= (incf-index-in-frame) 1)))
    (testing "next frame"
      (ok (not (signals (incf-frame-count))))
      (ok (= (get-frame-count) 1))
      (ok (= (incf-index-in-frame) 0))
      (ok (= (incf-index-in-frame) 1)))
    (testing "reset frame"
      (ok (not (signals (reset-frame-count))))
      (ok (= (get-frame-count) 0))
      (ok (= (incf-index-in-frame) 0))
      (ok (= (incf-index-in-frame) 1)))))
