(defpackage cl-csr/utils/hash-table
  (:use :cl)
  (:export :downcase-hash-keys))
(in-package :cl-csr/utils/hash-table)

;; --- interface --- ;;

(defun downcase-hash-keys (nested-hash)
  (labels ((down (sym)
             (intern (string-downcase (symbol-name sym))
                     (symbol-package sym)))
           (rec (hash)
             (let ((res (make-hash-table)))
               (maphash (lambda (k v)
                          (setf (gethash (down k) res)
                                (if (hash-table-p v)
                                    (rec v)
                                    v)))
                        hash)
               res)))
    (rec nested-hash)))
