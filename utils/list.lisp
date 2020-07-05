(defpackage cl-csr/utils/list
  (:use :cl)
  (:export :make-plist-by-param
           :plist-to-nested-hash-table)
  (:import-from :alexandria
                :make-keyword
                :plist-hash-table))
(in-package :cl-csr/utils/list)

;; --- interface --- ;;

(defmacro make-plist-by-param (&rest params)
  "Ex. (make-plist-by-param a b c) -> (list :a a :b b :c c)"
  `(list ,@(mapcan (lambda (param)
                     (list (make-keyword param)
                           param))
                   params)))

(defun plist-to-nested-hash-table (plist)
  (labels ((rec (lst)
             (let ((table (plist-hash-table lst)))
               (maphash (lambda (k v)
                          (when (listp v)
                            (setf (gethash k table) (rec v))))
                        table)
               table)))
    (rec plist)))
