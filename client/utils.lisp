(defpackage cl-csr/client/utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :with-command-data)
  (:import-from :alexandria
                :make-keyword))
(in-package :cl-csr/client/utils)

;; --- interface --- ;;

;; - message - ;;

(defmacro.ps+ with-command-data ((&rest target-list) command &body body)
  `(let (,@(mapcar (lambda (target)
                     `(,target (get-data ,command ,(make-keyword target))))
                   target-list))
     ,@body))

;; --- internal --- ;;

;; - message - ;;

(defun.ps+ get-data (command target)
  (gethash target (gethash :data command)))
