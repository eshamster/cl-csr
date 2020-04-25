(defpackage cl-csr/font
  (:use :cl)
  (:export :load-font
           :update-font
           :get-font-id
           ;; - for test - ;;
           :with-clean-font-state)
  (:import-from :cl-csr/client-list-manager
                :with-sending-to-new-clients)
  (:import-from :cl-csr/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :cl-csr/protocol
                :send-load-font)
  (:import-from :cl-csr/texture
                :get-image-relative-path
                :get-image-root-path
                :get-texture-id)
  (:import-from :alexandria
                :maphash-values))
(in-package :cl-csr/font)

;; --- data --- ;;

(defvar *font-id* 0)

(defvar *font-table* (make-hash-table)
  "Key: A name represented as a keyword; Value: font-info")

(defstruct font-info id font-name)

(defmacro with-clean-font-state (&body body) ; for test
  `(let ((*font-id* 0)
         (*font-table* (make-hash-table)))
     ,@body))

;; --- interface --- ;;

(defun update-font ()
  (with-sending-to-new-clients ()
    (maphash-values (lambda (font-info)
                      (process-load-font font-info))
                    *font-table*)))

(defun load-font (&key name font-name)
  "Load a font.
The name is represented as a keyword."
  (check-type name keyword)
  (let ((info (make-font-info :id (make-font-id name)
                              :font-name font-name)))
    (setf (gethash name *font-table*) info)
    (process-load-font info)))

;; TODO: Functions to remove fonts

(defun get-font-id (name)
  (multiple-value-bind (info foundp)
      (gethash name *font-table*)
    (when foundp
      (font-info-id info))))

;; --- internal --- ;;

(defun make-font-id (name)
  (let ((info (gethash name *font-table*)))
    (if info
        (font-info-id info)
        (incf *font-id*))))

;; - sender - ;;

(defun process-load-font (font-info)
  (send-load-font (get-frame-count) (incf-index-in-frame)
                  :font-id (font-info-id font-info)
                  :font-name (font-info-font-name font-info)))
