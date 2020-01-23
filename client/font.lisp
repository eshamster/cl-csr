(defpackage cl-csr/client/font
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :update-font
           :interpret-font-message
           :make-text-mesh)
  (:import-from :cl-csr/protocol
                :code-to-name)
  (:import-from :cl-csr/client/utils
                :with-command-data)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :cl-ps-ecs
                :register-func-with-pred))
(in-package :cl-csr/client/font)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defstruct.ps+ font-info id font-name)

(defvar.ps+ *font-info-buffer* (list))

(defvar.ps+ *font-info-table* (make-hash-table)
  "Key: font id, Value: font-info")

;; --- interface --- ;;

;; - for general processor - ;;

(defun.ps+ update-font ()
  )

;; - for messenger - ;;

(defun.ps+ interpret-font-message (kind-code command)
  (ecase (code-to-name kind-code)
    (:load-font
     (with-command-data (font-id font-name)
         command
       (load-font :id font-id :font-name font-name)))))

;; - for drawer - ;;

(defun.ps make-text-mesh (&key text font-id font-size color)
  (let ((opts (make-hash-table))
        (font-info (gethash font-id *font-info-table*)))
    (unless font-info
      (error "font-info is not id ~D has not been loaded" font-id))
    (dolist (pair (list (list "fontFamily" (font-info-font-name font-info))
                        (list "fontSize" font-size)
                        (list "fill" color)))
      (setf (gethash (car pair) opts)
            (cadr pair)))
    (let ((style (new (#j.PIXI.TextStyle# opts))))
      (new (#j.PIXI.Text# text style)))))

;; --- internal --- ;;

(defun.ps+ load-font (&key id font-name)
  (setf (gethash id *font-info-table*)
        (make-font-info :id id :font-name font-name)))

;; TODO: unload-font

