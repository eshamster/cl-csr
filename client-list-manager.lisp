(defpackage cl-csr/client-list-manager
  (:use :cl)
  (:export :update-client-list
           :get-new-client-id-list
           :get-deleted-client-id-list
           :get-client-id-list
           :client-alive-p
           :with-sending-to-new-clients
           ;; - for test - ;;
           :with-clean-client-list-manager)
  (:import-from :cl-csr/ws-server
                :*target-client-id-list*
                :get-ws-server
                :pop-new-client-ids
                :pop-deleted-client-ids))
(in-package :cl-csr/client-list-manager)

;; --- interface --- ;;

(defun update-client-list ()
  (let ((ws-server (get-ws-server)))
    (setf *new-client-list* (pop-new-client-ids ws-server)
          *deleted-client-list* (pop-deleted-client-ids ws-server)))
  (setf *client-list* (append (copy-list *client-list*)
                              (copy-list *new-client-list*)))
  (dolist (deleted-id *deleted-client-list*)
    (setf *client-list* (delete deleted-id *client-list*))))

(defun get-new-client-id-list ()
  *new-client-list*)

(defun get-deleted-client-id-list ()
  *deleted-client-list*)

(defun get-client-id-list ()
  *client-list*)

(defun client-alive-p (client-id)
  (find client-id *client-list*))

(defmacro with-sending-to-new-clients (() &body body)
  (let ((new-clients (gensym)))
    `(let ((,new-clients (get-new-client-id-list)))
       (when ,new-clients
         (let ((*target-client-id-list* ,new-clients))
           ,@body)))))

;; - for test - ;;

(defmacro with-clean-client-list-manager (&body body)
  `(let ((*new-client-list* nil)
         (*deleted-client-list* nil)
         (*client-list* nil))
     ,@body))

;; --- internal --- ;;

(defvar *new-client-list* nil)
(defvar *deleted-client-list* nil)
(defvar *client-list* nil)
