(defpackage cl-csr/screen-size
  (:use :cl)
  (:export :update-screen-size
           :get-screen-size
           :set-screen-size
           ;; - for test - ;;
           :with-clean-screen-size)
  (:import-from :cl-csr/client-list-manager
                :get-new-client-id-list)
  (:import-from :cl-csr/frame-counter
                :get-frame-count
                :incf-index-in-frame)
  (:import-from :cl-csr/protocol
                :send-set-screen-size)
  (:import-from :cl-csr/client-list-manager
                :with-sending-to-new-clients))
(in-package :cl-csr/screen-size)

;; Note: Assume that screen size is common for all clients.

;; --- data --- ;;

(defvar *screen-width* 800)
(defvar *screen-height* 600)

(defmacro with-clean-screen-size ((width height) &body body)
  `(let ((*screen-width* ,width)
         (*screen-height* ,height))
     ,@body))

;; --- interface --- ;;

;; TODO: Prevent sending an old message when other messages are sent.
(defun update-screen-size ()
  (when (get-new-client-id-list)
    (with-sending-to-new-clients ()
      (multiple-value-bind (width height) (get-screen-size)
        (set-screen-size :width width :height height)))))

(defun get-screen-size ()
  "Return screen size as multiple values of (width height)."
  (values *screen-width* *screen-height*))

(defun set-screen-size (&key width height)
  ;; FIXME: Validate arguments
  (setf *screen-width* width
        *screen-height* height)
  (send-set-screen-size (get-frame-count) (incf-index-in-frame)
                        :width width :height height))
