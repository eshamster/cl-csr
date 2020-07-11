(defpackage cl-csr/ws/mock-client
  (:use :cl)
  (:export :ws-client-mock
           :receive-message-from-server
           :get-latest-messages
           :get-bufferred-messages)
  (:import-from :cl-csr/protocol
                :code-to-name))
(in-package :cl-csr/ws/mock-client)

(defclass ws-client-mock ()
  ((messages-list :initform nil
                  :accessor wcm-messages-list
                  :documentation "Each item of the list is a list of messages from :frame-start to :frame-end")
   (message-buffer :initform nil
                   :accessor wcm-message-buffer
                   :documentation "List of messages from :frame-start until :frame-end. If :frame-end is come, they are moved to message-list")))

(defmethod receive-message-from-server ((wcm ws-client-mock) (message hash-table))
  (symbol-macrolet ((buffer (wcm-message-buffer wcm)))
    (let ((kind (code-to-name (gethash :kind message))))
      (when (and (null buffer)
                 (not (eq kind :frame-start)))
        (error "Frame is not started but got \"~D\"" kind))
      (when (and (not (null buffer))
                 (eq kind :frame-start))
        (error "Got :frame-start but frame has been started"))
      (push message buffer)
      (when (eq kind :frame-end)
        (push (reverse buffer) (wcm-messages-list wcm))
        (setf buffer nil)))))

(defmethod get-latest-messages ((wcm ws-client-mock))
  (car (wcm-messages-list wcm)))

(defmethod get-bufferred-messages ((wcm ws-client-mock))
  (reverse (wcm-message-buffer wcm)))
