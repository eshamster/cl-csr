(defpackage cl-csr/ws/mock-server
  (:use :cl)
  (:export :make-ws-server-mock
           :add-mock-client
           :delete-mock-client
           :receive-message-from-client
           :get-mock-client-ids)
  (:import-from :cl-csr/ws/ws
                :send-from-server
                :*target-client-id-list*
                :pop-new-client-ids
                :pop-deleted-client-ids
                :pop-client-messages
                :make-client-message)
  (:import-from :cl-csr/ws/mock-client
                :ws-client-mock
                :receive-message-from-server))
(in-package :cl-csr/ws/mock-server)

;; --- data structure --- ;;

(defclass ws-server-mock ()
  ((new-client-id-list :initform nil
                       :accessor wss-new-client-id-list)
   (deleted-client-id-list :initform nil
                           :accessor wss-deleted-client-id-list)
   (client-message-list :initform nil
                        :accessor wss-client-message-list)
   (mock-client-table :initform (make-hash-table)
                      :accessor wss-mock-client-table)))

;; --- common interfaces --- ;;

(defmethod pop-new-client-ids ((wss ws-server-mock))
  (let ((ids (wss-new-client-id-list wss)))
    (setf (wss-new-client-id-list wss) nil)
    ids))

(defmethod pop-deleted-client-ids ((wss ws-server-mock))
  (let ((ids (wss-deleted-client-id-list wss)))
    (setf (wss-deleted-client-id-list wss) nil)
    ids))

(defmethod pop-client-messages ((wss ws-server-mock))
  (let ((messages (wss-client-message-list wss)))
    (setf (wss-client-message-list wss) nil)
    (nreverse messages)))

(defmethod send-from-server ((wss ws-server-mock) messages)
  (maphash (lambda (id client)
             (when (or (eq *target-client-id-list* :all)
                       (find id *target-client-id-list*))
               (dolist (message messages)
                 (receive-message-from-server client message))))
           (wss-mock-client-table wss)))

;; --- original interfaces --- ;;

(defun make-ws-server-mock ()
  (make-instance 'ws-server-mock))

(defmethod add-mock-client ((wss ws-server-mock) client-id)
  "Add mock client to mock server and return mock client"
  (check-client-not-exist wss client-id)
  (let ((wsc (make-instance 'ws-client-mock)))
    (setf (gethash client-id (wss-mock-client-table wss))
          wsc)
    (push client-id (wss-new-client-id-list wss))
    wsc))

(defmethod delete-mock-client ((wss ws-server-mock) client-id)
  (check-client-exist wss client-id)
  (remhash client-id (wss-mock-client-table wss))
  (push client-id (wss-deleted-client-id-list wss))
  client-id)

(defmethod receive-message-from-client ((wss ws-server-mock) client-id (message hash-table))
  (check-client-exist wss client-id)
  (push (make-client-message :client-id client-id
                             :message message)
        (wss-client-message-list wss)))

(defmethod get-mock-client-ids ((wss ws-server-mock))
  (let (res)
    (maphash (lambda (id client)
               (declare (ignore client))
               (push id res))
             (wss-mock-client-table wss))
    res))

;; - internal - ;;

(defmethod check-client-exist ((wss ws-server-mock) client-id)
  (unless (gethash client-id (wss-mock-client-table wss))
    (error "Client id ~D has not been added to mock server" client-id)))

(defmethod check-client-not-exist ((wss ws-server-mock) client-id)
  (when (gethash client-id (wss-mock-client-table wss))
    (error "Client id ~D has been added to mock server" client-id)))
