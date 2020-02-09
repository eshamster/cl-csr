(defpackage cl-csr/ws-server-impl
  (:use :cl)
  (:export :make-ws-app)
  (:import-from :cl-csr/ws-server
                :get-ws-server
                :set-ws-server
                :send-from-server
                :*target-client-id-list*
                :pop-new-client-ids
                :pop-deleted-client-ids
                :pop-client-messages
                :make-client-message)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held)
  (:import-from :jonathan
                :parse)
  (:import-from :websocket-driver
                :make-server
                :on
                :send
                :start-connection
                :ready-state))
(in-package :cl-csr/ws-server-impl)

(defclass ws-server ()
  ((new-client-id-list :initform nil
                       :accessor wss-new-client-id-list)
   (lock-for-new :initform (make-lock "Lock for new client id list")
                 :reader wss-lock-for-new)
   (deleted-client-id-list :initform nil
                           :accessor wss-deleted-client-id-list)
   (lock-for-deleted :initform (make-lock "Lock for deleted client id list")
                     :reader wss-lock-for-deleted)
   (client-message-list :initform nil
                        :accessor wss-client-message-list)
   (lock-for-message :initform (make-lock "Lock for client message")
                     :reader wss-lock-for-message)))

(defmethod pop-new-client-ids ((wss ws-server))
  (symbol-macrolet ((ids (wss-new-client-id-list wss)))
    (when ids
      (with-lock-held ((wss-lock-for-new wss))
        (let ((res ids))
          (setf ids nil)
          res)))))

(defmethod pop-deleted-client-ids ((wss ws-server))
  (symbol-macrolet ((ids (wss-deleted-client-id-list wss)))
    (when ids
      (with-lock-held ((wss-lock-for-deleted wss))
        (let ((res ids))
          (setf ids nil)
          res)))))

(defmethod pop-client-messages ((wss ws-server))
  (symbol-macrolet ((messages (wss-client-message-list wss)))
    (when messages
      (with-lock-held ((wss-lock-for-deleted wss))
        (let ((res messages))
          (setf messages nil)
          res)))))

(defvar *latest-client-id* 0)

(defstruct client-info
  target-server
  (id (incf *latest-client-id*)))

(defvar *client-info-list* nil)

(defun make-ws-app ()
  (set-ws-server (make-instance 'ws-server))
  (lambda (env)
    (let* ((server (make-server env))
           (client-info (make-client-info :target-server server))
           (client-id (client-info-id client-info))
           (ws-server (get-ws-server)))
      (check-type ws-server ws-server)
      (push client-info *client-info-list*)
      (with-lock-held ((wss-lock-for-new ws-server))
        (push client-id (wss-new-client-id-list ws-server)))
      (on :message server
          (lambda (json-string)
            (labels ((parse-value (value)
                       (typecase value
                         (hash-table (parse-table value))
                         (list (mapcar (lambda (elem)
                                         (parse-value elem))
                                       value))
                         (t value)))
                     (parse-table (base-table)
                       (let ((result (make-hash-table)))
                         (maphash (lambda (key value)
                                    (setf (gethash (intern (string-upcase key) "KEYWORD")
                                                   result)
                                          (parse-value value)))
                                  base-table)
                         result)))
              (let* ((temp-table (parse json-string :as :hash-table))
                     (parsed-table (parse-table temp-table)))
                (push (make-client-message :client-id client-id
                                           :message parsed-table)
                      (wss-client-message-list ws-server))))))
      (lambda (responder)
        (declare (ignore responder))
        (format t "~&Connection started: ~D" client-id)
        (start-connection server)))))

(defmethod send-from-server ((wss ws-server) message)
  (dolist (client-info (copy-list *client-info-list*))
    (let ((server (client-info-target-server client-info))
          (id (client-info-id client-info)))
      (case (ready-state server)
        (:open (when (or (eq *target-client-id-list* :all)
                         (find id *target-client-id-list*))
                 (send server message)))
        (:closed (format t "~&Connection closed: ~D" id)
                 (setf *client-info-list* (remove client-info *client-info-list*))
                 (with-lock-held ((wss-lock-for-deleted wss))
                   (push id (wss-deleted-client-id-list wss))))
        ;; otherwise do nothing
        ))))
