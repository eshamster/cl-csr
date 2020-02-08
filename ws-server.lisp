(defpackage cl-csr/ws-server
  (:use :cl)
  (:export :make-ws-app
           :*ws-server* ; TODO: remove
           :send-from-server
           :register-message-processor
           :*target-client-id-list*
           :same-target-client-list-p
           :copy-target-client-id-list
           :pop-new-client-ids
           :pop-deleted-client-ids)
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
(in-package :cl-csr/ws-server)

(defclass ws-server ()
  ((new-client-id-list :initform nil
                       :accessor wss-new-client-id-list)
   (lock-for-new :initform (make-lock "Lock for new client id list")
                 :reader wss-lock-for-new)
   (deleted-client-id-list :initform nil
                           :accessor wss-deleted-client-id-list)
   (lock-for-deleted :initform (make-lock "Lock for deleted client id list")
                     :reader wss-lock-for-deleted)))

(defmethod pop-new-client-ids ((wss ws-server))
  (let ((ids (wss-new-client-id-list wss)))
    (when ids
      (with-lock-held ((wss-lock-for-new wss))
        (setf (wss-new-client-id-list wss) nil))
      ids)))

(defmethod pop-deleted-client-ids ((wss ws-server))
  (let ((ids (wss-deleted-client-id-list wss)))
    (when ids
      (with-lock-held ((wss-lock-for-deleted wss))
        (setf (wss-deleted-client-id-list wss) nil))
      ids)))

(defvar *ws-server* (make-instance 'ws-server))

(defvar *target-client-id-list* :all
  "If ':all', a message is sent to all clients.
Otherwise, it is sent to the listed clients.")

(defun same-target-client-list-p (lst1 lst2)
  (or (and (eq lst1 :all)
           (eq lst2 :all))
      (and (listp lst1)
           (listp lst2)
           (equalp (sort lst1 #'<)
               (sort lst2 #'<)))))

(defun copy-target-client-id-list (&optional (lst *target-client-id-list*))
  (if (eq lst :all)
      :all
      (copy-list lst)))

(defvar *latest-client-id* 0)

(defstruct client-info
  target-server
  (id (incf *latest-client-id*)))

(defvar *client-info-list* nil)

(defvar *message-processor-table* (make-hash-table))

(defun register-message-processor (id-as-symbol callback)
  "The callback should take 2 arguments.
The first is an id of client.
The second is a message represented as a hash table."
  (setf (gethash id-as-symbol *message-processor-table*) callback))

(defun make-ws-app ()
  (lambda (env)
    (let* ((server (make-server env))
           (client-info (make-client-info :target-server server))
           (client-id (client-info-id client-info))
           (ws-server *ws-server*))
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
                (maphash (lambda (key callback)
                           (declare (ignore key))
                           (funcall callback client-id parsed-table))
                         *message-processor-table*)))))
      (lambda (responder)
        (declare (ignore responder))
        (format t "~&Connection started: ~D" client-id)
        (start-connection server)))))

(defun send-from-server (message)
  (dolist (client-info (copy-list *client-info-list*))
    (let ((server (client-info-target-server client-info))
          (id (client-info-id client-info))
          (ws-server *ws-server*))
      (case (ready-state server)
        (:open (when (or (eq *target-client-id-list* :all)
                         (find id *target-client-id-list*))
                 (send server message)))
        (:closed (format t "~&Connection closed: ~D" id)
                 (setf *client-info-list* (remove client-info *client-info-list*))
                 (with-lock-held ((wss-lock-for-deleted ws-server))
                   (push id (wss-deleted-client-id-list ws-server))))
        ;; otherwise do nothing
        ))))
