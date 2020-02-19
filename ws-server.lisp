(defpackage cl-csr/ws-server
  (:use :cl)
  (:export :get-ws-server
           :set-ws-server
           :send-from-server
           :*target-client-id-list*
           :same-target-client-list-p
           :copy-target-client-id-list
           :calc-common-target
           :pop-new-client-ids
           :pop-deleted-client-ids
           :pop-client-messages

           :client-message
           :client-message-client-id
           :client-message-message
           :make-client-message
           ;; - for test - ;;
           :with-ws-server))
(in-package :cl-csr/ws-server)

(defstruct client-message
  client-id
  message ; represented as a hash table
  )

(defgeneric pop-new-client-ids (ws-server))
(defgeneric pop-deleted-client-ids (ws-server))
(defgeneric pop-client-messages (ws-server)
  (:documentation "each message is an instance of client-message structure"))
(defgeneric send-from-server (ws-server messages)
  (:documentation "messages is represented as a list of nested hash tables"))

(defvar *ws-server* nil)
(defun get-ws-server () *ws-server*)
(defun set-ws-server (ws-server) (setf *ws-server* ws-server))

(defmacro with-ws-server ((ws-server) &body body)
  `(let ((*ws-server* ,ws-server))
     ,@body))

;; TODO: If possible, remove the variable and
;; add an argument "target-client-id-list" to send-from-server method.
(defvar *target-client-id-list* :all
  "If ':all', a message is sent to all clients.
Otherwise, it is sent to the listed clients.")

;; --- utils --- ;;

(defun same-target-client-list-p (lst1 lst2)
  (or (and (eq lst1 :all)
           (eq lst2 :all))
      (and (listp lst1)
           (listp lst2)
           (equalp (sort (copy-list lst1) #'<)
                   (sort (copy-list lst2) #'<)))))

(defun copy-target-client-id-list (&optional (lst *target-client-id-list*))
  (if (eq lst :all)
      :all
      (copy-list lst)))

(defun calc-common-target (id-list1 id-list2)
  (cond ((eq id-list1 :all) id-list2)
        ((eq id-list2 :all) id-list1)
        (t (remove-if (lambda (id)
                        (not (find id id-list1)))
                      id-list2))))
