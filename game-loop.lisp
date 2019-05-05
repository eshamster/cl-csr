(defpackage proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-game-loop
           :stop-game-loop
           :draw-rect
           :draw-circle
           :log-console)
  (:import-from :proto-cl-client-side-rendering/input
                :update-input)
  (:import-from :proto-cl-client-side-rendering/protocol
                :send-frame-start
                :send-delete-draw-object
                :send-draw-rect
                :send-draw-circle
                :send-log-console
                :send-frame-end)
  (:import-from :proto-cl-client-side-rendering/ws-server
                :send-from-server
                :*target-client-id-list*
                :register-callback-on-connecting)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread))
(in-package :proto-cl-client-side-rendering/game-loop)

(defvar *current-frame* 0)
(defvar *index-in-frame* 0)
(defvar *stop-game-loop-p* nil)
(defvar *loop-thread* nil)

(defun start-game-loop (&key (update-func (lambda ())))
  (stop-game-loop)
  (setf *stop-game-loop-p* nil
        *current-frame* 0)
  (setf *loop-thread*
        (make-thread (lambda ()
                       (loop :do
                            (when *stop-game-loop-p*
                              (return))
                            (setf *index-in-frame* 0)
                            (incf *current-frame*)
                            (update-input)
                            (unwind-protect
                                 (progn
                                   (send-frame-start *current-frame* (incf *index-in-frame*))
                                   (funcall update-func)
                                   (process-all-draw-messages))
                              (send-frame-end *current-frame* (incf *index-in-frame*)))
                            (update-new-client-list)
                            (sleep 0.5))))))

(defun stop-game-loop ()
  (setf *stop-game-loop-p* t)
  (when *loop-thread*
    (join-thread *loop-thread*)
    (setf *loop-thread* nil)))

;; --- new client list --- ;;

(defvar *new-client-list-buffer* nil)
(defvar *new-client-list* nil)

(defun update-new-client-list ()
  ;; TODO: lock list
  (setf *new-client-list* *new-client-list-buffer*
        *new-client-list-buffer* nil))

(register-callback-on-connecting
 'add-new-client-list-to-buffer
 (lambda (client-id) (push client-id *new-client-list-buffer*)))

;; --- draw information manager --- ;;

(defstruct draw-info sender client-id-list param-table)

(defvar *draw-info-table* (make-hash-table)
  "Key: id, Value: draw-info")
(defvar *prev-draw-info-table* (make-hash-table))

(defun same-param-table-p (table1 table2)
  ;; Note: Assume that two tables have same keys and
  (maphash (lambda (key value)
             (unless (equalp value (gethash key table2))
               (return-from same-param-table-p nil)))
           table1)
  t)

(defun same-draw-info-p (info1 info2)
  (and (eq (draw-info-sender info1)
           (draw-info-sender info2))
       ;; FIXME: Assume client-id-list never be changed
       ;;        (but adding new client can be detected)
       (same-param-table-p (draw-info-param-table info1)
                           (draw-info-param-table info2))))

(defun calc-common-target (default-id-list new-client-id-list)
  (if (listp default-id-list)
      (remove-if (lambda (id)
                   (not (find id default-id-list)))
                 new-client-id-list)
      new-client-id-list ; The default list is ":all"
      ))

(defun calc-target-client-id-list (object-id)
  (let ((info (gethash object-id *draw-info-table*))
        (prev-info (gethash object-id *prev-draw-info-table*)))
    (let ((list-in-info (draw-info-client-id-list info)))
      (cond ((null prev-info) list-in-info)
            ((same-draw-info-p info prev-info)
             (if *new-client-list*
                 (calc-common-target list-in-info *new-client-list*)
                 nil))
            (t list-in-info)))))

(defun process-all-draw-messages ()
  (maphash (lambda (id draw-info)
             (let ((*target-client-id-list* (calc-target-client-id-list id)))
               (call-sender-by-param-table
                (draw-info-sender draw-info)
                (draw-info-param-table draw-info))))
           *draw-info-table*)
  ;; XXX: Should ensure delete messages are sent to all clients
  (maphash (lambda (id draw-info)
             (declare (ignore draw-info))
             (multiple-value-bind (value found)
                 (gethash id *draw-info-table*)
               (declare (ignore value))
               (unless found
                 (send-delete-draw-object
                  *current-frame* (incf *index-in-frame*)
                  :id id))))
           *prev-draw-info-table*)
  (switch-draw-info-table))

(defun switch-draw-info-table ()
  (setf *prev-draw-info-table* *draw-info-table*
        *draw-info-table* (make-hash-table)))

(defmacro init-table-by-params (&rest params)
  (let ((g-param-table (gensym "PARAM-TABLE")))
    `(let ((,g-param-table (make-hash-table)))
       (progn ,@(mapcar (lambda (param)
                          (let ((key (make-keyword param)))
                            `(setf (gethash ,key ,g-param-table) ,param)))
                        params)
              ,g-param-table))))

(defun call-sender-by-param-table (sender param-table)
  (let (param-list)
    (maphash (lambda (key value)
               (push value param-list)
               (push key param-list))
             param-table)
    (apply sender (list* *current-frame* (incf *index-in-frame*)
                         param-list))))

;; --- sender --- ;;

;; TODO: Consider more proper package

(defun draw-circle (&key id x y depth color fill-p r)
  (setf (gethash id *draw-info-table*)
        (make-draw-info :sender #'send-draw-circle
                        :param-table (init-table-by-params
                                      id x y depth color fill-p r)
                        :client-id-list *target-client-id-list*)))

(defun draw-rect (&key id x y depth color fill-p width height rotate)
  (setf (gethash id *draw-info-table*)
        (make-draw-info :sender #'send-draw-rect
                        :param-table (init-table-by-params
                                      id x y depth color fill-p width height rotate)
                        :client-id-list *target-client-id-list*)))

(defun log-console (&key message)
  (send-log-console *current-frame* (incf *index-in-frame*)
                    :message message))
