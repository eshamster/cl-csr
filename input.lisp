(defpackage cl-csr/input
  (:use :cl)
  (:export :update-input

           :key-down-now-p
           :key-down-p
           :key-up-now-p
           :key-up-p

           :mouse-down-now-p
           :mouse-down-p
           :mouse-up-now-p
           :mouse-up-p
           :get-mouse-pos
           :get-wheel-delta-y

           :touch-summary-down-now-p
           :touch-summary-down-p
           :touch-summary-up-now-p
           :touch-summary-up-p
           :get-touch-summary-pos
           ;; - for test - ;;
           :with-clean-input-state)
  (:import-from :cl-csr/utils/input
                :make-key-input-info
                :update-key-input-info
                :input-down-now-p
                :input-down-p
                :input-up-now-p
                :input-up-p
                :set-raw-key-state
                :get-input-state)
  (:import-from :cl-csr/client-list-manager
                :get-deleted-client-id-list)
  (:import-from :cl-csr/protocol
                :name-to-code
                :code-to-name)
  (:import-from :cl-csr/ws-server
                :get-ws-server
                :pop-client-messages
                :client-message-client-id
                :client-message-message)
  (:import-from :alexandria
                :make-keyword
                :ensure-gethash
                :hash-table-values))
(in-package :cl-csr/input)

(defun process-input-message (client-id message-table)
  (let ((kind (code-to-name (gethash :kind message-table)))
        (data (gethash :data message-table)))
    (case kind
      ((:key-down :key-up)
       (set-client-raw-key-state
        client-id (make-keyword (string-upcase (gethash :key data)))
        (eq kind :key-down)))
      ((:mouse-down :mouse-up)
       (let ((raw-button (gethash :button data)))
         ;; If raw-button is not string, the button is not implemented yet.
         (when (stringp raw-button)
           (let* ((button (make-keyword (string-upcase raw-button)))
                  (key-name (mouse-button-to-key-name button)))
             (when key-name
               (set-client-raw-key-state
                client-id key-name (eq kind :mouse-down))))))
       (update-mouse-pos-buffer
        client-id
        (gethash :x data)
        (gethash :y data)))
      (:mouse-move
       (update-mouse-pos-buffer
        client-id
        (gethash :x data)
        (gethash :y data)))
      (:mouse-wheel
       (update-mouse-wheel-buffer
        client-id
        (gethash :delta-y data)))
      ((:touch-start :touch-end :touch-move)
       (update-touch-info-by-event client-id kind data))
      (t (print-nested-hash-table message-table)))))

(defun update-input ()
  (dolist (message (pop-client-messages (get-ws-server)))
    (process-input-message (client-message-client-id message)
                           (client-message-message message)))
  ;; - delete - ;;
  (dolist (client-id (get-deleted-client-id-list))
    (print 'test)
    (delete-client-input-info client-id))
  ;; - update - ;;
  ;; keyboard
  (maphash (lambda (id info)
             (declare (ignore id))
             (update-key-input-info info))
           *client-input-info-table*)
  ;; mouse
  (update-mouse-info)
  ;; touch
  (update-touch-info))

(defmacro with-clean-input-state (&body body)
  ;; TODO: integrate these global variables to one variable
  `(let ((*client-input-info-table* (make-hash-table))
         (*mouse-info-table* (make-hash-table))
         (*latest-touch-id* 0)
         (*touch-info-table* (make-hash-table)))
     ,@body))

;; - keyboard - ;;

(defun key-down-now-p (client-id key)
  (input-down-now-p (get-client-info client-id) key))
(defun key-down-p (client-id key)
  (input-down-p (get-client-info client-id) key))
(defun key-up-now-p (client-id key)
  (input-up-now-p (get-client-info client-id) key))
(defun key-up-p (client-id key)
  (input-up-p (get-client-info client-id) key))

;; - mouse- ;;

(defun mouse-down-now-p (client-id button)
  (key-down-now-p client-id (mouse-button-to-key-name button)))
(defun mouse-down-p (client-id button)
  (key-down-p client-id (mouse-button-to-key-name button)))
(defun mouse-up-now-p (client-id button)
  (key-up-now-p client-id (mouse-button-to-key-name button)))
(defun mouse-up-p (client-id button)
  (key-up-p client-id (mouse-button-to-key-name button)))

(defun get-mouse-pos (client-id)
  "Returns (value x y)"
  (let ((info (get-mouse-info client-id)))
    (values (mouse-info-x info) (mouse-info-y info))))

(defun get-wheel-delta-y (client-id)
  (mouse-info-delta-y (get-mouse-info client-id)))

;; - touch - ;;

;; TODO: Define functions to get information of each touch.

(defun touch-summary-down-now-p (client-id)
  "Do one or more touches exist from this frame?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :down-now))))
(defun touch-summary-down-p (client-id)
  "Do one or more touches exist?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :down-now) (eq state :down))))
(defun touch-summary-up-now-p (client-id)
  "Do no touches exist from this frame?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :up-now))))
(defun touch-summary-up-p (client-id)
  "Do no touches exist?"
  (let ((state (get-touch-state-summary client-id)))
    (or (eq state :up-now) (eq state :up))))

(defun get-touch-summary-pos (client-id)
  "Return average point of all touches as (values x y).
If no touches exist, return nil"
  (let ((info-list (gethash client-id *touch-info-table*)))
    (unless info-list
      (return-from get-touch-summary-pos nil))
    (let ((sum-x 0)
          (sum-y 0)
          (num (length info-list)))
      (dolist (info info-list)
        (incf sum-x (touch-info-x info))
        (incf sum-y (touch-info-y info)))
      (values (/ sum-x num) (/ sum-y num)))))

;; --- internal --- ;;

(defun delete-client-input-info (client-id)
  (delete-keyboard-info client-id)
  (delete-mouse-info client-id)
  (delete-touch-info client-id))

;; - keyboard (and mouse click) - ;;

(defparameter *client-input-info-table* (make-hash-table))

(defun get-client-info (client-id)
  (let ((info (gethash client-id *client-input-info-table*)))
    (if info
        info
        (setf (gethash client-id *client-input-info-table*)
              (make-key-input-info)))))

(defun delete-keyboard-info (client-id)
  (remhash client-id *client-input-info-table*))

(defun set-client-raw-key-state (client-id key down-p)
  (set-raw-key-state (get-client-info client-id) key down-p))

(defun get-key-count (client-id key)
  (gethash key
           (client-input-info-key-count-table
            (get-client-info client-id))
           -2)) ; -2 is ":up" state

;; - mouse - ;;

(defstruct mouse-info
  (x 0) (x-buffer 0)
  (y 0) (y-buffer 0)
  (delta-y 0) (delta-y-buffer 0))

(defvar *mouse-info-table* (make-hash-table))

(defun mouse-button-to-key-name (button)
  (case button
    (:left :mouse-left)
    (:right :mouse-right)
    (:center :mouse-center)))

(defun get-mouse-info (client-id)
  (ensure-gethash client-id *mouse-info-table*
                  (make-mouse-info)))

(defun update-mouse-pos-buffer (client-id x y)
  (let ((info (get-mouse-info client-id)))
    (setf (mouse-info-x-buffer info) x
          (mouse-info-y-buffer info) y)))

(defun update-mouse-wheel-buffer (client-id delta-y)
  (let ((info (get-mouse-info client-id)))
    (incf (mouse-info-delta-y-buffer info) delta-y)))

(defun update-mouse-info ()
  (dolist (info (hash-table-values *mouse-info-table*))
    (with-slots (x y x-buffer y-buffer
                   delta-y delta-y-buffer)
        info
      (setf x x-buffer
            y y-buffer
            delta-y delta-y-buffer
            delta-y-buffer 0))))

(defun delete-mouse-info (client-id)
  (remhash client-id *mouse-info-table*))

;; - touch - ;;

(defvar *latest-touch-id* 0)

(defstruct touch-info
  (id (incf *latest-touch-id*))
  raw-id
  x y x-buffer y-buffer
  (key-input-info (make-key-input-info)))

(defvar *touch-info-table* (make-hash-table)
  "Key: client id, Value list of touch-info")

(defun update-touch-info-by-event (client-id kind data-table-list)
  (let ((info-list (gethash client-id *touch-info-table* (list))))
    (dolist (data-table data-table-list)
      (let ((x (gethash :x data-table))
            (y (gethash :y data-table))
            (raw-id (gethash :id data-table)))
        (flet ((get-info ()
                 (find raw-id info-list :key #'touch-info-raw-id)))
          (ecase kind
            (:touch-start
             ;; FIXME: Should prevent from getting new touch info until next frame.
             (let ((info (make-touch-info :raw-id raw-id
                                          :x x :x-buffer x
                                          :y y :y-buffer y)))
               (set-raw-key-state (touch-info-key-input-info info) :touch t)
               (push info info-list)))
            (:touch-end
             (let ((info (get-info)))
               (setf (touch-info-raw-id info) nil
                     (touch-info-x-buffer info) x
                     (touch-info-y-buffer info) y)
               (set-raw-key-state (touch-info-key-input-info info) :touch nil)))
            (:touch-move
             (let ((info (get-info)))
               (setf (touch-info-x-buffer info) x
                     (touch-info-y-buffer info) y)))))))
    (setf (gethash client-id *touch-info-table*) info-list)))

(defun update-touch-info ()
  (maphash (lambda (client-id info-list)
             (dolist (info info-list)
               (with-slots (x y x-buffer y-buffer) info
                 (setf x x-buffer
                       y y-buffer))
               (update-key-input-info (touch-info-key-input-info info)))
             (setf (gethash client-id *touch-info-table*)
                   (remove-if (lambda (info)
                                (let ((key-info (touch-info-key-input-info info)))
                                  (and (not (input-up-now-p key-info :touch))
                                       (input-up-p key-info :touch))))
                              info-list)))
           *touch-info-table*))

(defun delete-touch-info (client-id)
  (remhash client-id *touch-info-table*))

(defun get-touch-state-summary (client-id)
  (let* ((info-list (gethash client-id *touch-info-table* (list)))
         (state-list (mapcar (lambda (info)
                               (get-input-state
                                (touch-info-key-input-info info) :touch))
                             info-list)))
    (case (length state-list)
      (0 :up)
      (1 (car state-list))
      (t (flet ((some-state (target-state)
                  (some (lambda (state) (eq state target-state))
                        state-list)))
           (cond ((some-state :down) :down)
                 ((some-state :down-now) :down-now)
                 ((some-state :up-now) :up-now)
                 (t :up)))))))

;; - utils - ;;

;; For debug when implementing a new event.
(defun print-nested-hash-table (table)
  (format t "~&-----------~%~A" (code-to-name (gethash :kind table)))
  (print (jonathan:parse (jonathan:to-json table))))
