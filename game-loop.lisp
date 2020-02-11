(defpackage cl-csr/game-loop
  (:use :cl)
  (:export :start-game-loop
           :stop-game-loop
           :get-fps
           :set-fps
           :log-console)
  (:import-from :cl-csr/camera
                :update-camera-info)
  (:import-from :cl-csr/client-list-manager
                :update-client-list
                :get-new-client-id-list
                :with-sending-to-new-clients)
  (:import-from :cl-csr/frame-counter
                :incf-frame-count
                :get-frame-count
                :reset-frame-count
                :incf-index-in-frame)
  (:import-from :cl-csr/font
                :update-font)
  (:import-from :cl-csr/graphics
                :update-graphics)
  (:import-from :cl-csr/input
                :update-input)
  (:import-from :cl-csr/protocol
                :send-frame-start
                :send-delete-draw-object
                :send-draw-rect
                :send-draw-circle
                :send-draw-line
                :send-draw-arc
                :send-log-console
                :send-frame-end
                :send-set-fps)
  (:import-from :cl-csr/screen-size
                :update-screen-size)
  (:import-from :cl-csr/texture
                :update-texture)
  (:import-from :alexandria
                :make-keyword
                :with-gensyms)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread))
(in-package :cl-csr/game-loop)

;; --- data --- ;;

(defvar *stop-game-loop-p* nil)
(defvar *loop-thread* nil)
(defvar *FPS* 15)

;; --- internal macro --- ;;

(defmacro with-interval ((target-sec) &body body)
  (with-gensyms (before after g-target-sec)
    `(let ((,before (get-current-sec))
           (,g-target-sec ,target-sec))
       ,@body
       (let ((,after (get-current-sec)))
         (when (< (- ,after ,before) ,g-target-sec)
           (sleep (- ,g-target-sec (- ,after ,before))))))))

;; --- interface --- ;;

(defun start-game-loop (&key (update-func (lambda ())))
  (stop-game-loop)
  (setf *stop-game-loop-p* nil)
  (reset-frame-count)
  (setf *loop-thread*
        (make-thread (lambda ()
                       (loop :do
                            (with-interval ((/ 1 *FPS*))
                              (when *stop-game-loop-p*
                                (return))
                              (update-client-list)
                              (incf-frame-count)
                              (unwind-protect
                                   (progn
                                     (send-frame-start (get-frame-count) (incf-index-in-frame))
                                     (update-texture)
                                     (update-camera-info)
                                     (update-input)
                                     (update-fps)
                                     (update-screen-size)
                                     (funcall update-func)
                                     (update-graphics)
                                     (update-font))
                                (send-frame-end (get-frame-count) (incf-index-in-frame)))))))))

(defun stop-game-loop ()
  (setf *stop-game-loop-p* t)
  (when *loop-thread*
    (join-thread *loop-thread*)
    (setf *loop-thread* nil)))

(defun get-fps ()
  "Get FPS (frame per second) of game loop."
  *FPS*)

(defun set-fps (fps)
  "Set FPS (frame per second) of game loop."
  (setf *FPS* fps)
  (send-set-fps (get-frame-count) (incf-index-in-frame)
                :value fps))

(defun update-fps ()
  (with-sending-to-new-clients ()
    (send-set-fps (get-frame-count) (incf-index-in-frame)
                :value *FPS*)))

;; --- utils --- ;;

(defun get-current-sec ()
  (/ (get-internal-real-time) internal-time-units-per-second))

;; --- sender --- ;;

;; TODO: Consider more proper package

(defun log-console (&key message)
  (send-log-console (get-frame-count) (incf-index-in-frame)
                    :message message))
