(defpackage cl-csr/client/input
  (:use :cl)
  (:export :init-input)
  (:import-from :cl-csr/protocol
                :name-to-code)
  (:import-from :cl-csr/client/renderer
                :get-screen-offset
                :get-screen-scale)
  (:import-from :cl-csr/client/socket
                :send-json-to-server)
  (:import-from :ps-experiment
                :defvar.ps
                :defun.ps
                :defun.ps+
                :enable-ps-experiment-syntax))
(in-package :cl-csr/client/input)

(enable-ps-experiment-syntax)

(defun.ps init-input (renderer)
  (window.add-event-listener "keydown" on-keydown)
  (window.add-event-listener "keyup" on-keyup)
  (window.add-event-listener "mouseup" (lambda (e) (on-mouseup e renderer)))
  (window.add-event-listener "mousedown" (lambda (e) (on-mousedown e renderer)))
  (window.add-event-listener "mousemove" (lambda (e) (on-mousemove e renderer)))
  (window.add-event-listener "wheel" on-wheel)
  (window.add-event-listener "touchstart" (lambda (e) (on-touchstart e renderer)))
  (window.add-event-listener "touchend" (lambda (e) (on-touchsend e renderer)))
  (window.add-event-listener "touchmove" (lambda (e) (on-touchmove e renderer))))

;; --- internal --- ;;

;; - keyboard - ;;

(defvar.ps *key-down-table* (make-hash-table))

(defun.ps adjust-key-name (key-name)
  (cond ((key-name.starts-with "Arrow")
         (key-name.substr (length "Arrow")))
        ((string= key-name " ") :space)
        (t key-name)))

(defun.ps on-keydown (e)
  (let ((key e.key))
    (unless (gethash key *key-down-table*)
      (send-json-to-server (ps:create :kind (name-to-code :key-down)
                                      :data (ps:create
                                             :key (adjust-key-name key))))
      (setf (gethash key *key-down-table*) t))))

(defun.ps on-keyup (e)
  (let ((key e.key))
    (send-json-to-server (ps:create :kind (name-to-code :key-up)
                                    :data (ps:create
                                           :key (adjust-key-name key))))
    (setf (gethash key *key-down-table*) nil)))

;; - mouse - ;;

(defun.ps+ calc-adjusted-input-point (renderer x y)
  (let ((scale (get-screen-scale renderer)))
    (multiple-value-bind (offset-x offset-y) (get-screen-offset renderer)
      (values (floor (/ (- x offset-x) scale))
              (floor (/ (- y offset-y) scale))))))

(defun.ps+ mouse-button-to-string (button)
  (case button
    (0 :left)
    (1 :center)
    (2 :rihgt)
    (t button)))

(defun.ps send-mouse-message (kind e renderer)
  (multiple-value-bind (x y)
      (calc-adjusted-input-point renderer e.client-x e.client-y)
    (send-json-to-server (ps:create :kind (name-to-code kind)
                                    :data (ps:create
                                           :button (mouse-button-to-string e.button)
                                           :x x
                                           :y y)))))

(defun.ps+ on-mousedown (e renderer)
  (send-mouse-message :mouse-down e renderer))

(defun.ps+ on-mouseup (e renderer)
  (send-mouse-message :mouse-up e renderer))

(defun.ps+ on-mousemove (e renderer)
  (send-mouse-message :mouse-move e renderer))

;; wheel

(defun.ps on-wheel (e)
  (send-json-to-server (ps:create :kind (name-to-code :mouse-wheel)
                                  :data (ps:create
                                         :delta-y e.delta-y))))

;; - touch - ;;

(defun.ps send-touch-message (kind e renderer)
  (let ((touches
         (loop :for i :from 0 :below e.changed-touches.length
            :collect
              (let ((touch (nth 0 e.changed-touches)))
                (multiple-value-bind (x y)
                    (calc-adjusted-input-point renderer
                                               (ps:@ touch client-x)
                                               (ps:@ touch client-y))
                  (ps:create :id touch.identifier :x x :y y))))))
    (send-json-to-server (ps:create :kind (name-to-code kind)
                                    :data touches))))

(defun.ps+ on-touchstart (e renderer)
  (send-touch-message :touch-start e renderer))

(defun.ps+ on-touchend (e renderer)
  (send-touch-message :touch-end e renderer))

(defun.ps+ on-touchmove (e renderer)
  (send-touch-message :touch-move e renderer))
