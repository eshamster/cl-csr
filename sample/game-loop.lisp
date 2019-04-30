(defpackage sample-proto-cl-client-side-rendering/game-loop
  (:use :cl)
  (:export :start-sample-game-loop
           :stop-sample-game-loop)
  (:import-from :proto-cl-client-side-rendering
                :start-game-loop
                :stop-game-loop
                :draw-rect
                :draw-circle))
(in-package :sample-proto-cl-client-side-rendering/game-loop)

(defun start-sample-game-loop ()
  (start-game-loop :update-func #'update))

(defun stop-sample-game-loop ()
  (stop-game-loop))

;; --- internal --- ;;

(defvar *temp-counter* 0)

(defun update ()
  (incf *temp-counter*)
  (let ((id 0))
    (draw-circle :id (incf id)
                 :x 200 :y (+ 300 (* 100 (sin (/ *temp-counter* 2))))
                 :depth 0
                 :r 40 :color #xff0000)
    (draw-circle :id (incf id)
                 :x 300 :y (+ 300 (* 100 (sin (/ *temp-counter* 3))))
                 :depth 0 :fill-p t
                 :r 40 :color #x00ffff)
    (draw-rect :id (incf id)
               :x 400 :y 300
               :depth 0 :fill-p nil
               :width 20 :height 40
               :rotate (* 1/5 *temp-counter*)
               :color #x00ff00)
    (draw-rect :id (incf id)
               :x 500 :y 300
               :depth 0 :fill-p t
               :width 20 :height 40
               :rotate (* -1/5 *temp-counter*)
               :color #xff00ff)))