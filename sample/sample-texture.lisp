(defpackage sample-cl-csr/sample-texture
  (:use :cl)
  (:export :start-texture
           :stop-texture)
  (:import-from :cl-csr
                :start-game-loop
                :stop-game-loop
                :set-screen-size
                :draw-circle
                :draw-image
                :draw-text
                :load-texture
                :load-image
                :make-image-uv
                :load-font))
(in-package :sample-cl-csr/sample-texture)

(defun start-texture ()
  (load-texture :name :sample
                :path "sample.png")
  (load-image :image-name :sample
              :texture-name :sample)
  (load-texture :name :multiple-image
                :path "multiple_image.png")
  (load-image :image-name :a
              :texture-name :multiple-image
              :uv (make-image-uv :width 0.5))
  (load-image :image-name :b
              :texture-name :multiple-image
              :uv (make-image-uv :x 0.5 :width 0.5))

  (start-game-loop :update-func (lambda () (update)))
  (set-screen-size :width 800 :height 600))

(defun stop-texture ()
  (stop-game-loop))

;; --- internal --- ;;

(defvar *temp-counter* 0)

(defun update ()
  (incf *temp-counter*)
  (let ((id 0))
    (draw-image :id (incf id)
                :image-name :sample
                :x 400 :y 300
                :width 50 :height 50
                :rotate (* -1/10 *temp-counter*)
                :depth 0 :color #xffffff)
    (draw-image :id (incf id)
                :image-name :a
                :x 500 :y 300
                :width 50 :height 50
                :rotate (* -1/10 *temp-counter*)
                :depth 0 :color #xffffff)
    (draw-image :id (incf id)
                :image-name :b
                :x 520 :y 300
                :width 50 :height 50
                :rotate (* 1/10 *temp-counter*)
                :depth 0 :color #xffffff)))
