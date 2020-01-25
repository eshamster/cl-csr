(defpackage sample-cl-csr/sample-text
  (:use :cl)
  (:export :start-text
           :stop-text)
  (:import-from :cl-csr
                :start-game-loop
                :stop-game-loop
                :set-screen-size
                :load-font
                :draw-text))
(in-package :sample-cl-csr/sample-text)

(defun start-text ()
  (load-font :name :sample-font
             :font-name "Arial")

  (start-game-loop :update-func (lambda () (update)))
  (set-screen-size :width 800 :height 600))

(defun stop-text ()
  (stop-game-loop))

;; --- internal --- ;;

(defun update ()
  (let ((id 0))
    (let ((font-size 24))
      ;; Top
      (draw-text :id (incf id)
                 :text "Top-Left"
                 :font-name :sample-font
                 :x 0 :y 0
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :left
                 :align-vert :top)
      (draw-text :id (incf id)
                 :text "Top-Center"
                 :font-name :sample-font
                 :x 400 :y 0
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :center
                 :align-vert :top)
      (draw-text :id (incf id)
                 :text "Top-Right"
                 :font-name :sample-font
                 :x 800 :y 0
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :right
                 :align-vert :top)
      ;; Center (vertically)
      (draw-text :id (incf id)
                 :text "Center-Left"
                 :font-name :sample-font
                 :x 0 :y 300
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :left
                 :align-vert :center)
      (draw-text :id (incf id)
                 :text "Center-Center"
                 :font-name :sample-font
                 :x 400 :y 300
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :center
                 :align-vert :center)
      (draw-text :id (incf id)
                 :text "Center-Right"
                 :font-name :sample-font
                 :x 800 :y 300
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :right
                 :align-vert :center)
      ;; Bottom
      (draw-text :id (incf id)
                 :text "Bottom-Left"
                 :font-name :sample-font
                 :x 0 :y 600
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :left
                 :align-vert :bottom)
      (draw-text :id (incf id)
                 :text "Bottom-Center"
                 :font-name :sample-font
                 :x 400 :y 600
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :center
                 :align-vert :bottom)
      (draw-text :id (incf id)
                 :text "Bottom-Right"
                 :font-name :sample-font
                 :x 800 :y 600
                 :font-size font-size
                 :depth 0 :color #xffffff
                 :align-horiz :right
                 :align-vert :bottom))))
