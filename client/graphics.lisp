(defpackage cl-csr/client/graphics
  (:use :cl)
  (:export :make-line
           :make-lines
           :make-solid-rect
           :make-wired-rect
           :make-arc
           :make-solid-circle
           :make-wired-circle

           :model
           :make-model
           :model-graphics
           :model-offset-x
           :model-offset-y
           :set-model-pos)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :parenscript
                :create
                :new)
  (:import-from :ps-experiment
                :enable-ps-experiment-syntax
                :defun.ps
                :defun.ps+
                :defmacro.ps+
                :defstruct.ps+))
(in-package :cl-csr/client/graphics)

;; Note: This is ported from cl-web-2d-game

(enable-ps-experiment-syntax)

(defstruct.ps+ model
    graphics ; PIXI.Graphics
  (offset-x 0)
  (offset-y 0))

(defun.ps set-model-pos (&key model x y rotation)
  (let ((graphics (model-graphics model)))
    (graphics.position.set (+ x (model-offset-x model))
                           (+ y (model-offset-y model)))
    (when rotation
      (setf graphics.rotation rotation))))

;; --- line --- ;;

(defun.ps make-line (&key pos-a pos-b color (line-width 1))
  (let ((line (new (#j.PIXI.Graphics#))))
    (line.line-style line-width color 1)
    (line.move-to (car pos-a) (cadr pos-a))
    (line.line-to (car pos-b) (cadr pos-b))
    (make-model :graphics line)))

(defun.ps make-lines (&key pnt-list color (line-width 1))
  (let ((line (new (#j.PIXI.Graphics#)))
        (head-pnt (car pnt-list))
        (rest-pnts (car pnt-list)))
    (unless head
      (return-from make-lines line))
    (line.line-style line-width color 1)
    (line.move-to (car head-pnt) (cadr head-pnt))
    (dolist (pnt rest-pnts)
      (line.line-to (car pnt) (cadr pnt)))
    (make-model :graphics line)))

;; --- rectangle --- ;;

(defun.ps make-solid-rect (&key width height color (alpha 1))
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.begin-fill color alpha)
    (rect.draw-rect 0 0 width height)
    (rect.end-fill)
    (make-model :graphics rect)))

(defun.ps make-wired-rect (&key width height color (line-width 1))
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.line-style line-width color 1)
    (rect.draw-rect 0 0 width height)
    (make-model :graphics rect)))

;; --- arc --- ;;

(defun.ps make-arc (&key start-angle sweep-angle r color (line-width 1))
  (let ((arc (new (#j.PIXI.Graphics#))))
    (arc.line-style line-width color 1)
    (arc.arc 0 0 r start-angle (+ start-angle sweep-angle))
    (make-model :graphics arc)))

;; --- circle --- ;;

(defun.ps make-solid-circle (&key r color (alpha 1))
  (let ((circle (new (#j.PIXI.Graphics#))))
    (circle.begin-fill color alpha)
    (circle.draw-circle 0 0 r)
    (circle.end-fill)
    (make-model :graphics circle)))

;; TODO: alpha
(defun.ps make-wired-circle (&key r color (line-width 1))
  (let ((circle (new (#j.PIXI.Graphics#))))
    (circle.line-style line-width color 1)
    (circle.draw-circle 0 0 r)
    (make-model :graphics circle)))
