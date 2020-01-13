(defpackage cl-csr/client/graphics
  (:use :cl)
  (:export :make-line
           :make-lines
           :make-solid-rect
           :make-wired-rect
           :make-solid-regular-polygon
           :make-wired-regular-polygon
           :make-arc
           :make-solid-circle
           :make-wired-circle
           :make-wired-polygon
           :make-solid-polygon
           :change-model-color
           :change-geometry-uvs

           :get-mesh-width
           :get-mesh-height
           :get-mesh-size)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :parenscript
                :create
                :new)
  (:import-from :ps-experiment
                :enable-ps-experiment-syntax
                :defun.ps
                :defun.ps+
                :defmacro.ps+))
(in-package :cl-csr/client/graphics)

;; Note: This is ported from cl-web-2d-game

(enable-ps-experiment-syntax)

;; --- basic funcations and macros

(defun.ps push-vertices-to (geometry raw-vertex-lst)
  (dolist (vertex-as-lst raw-vertex-lst)
    (geometry.vertices.push
     (new (#j.THREE.Vector3# (nth 0 vertex-as-lst)
                             (nth 1 vertex-as-lst)
                             0)))))

(defun.ps push-faces-to (geometry raw-face-lst)
  (dolist (face-as-lst raw-face-lst)
    (geometry.faces.push
     (new (#j.THREE.Face3# (nth 0 face-as-lst)
                           (nth 1 face-as-lst)
                           (nth 2 face-as-lst))))))

(defun.ps+ to-rad (degree)
  (/ (* degree PI) 180))

(defun.ps make-line-model (geometry color)
  (let ((material (new (#j.THREE.LineBasicMaterial# (create :color color)))))
    (new (#j.THREE.Line# geometry material))))

(defmacro.ps+ def-wired-geometry (name args &body body)
  (let ((geometry (gensym)))
    `(defun.ps ,name (&key ,@args color)
       (let ((,geometry (new (#j.THREE.Geometry#))))
         (flet ((push-vertices (&rest rest)
                  (push-vertices-to ,geometry rest)))
           ,@body)
         (make-line-model ,geometry color)))))

(defun.ps make-solid-model (geometry color)
  (let ((material (new (#j.THREE.MeshBasicMaterial# (create :color color)))))
    (new (#j.THREE.Mesh# geometry material))))

(defmacro.ps+ def-solid-geometry (name args &body body)
  (let ((geometry (gensym)))
    `(defun.ps ,name (&key ,@args color)
       (let ((,geometry (new (#j.THREE.Geometry#))))
         (flet ((push-vertices (&rest rest)
                  (push-vertices-to ,geometry rest))
                (push-faces (&rest rest)
                  (push-faces-to ,geometry rest)))
           ,@body)
         (make-solid-model ,geometry color)))))

;; --- utils --- ;;

(defun.ps get-mesh-width (mesh)
  (- mesh.geometry.bounding-box.max.x
     mesh.geometry.bounding-box.min.x))

(defun.ps get-mesh-height (mesh)
  (- mesh.geometry.bounding-box.max.y
     mesh.geometry.bounding-box.min.y))

(defun.ps+ get-mesh-size (mesh)
  (list :width (get-mesh-width mesh)
        :height (get-mesh-height mesh)))

;; --- line --- ;;

(defun.ps make-line (&key pos-a pos-b color (line-width 1))
  (let ((line (new (#j.PIXI.Graphics#))))
    (line.line-style line-width color 1)
    (line.move-to (car pos-a) (cadr pos-a))
    (line.line-to (car pos-b) (cadr pos-b))
    line))

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
    line))

;; --- rectangle --- ;;

(defun.ps make-solid-rect (&key width height color (alpha 1))
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.begin-fill color alpha)
    (rect.draw-rect 0 0 width height)
    (rect.end-fill)
    rect))

(defun.ps make-wired-rect (&key width height color (line-width 1))
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.line-style line-width color 1)
    (rect.draw-rect 0 0 width height)
    rect))

;; --- arc --- ;;

(def-wired-geometry make-wired-arc-with-vertex-count (r n start-angle sweep-angle)
  (dotimes (i n)
    (let ((angle (+ (/ (* sweep-angle i) (1- n)) start-angle)))
      (push-vertices (list (* r (cos angle))
                           (* r (sin angle)))))))

(defun.ps make-arc (&key start-angle sweep-angle r color (line-width 1))
  (let ((arc (new (#j.PIXI.Graphics#))))
    (arc.line-style line-width color 1)
    (arc.arc 0 0 r start-angle (+ start-angle sweep-angle))
    arc))

;; --- regular polygon --- ;;

(def-solid-geometry make-solid-regular-polygon (r n (start-angle 0))
  (dotimes (i n)
    (let ((angle (to-rad (+ (/ (* 360 i) n) start-angle))))
      (push-vertices (list (* r (cos angle))
                           (* r (sin angle))))))
  (push-vertices (list 0 0))
  (dotimes (i n)
    (push-faces (list n i (rem (1+ i) n)))))

(defun.ps+ make-wired-regular-polygon (&key color r n (start-angle 0))
  (make-wired-arc-with-vertex-count
   :color color :r r :start-angle start-angle :sweep-angle (* 2 PI) :n (1+ n)))

;; --- circle --- ;;

(defun.ps make-solid-circle (&key r color (alpha 1))
  (let ((circle (new (#j.PIXI.Graphics#))))
    (circle.begin-fill color alpha)
    (circle.draw-circle 0 0 r)
    (circle.end-fill)
    circle))

;; TODO: alpha
(defun.ps make-wired-circle (&key r color (line-width 1))
  (let ((circle (new (#j.PIXI.Graphics#))))
    (circle.line-style line-width color 1)
    (circle.draw-circle 0 0 r)
    circle))

;; --- arbitrary polygon --- ;;

(def-wired-geometry make-wired-polygon (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices pnt))
  (push-vertices (nth 0 pnt-list)))

(def-solid-geometry make-solid-polygon (pnt-list)
  (dolist (pnt pnt-list)
    (push-vertices pnt))
  (let ((len (length pnt-list)))
    (dotimes (i (1- len))
      (push-faces (list 0
                        (+ i 1)
                        (rem (+ i 2) len))))))

;; --- auxiliary functions --- ;;

(defun.ps change-model-color (model-2d new-color-rgb)
  (with-slots (model) model-2d
    (setf model.material.color (new (#j.THREE.Color# new-color-rgb)))
    (setf model.material.needs-update t))
  model-2d)
