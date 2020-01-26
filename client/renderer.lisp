(defpackage cl-csr/client/renderer
  (:use :cl)
  (:export :set-screen-size
           :get-screen-offset
           :get-screen-scale
           :set-camera
           :init-renderer
           :update-renderer-after
           :add-graphics
           :remove-graphics
           :graphics-added-p)
  (:import-from :parenscript
                :chain
                :create
                :new)
  (:import-from :ps-experiment
                :defvar.ps
                :defvar.ps+
                :defun.ps
                :defun.ps+
                :defstruct.ps+
                :enable-ps-experiment-syntax))
(in-package :cl-csr/client/renderer)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defstruct.ps+ renderer
    app
  container
  screen-scale)

;; --- interface --- ;;

(defun.ps get-screen-size (renderer)
  (let ((app (renderer-app renderer)))
    (values app.renderer.screen.width
            app.renderer.screen.height)))

(defun.ps get-screen-offset (renderer)
  (let* ((app (renderer-app renderer))
         (style app.renderer.view.style))
    (flet ((parse (val)
             (parse-int (val.replace "px" "")))))
    (values (parse style.left)
            (parse style.top))))

(defun.ps get-screen-scale (renderer)
  (renderer-screen-scale renderer))

(defun.ps set-screen-size (renderer screen-width screen-height)
  (let* ((app (renderer-app renderer))
         (style app.renderer.view.style)
         (scale (min (/ window.inner-width screen-width)
                     (/ window.inner-height screen-height)))
         (width (* screen-width scale))
         (height (* screen-height scale)))
    (setf (renderer-screen-scale renderer) scale)
    (app.renderer.resize screen-width screen-height)
    (setf style.width (+ width "px")
          style.height (+ height "px")
          style.position "absolute"
          style.left (+ (/ (- window.inner-width width) 2) "px")
          style.top (+ (/ (- window.inner-height height) 2) "px"))))

(defun.ps set-camera (renderer center-x center-y scale)
  (let ((container (renderer-container renderer)))
    (setf container.scale.x scale
          container.scale.y scale)
    (multiple-value-bind (width height) (get-screen-size renderer)
      (setf container.x (- (/ width 2) center-x)
            container.y (- (/ height 2) center-y)))))

(defun.ps init-renderer (rendered-dom app)
  (let* ((container (new #j.PIXI.Container#))
         (renderer (make-renderer :app app
                                  :container container)))
    (app.stage.add-child container)
    ;; Note: The values of width (0) and height (0) are temporal.
    ;; They are immediately overwritten by ":set-screen-size" operation just after connecting.
    (set-screen-size renderer 0 0)
    (chain rendered-dom (append-child app.view))
    (setf *rendered-dom* rendered-dom)
    (let ((resize-timer nil))
      (window.add-event-listener
       "resize" (lambda (e)
                  (declare (ignore e))
                  (when resize-timer
                    (clear-timeout resize-timer))
                  (setf resize-timer
                        (set-timeout (lambda ()
                                       (multiple-value-bind (width height)
                                           (get-screen-size renderer)
                                         (set-screen-size renderer width height)))
                                     100)))))
    renderer))

(defun.ps add-graphics (renderer graphics)
  (let ((container (renderer-container renderer)))
    (container.add-child graphics)))

(defun.ps remove-graphics (renderer graphics)
  (let ((container (renderer-container renderer)))
    (container.remove-child graphics)))

(defun.ps graphics-added-p (renderer graphics)
  (declare (ignore renderer))
  (when graphics.parent t))

(defun.ps update-renderer-after (renderer)
  (let ((container (renderer-container renderer)))
    (container.children.sort
     (lambda (a b)
       (- a.z-index b.z-index)))))
