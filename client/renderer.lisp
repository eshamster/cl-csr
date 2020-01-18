(defpackage cl-csr/client/renderer
  (:use :cl)
  (:export :set-screen-size
           :get-screen-size
           :get-screen-scale
           :get-rendered-dom
           :init-renderer
           :add-graphics
           :remove-graphics)
  (:import-from :cl-csr/client/camera
                :init-camera
                :set-camera-params)
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

(defvar.ps+ *resize-to-screen-p* t)
(defvar.ps+ *rendered-dom* nil)

;; Note: The values of width (800) and height (600) are temporal.
;; They are immediately overwritten by ":set-screen-size" operation just after connecting.
(defvar.ps+ *screen-width* 800)
(defvar.ps+ *screen-height* 600)
(defvar.ps+ *screen-scale* 1)

(defstruct.ps+ renderer
    app
  container)

;; --- interface --- ;;

(defun.ps+ get-screen-size ()
  (values *screen-width* *screen-height*))

(defun.ps+ get-screen-scale ()
  *screen-scale*)

(defun.ps+ get-rendered-dom ()
  *rendered-dom*)

(defun.ps set-screen-size (renderer screen-width screen-height)
  (let* ((app (renderer-app renderer))
         (style app.renderer.view.style)
         (scale (if *resize-to-screen-p*
                    (min (/ window.inner-width screen-width)
                         (/ window.inner-height screen-height))
                    1))
         (width (* screen-width scale))
         (height (* screen-height scale)))
    (app.renderer.resize screen-width screen-height)
    (setf style.width (+ width "px")
          style.height (+ height "px")
          style.position "absolute"
          style.left (+ (/ (- window.inner-width width) 2) "px")
          style.top (+ (/ (- window.inner-height height) 2) "px"))
    (setf *screen-width* screen-width
          *screen-height* screen-height
          *screen-scale* scale)))

(defun.ps init-renderer (rendered-dom app)
  (let* ((container (new #j.PIXI.Container#))
         (renderer (make-renderer :app app
                                  :container container)))
    (app.stage.add-child container)
    (set-screen-size renderer *screen-width* *screen-height*)
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
                                       (set-screen-size renderer *screen-width* *screen-height*))
                                     100)))))
    renderer))

(defun.ps add-graphics (renderer graphics)
  (let ((container (renderer-container renderer)))
    (container.add-child graphics)))

(defun.ps remove-graphics (renderer graphics)
  (let ((container (renderer-container renderer)))
    (container.remove-child graphics)))
