(defpackage cl-csr/client/core
  (:use :cl)
  (:export :output-client-js)
  (:import-from :cl-csr/client/camera
                :get-camera)
  (:import-from :cl-csr/client/font
                :update-font)
  (:import-from :cl-csr/client/frame-counter
                :update-frame-counter)
  (:import-from :cl-csr/client/input
                :init-input)
  (:import-from :cl-csr/client/message
                :dequeue-draw-commands-list
                :interpret-draw-command
                :process-message)
  (:import-from :cl-csr/client/renderer
                :get-screen-size
                :init-renderer)
  (:import-from :cl-csr/client/socket
                :register-socket-on-message)
  (:import-from :cl-csr/client/texture
                :update-texture)
  (:import-from :cl-ps-ecs
                :ecs-main)
  (:import-from :parenscript
                :chain
                :@)
  (:import-from :ps-experiment
                :defvar.ps
                :defvar.ps+
                :defun.ps
                :defun.ps+
                :def-top-level-form.ps
                :enable-ps-experiment-syntax
                :with-use-ps-pack))
(in-package :cl-csr/client/core)

(enable-ps-experiment-syntax)

;; --- compiler -- - ;;

(defun output-client-js (file-path)
  (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (princ (with-use-ps-pack (:this)
             (register-socket-on-message #'process-message)
             (init-input))
           file)))

;; --- initializer --- ;;

(defun.ps+ empty-init-func (app)
  (declare (ignore app)))

(defun.ps+ empty-update-func (app)
  (declare (ignore app)))

(defun.ps start-2d-game (&key rendered-dom
                              (resize-to-screen-p t)
                              (init-function #'empty-init-func)
                              (update-function #'empty-update-func))
  (let ((app (init-renderer rendered-dom)))
    (funcall init-function app)
    (app.ticker.add (lambda ()
                      (update-frame-counter)
                      (update-texture)
                      (update-font)
                      (ecs-main)
                      (funcall update-function app)))))

(defun.ps clear-scene (scene)
  (loop :while (> scene.children.length 0)
     :do (scene.remove (@ scene children 0))))

(defun.ps+ update-draw (scene)
  (let ((draw-commands-list (dequeue-draw-commands-list)))
    (dolist (draw-commands draw-commands-list)
      (dolist (command draw-commands)
        (interpret-draw-command scene command)))))

(def-top-level-form.ps :run-start-2d-game
  (start-2d-game :rendered-dom (document.query-selector "#renderer")
                 :update-function #'update-draw))
