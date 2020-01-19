(defpackage cl-csr/client/core
  (:use :cl)
  (:export :output-client-js)
  (:import-from :cl-csr/client/font
                :update-font)
  (:import-from :cl-csr/client/frame-counter
                :update-frame-counter)
  (:import-from :cl-csr/client/input
                :init-input)
  (:import-from :cl-csr/client/message
                :process-message
                :update-draw)
  (:import-from :cl-csr/client/renderer
                :init-renderer)
  (:import-from :cl-csr/client/socket
                :register-socket-on-message)
  (:import-from :cl-csr/client/texture
                :update-texture)
  (:import-from :cl-ps-ecs
                :ecs-main)
  (:import-from :parenscript
                :chain
                :create
                :new
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
             (let* ((rendered-dom (document.query-selector "#renderer"))
                    (app (new (#j.PIXI.Application# (create))))
                    (renderer (init-renderer rendered-dom app)))
               (init-input renderer)
               (start-2d-game :app app :renderer renderer)))
           file)))

;; --- initializer --- ;;

(defun.ps start-2d-game (&key app renderer
                              ;; TODO: (resize-to-screen-p t)
                              )
  (app.ticker.add (lambda ()
                    (update-frame-counter)
                    (update-texture)
                    (update-font)
                    (ecs-main)
                    (update-draw renderer))))

(def-top-level-form.ps :run-start-2d-game
  (console.log "test"))
