(defpackage cl-csr/client/texture
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :update-texture
           :interpret-texture-message
           :make-image-model
           :texture-loaded-p)
  (:import-from :cl-csr/protocol
                :code-to-name)
  (:import-from :cl-csr/client/graphics
                :make-solid-rect
                :make-model
                :model-graphics)
  (:import-from :cl-csr/client/utils
                :with-command-data)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :cl-ps-ecs
                :register-func-with-pred))
(in-package :cl-csr/client/texture)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defstruct.ps+ texture-info
  id
  bitmap-image
  alpha-bitmap-image)

(defstruct.ps+ image-info
  id
  texture-id
  uv-x uv-y uv-width uv-height)

(defvar.ps+ *texture-info-buffer* (list))
(defvar.ps+ *texture-info-table* (make-hash-table)
  "Key: texture id, Value: texture-info")
(defvar.ps+ *image-info-table* (make-hash-table)
  "Key: image id, Value: image-info")


;; --- interface --- ;;

;; - for general processor - ;;

(defun.ps+ update-texture ()
  (dolist (tex-info *texture-info-buffer*)
    (setf (gethash (texture-info-id tex-info)
                   *texture-info-table*)
          tex-info))
  ;; XXX: Assure all information in buffer is moved
  (setf *texture-info-buffer* (list)))

;; - for messenger - ;;

(defun.ps+ interpret-texture-message (kind-code command)
  (ecase (code-to-name kind-code)
    (:load-texture
     (with-command-data (path texture-id) command
       (load-texture :path path :id texture-id)))
    (:load-image
     (with-command-data (image-id texture-id uv-x uv-y uv-width uv-height)
         command
       (register-image :id image-id
                       :texture-id texture-id
                       :uv-x uv-x
                       :uv-y uv-y
                       :uv-width uv-width
                       :uv-height uv-height)))))

;; - for drawer - ;;

(defun.ps make-image-model (&key image-id width height color)
  (flet ((make-sprite ()
           (let* ((img-info (find-image-info-by-image-id image-id))
                  (tex-id (image-info-texture-id img-info))
                  (tex (get-texture tex-id)))
             (with-slots (uv-x uv-y uv-width uv-height) img-info
               (init-texture-uv tex uv-x uv-y uv-width uv-height))
             (new (#j.PIXI.Sprite# tex)))))
    ;; If the texture has not been loaded, returns a container with a temporal rectangle
    ;; that has same width, height. Then, rewrites by sprite created using the texture
    ;; after loading it.
    (let ((container (new (#j.PIXI.Container#))))
      (unless (image-loaded-p image-id)
        (let ((dummy-rect (make-solid-rect :width width :height height :color #x888888)))
          (register-func-with-pred
           (lambda ()
             (container.remove-child (model-graphics dummy-rect))
             (container.add-child (make-sprite)))
           (lambda () (image-loaded-p image-id)))
          (container.add-child (model-graphics dummy-rect))
          (return-from make-image-model (make-model :graphics container))))
      ;; the case where the texture has been loaded.
      (container.add-child (make-sprite))
      (make-model :graphics container))))

(defun.ps+ texture-loaded-p (tex-id)
  (gethash tex-id *texture-info-table*))

;; --- internal --- ;;

(defun.ps load-texture (&key path id)
  (let ((name (id-to-name id)))
    (chain (new (#j.PIXI.Loader#))
      (add name path)
      (load (lambda ()
              (push (make-texture-info
                     :id id
                     :bitmap-image (gethash name #j.PIXI.utils.TextureCache#))
                    *texture-info-buffer*))))))

(defun.ps id-to-name (id)
  ;; to string
  (+ "" id))

(defun.ps+ image-loaded-p (image-id)
  (find-tex-info-by-image-id image-id))

(defun.ps+ register-image (&key id texture-id
                                uv-x uv-y uv-width uv-height)
  (setf (gethash id *image-info-table*)
        (make-image-info :id id
                         :texture-id texture-id
                         :uv-x uv-x
                         :uv-y uv-y
                         :uv-width uv-width
                         :uv-height uv-height)))

(defun.ps get-texture (id)
  (let ((tex (gethash (id-to-name id) #j.PIXI.utils.TextureCache#)))
    (assert tex)
    (new (#j.PIXI.Texture# tex))))

;; TODO: unload-texture

(defun.ps+ find-image-info-by-image-id (image-id)
  (gethash image-id *image-info-table*))

(defun.ps+ find-tex-info-by-image-id (image-id)
  (let ((img-info (find-image-info-by-image-id image-id)))
    (when img-info
      (gethash (image-info-texture-id img-info)
               *texture-info-table*))))

(defun.ps init-texture-uv (tex ux uy uw uh)
  (let* ((frame tex.frame)
         (width frame.width)
         (height frame.height))
    (setf tex.frame (new (#j.PIXI.Rectangle# (* width ux)
                                             (* height uy)
                                             (* width uw)
                                             (* height uh))))))
