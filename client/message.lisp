(defpackage cl-csr/client/message
  (:use :cl)
  (:export :dequeue-draw-commands-list
           :interpret-draw-command
           :process-message)
  (:import-from :cl-csr/client/camera
                :set-camera-params)
  (:import-from :cl-csr/client/frame-counter
                :get-frame-count)
  (:import-from :cl-csr/client/graphics
                :make-solid-rect
                :make-wired-rect
                :make-solid-circle
                :make-wired-circle
                :make-line
                :make-arc)
  (:import-from :cl-csr/client/font
                :interpret-font-message
                :make-text-mesh)
  (:import-from :cl-csr/protocol
                :code-to-name
                :name-to-code
                :draw-code-p
                :texture-code-p
                :font-code-p
                :number-to-bool)
  (:import-from :cl-csr/client/renderer
                :get-screen-size
                :set-screen-size)
  (:import-from :cl-csr/client/texture
                :interpret-texture-message
                :make-image-mesh)
  (:import-from :cl-csr/utils/buffered-queue
                :init-buffered-queue
                :queue-to-buffer
                :dequeue-list-from-buffer)
  (:import-from :parenscript
                :chain
                :new
                :@)
  (:import-from :ps-experiment
                :defvar.ps
                :defvar.ps+
                :defstruct.ps+
                :defun.ps
                :defun.ps+
                :enable-ps-experiment-syntax))
(in-package :cl-csr/client/message)

(enable-ps-experiment-syntax)

(defvar.ps+ *frame-json-buffer* (list)) ; per frame

(defvar.ps+ *draw-command-buffer* (list)) ; per frame
;; frames
;; TODO: Adjust parameters of init-buffered-queue
(defvar.ps+ *draw-command-queue* (init-buffered-queue
                                  :min-count   1
                                  :start-count 2
                                  :max-count   4))

(defun.ps push-draw-command-to-buffer (parsed-message)
  (*draw-command-buffer*.push parsed-message))

(defun.ps queue-draw-commands-in-buffer ()
  (queue-to-buffer *draw-command-queue* (list *draw-command-buffer*))
  (setf *draw-command-buffer* (list)))

(defun.ps dequeue-draw-commands-list ()
  (if (= (mod (get-frame-count)
              (round (/ *client-fps* *server-fps*)))
         0)
      (dequeue-list-from-buffer *draw-command-queue*)
      (list)))

(defun.ps push-message-to-buffer (parsed-message-list)
  (incf *receive-count-in-frame*)
  (let ((frame-end-p nil))
    (dolist (message parsed-message-list)
      (*frame-json-buffer*.push message)
      (when (target-kind-p :frame-end message)
        (setf frame-end-p t)))
    frame-end-p))

(defvar.ps+ *client-fps* 60)
(defvar.ps+ *server-fps* 30) ; set by :set-fps protocol

;; debug
(defvar.ps+ *receive-count-in-frame* 0)

(defun.ps print-message-stat (message-stat)
  (let ((total 0)
        (text ""))
    (dolist (key (sort (-object.keys message-stat) (lambda (a b) (< a b))))
      (let ((count (gethash key message-stat)))
        (incf text (+ (code-to-name key) ":" #\Tab
                      count #\Newline))
        (incf total count)))
    (setf text
          (+ "TOTAL: " total #\Newline
             "Receive Count: " *receive-count-in-frame* #\Newline
             "---" #\Newline
             text))
    (setf (chain document (get-element-by-id "js-code") value) text)
    (setf *receive-count-in-frame* 0)))

(defun.ps+ process-message (message)
  (let ((parsed-message (receiving-to-json message)))
    (when (push-message-to-buffer parsed-message)
      (let ((message-stat (make-hash-table)))
        (dolist (parsed *frame-json-buffer*)
          (let ((kind-code (gethash :kind parsed)))
            (symbol-macrolet ((count (gethash kind-code message-stat)))
              (unless count
                (setf count 0))
              (incf count))
            (cond ((draw-code-p kind-code)
                   (push-draw-command-to-buffer parsed))
                  ((texture-code-p kind-code)
                   (interpret-texture-message kind-code parsed))
                  ((font-code-p kind-code)
                   (interpret-font-message kind-code parsed))
                  (t (ecase (code-to-name kind-code)
                       ((:frame-start :frame-end) t)
                       (:log-console (interpret-log-console parsed))
                       (:set-screen-size (interpret-set-screen-size parsed))
                       (:set-camera (interpret-set-camera-params parsed))
                       (:set-fps (interpret-set-fps parsed)))))))
        (print-message-stat message-stat))
      (queue-draw-commands-in-buffer)
      (setf *frame-json-buffer* (list)))))

(defun.ps+ target-kind-p (kind parsed-message)
  (eq (gethash :kind parsed-message)
      (name-to-code kind)))

(defun.ps receiving-to-json (message)
  (#j.JSON.parse# message))

(defun.ps interpret-log-console (command)
  (console.log (@ command :data :message)))

(defun.ps interpret-set-screen-size (command)
  (set-screen-size (@ command :data :width)
                   (@ command :data :height)))

(defun.ps interpret-set-camera-params (command)
  (let ((center-x (@ command :data :center-x))
        (center-y (@ command :data :center-y))
        (scale (@ command :data :scale)))
    (multiple-value-bind (width height) (get-screen-size)
      (set-camera-params
       :offset-x (- center-x (/ width scale 2))
       :offset-y (- center-y (/ height scale 2))
       :scale scale))))

(defun.ps interpret-set-fps (command)
  (setf *server-fps* (@ command :data :value)))

;; TODO: The followings should be moved to another package

(defstruct.ps+ draw-info
  kind
  data ; hash table
  mesh)

(defvar.ps+ *draw-info-table* (make-hash-table)
  "Key: id, Value: draw-info")

(defun.ps update-common-mesh-params (mesh data-table)
  (mesh.position.set (gethash :x data-table)
                     (gethash :y data-table))
  ;; TODO: set z-index (gethash :depth data-table)
  (let ((rotate (gethash :rotate data-table)))
    (when rotate
      (setf mesh.rotation rotate))))

(defun.ps+ make-mesh-by-command (command)
  (let* ((kind (code-to-name (gethash :kind command)))
         (data (gethash :data command))
         (mesh (ecase kind
                 (:draw-circle
                  (if (number-to-bool (gethash :fill-p data))
                      (make-solid-circle :r (gethash :r data)
                                         :color (gethash :color data))
                      (make-wired-circle :r (gethash :r data)
                                         :color (gethash :color data))))
                 (:draw-rect
                  (if (number-to-bool (gethash :fill-p data))
                      (make-solid-rect :width (gethash :width data)
                                       :height (gethash :height data)
                                       :color (gethash :color data))
                      (make-wired-rect :width (gethash :width data)
                                       :height (gethash :height data)
                                       :color (gethash :color data))))
                 (:draw-line
                  (make-line :pos-a (list (gethash :x1 data)
                                          (gethash :y1 data))
                             :pos-b (list (gethash :x2 data)
                                          (gethash :y2 data))
                             :color (gethash :color data)))
                 (:draw-arc
                  (make-arc :start-angle (gethash :start-angle data)
                            :sweep-angle (gethash :sweep-angle data)
                            :r (gethash :r data)
                            :color (gethash :color data)))
                 (:draw-image
                  (make-image-mesh :image-id (gethash :image-id data)
                                   :width (gethash :width data)
                                   :height (gethash :height data)
                                   :color (gethash :color data)))
                 (:draw-text
                  (make-text-mesh :text (gethash :text data)
                                  :font-id (gethash :font-id data)
                                  :width (gethash :width data)
                                  :height (gethash :height data)
                                  :color (gethash :color data))))))
    (update-common-mesh-params mesh data)
    mesh))

(defun.ps add-mesh-to-scene (app mesh)
  (app.stage.add-child mesh))

(defun.ps remove-mesh-from-scene (app mesh)
  (app.stage.remove-child mesh))

;; Note: change of color can be achieved without recreating mesh.
;;       But currently recreate for easy of programming.
(defun.ps+ should-recreate-p (prev-info new-kind new-data)
  (let ((prev-kind (draw-info-kind prev-info))
        (prev-data (draw-info-data prev-info)))
    (flet ((eq-params (&rest key-list)
             (every (lambda (key)
                      (= (gethash key prev-data)
                         (gethash key new-data)))
                    key-list)))
      (or (not (eq prev-kind new-kind))
          (case new-kind
            (:draw-circle
             (not (eq-params :fill-p :color :r)))
            (:draw-rect
             (not (eq-params :fill-p :color :width :height)))
            (:draw-image
             (not (eq-params :width :height :color :image-id)))
            (:draw-text
             (not (eq-params :width :height :color :text :font-id)))
            (t t))))))

(defun.ps+ add-or-update-mesh (app command)
  (let* ((kind (code-to-name (gethash :kind command)))
         (data (gethash :data command))
         (id (gethash :id data))
         (prev-info (gethash id *draw-info-table*)))
    (cond ((eq kind :delete-draw-object) ; delete
           (when (gethash id *draw-info-table*)
             (remhash id *draw-info-table*)
             (remove-mesh-from-scene app (draw-info-mesh prev-info))))
          ((null prev-info) ; add
           (let* ((mesh (make-mesh-by-command command)))
             (setf (gethash id *draw-info-table*)
                   (make-draw-info :kind kind
                                   :data data
                                   :mesh mesh))
             (add-mesh-to-scene app mesh)))
          ((should-recreate-p prev-info kind data) ; recreate
           (remhash id *draw-info-table*)
           (remove-mesh-from-scene app (draw-info-mesh prev-info))
           (add-or-update-mesh app command))
          (t ; simple update
           (update-common-mesh-params
            (draw-info-mesh prev-info) data)
           (setf (draw-info-data prev-info) data)))))

(defun.ps+ interpret-draw-command (app command)
  (add-or-update-mesh app command))
