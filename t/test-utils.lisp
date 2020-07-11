(defpackage cl-csr/t/test-utils
  (:use :cl
        :rove)
  (:export :expected-kind-seq-p
           :expected-client-messages-p
           :with-mock-ws-server
           :make-dummy-message
           :make-dummy-client-message
           :with-update-frame
           :get-default-test-screen-size)
  (:import-from :cl-csr/camera
                :with-clean-camera-info)
  (:import-from :cl-csr/font
                :with-clean-font-state)
  (:import-from :cl-csr/frame-counter
                :incf-frame-count
                :get-frame-count
                :reset-frame-count
                :incf-index-in-frame)
  (:import-from :cl-csr/graphics
                :with-clean-graphics-state)
  (:import-from :cl-csr/input
                :with-clean-input-state)
  (:import-from :cl-csr/protocol
                :name-to-code
                :code-to-name
                :with-protocol-state
                :make-protocol-state
                :send-frame-start
                :send-frame-end)
  (:import-from :cl-csr/client-list-manager
                :update-client-list
                :with-clean-client-list-manager)
  (:import-from :cl-csr/screen-size
                :with-clean-screen-size)
  (:import-from :cl-csr/texture
                :with-clean-texture-state)
  (:import-from :cl-csr/ws/ws
                :client-message
                :make-client-message
                :client-message-client-id
                :client-message-message
                :with-ws-server)
  (:import-from :cl-csr/ws/mock-server
                :make-ws-server-mock))
(in-package :cl-csr/t/test-utils)

(defun make-dummy-message (kind)
  (let ((res (make-hash-table)))
    (setf (gethash :kind res)
          (name-to-code kind))
    res))

(defun make-dummy-client-message (client-id kind)
  (make-client-message :client-id client-id
                       :message (make-dummy-message kind)))

;; --- ;;

(defvar *default-test-screen-width* 800)
(defvar *default-test-screen-height* 600)

(defun get-default-test-screen-size ()
  (values *default-test-screen-width*
          *default-test-screen-height*))

(defmacro with-mock-ws-server ((server-var) &body body)
  `(let ((,server-var (make-ws-server-mock)))
     (with-ws-server (,server-var)
       (with-clean-client-list-manager
         (with-protocol-state ((make-protocol-state))
           (with-clean-graphics-state
             (with-clean-input-state
               (with-clean-screen-size (*default-test-screen-width*
                                        *default-test-screen-height*)
                 (with-clean-camera-info
                   (with-clean-texture-state
                     (with-clean-font-state
                       ,@body)))))))))))

(defmacro with-update-frame (&body body)
  `(progn (update-client-list)
          (send-frame-start (get-frame-count) (incf-index-in-frame))
          ,@body
          (send-frame-end (get-frame-count) (incf-index-in-frame))
          (incf-frame-count)))

;; --- ;;

(defun expected-kind-p (message expected)
  (eq (code-to-name (gethash :kind message))
      expected))

(deftest test-expected-kind-p
  (ok (expected-kind-p (make-dummy-message :frame-start) :frame-start))
  (ok (not (expected-kind-p (make-dummy-message :frame-start) :frame-end))))

;; --- ;;

(defun messages-to-kind-list (messages)
  (mapcar (lambda (message)
            (code-to-name (gethash :kind message)))
          messages))

(defun expected-kind-seq-p (messages expected)
  (and (= (length messages) (length expected))
       (equalp (messages-to-kind-list messages) expected)))

(deftest test-expected-kind-seq-p
  (let ((messages (list (make-dummy-message :frame-start)
                        (make-dummy-message :draw-rect))))
    (ok (expected-kind-seq-p messages '(:frame-start :draw-rect)))
    (ok (not (expected-kind-seq-p messages '(:draw-rect))))
    (ok (not (expected-kind-seq-p messages '(:frame-start :draw-circle)))))
  (testing "empty messages"
    (ok (expected-kind-seq-p nil nil))
    (ok (not (expected-kind-seq-p nil '(:frame-start))))))

;; --- ;;

(defun expected-client-message-p (message expected)
  ;; expected := (client-id kind)
  (destructuring-bind (exp-id exp-kind) expected
    (and (= (client-message-client-id message) exp-id)
         (expected-kind-p (client-message-message message) exp-kind))))

(deftest test-expected-client-message-p
  (let ((message (make-client-message :client-id 0
                                      :message (make-dummy-message :key-down))))
    (ok (expected-client-message-p message '(0 :key-down)))
    (ok (not (expected-client-message-p message '(1 :key-down))))
    (ok (not (expected-client-message-p message '(0 :key-up))))))

;; --- ;;

(defun expected-client-messages-p (messages expected)
  ;; expected := ((client-id kind)...)
  ;; order is not aware
  (and (= (length messages) (length expected))
       (every (lambda (message)
                (check-type message client-message)
                (some (lambda (exp)
                        (expected-client-message-p message exp))
                      expected))
              messages)))

(deftest test-expected-client-messages-p
  (let ((messages (list (make-dummy-client-message 0 :key-down)
                        (make-dummy-client-message 1 :key-up))))
    (ok (expected-client-messages-p
         messages
         '((0 :key-down)
           (1 :key-up))))
    (ok (expected-client-messages-p
         messages
         '((1 :key-up)
           (0 :key-down))))
    (ok (not (expected-client-messages-p
              messages
              '((0 :key-down)
                (0 :key-down)
                (1 :key-up)))))
    (ok (not (expected-client-messages-p
              messages
              '((0 :key-down)))))
    (ok (not (expected-client-messages-p
              messages
              '((1 :key-down)
                (0 :key-up)))))))
