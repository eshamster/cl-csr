(defpackage cl-csr/t/test-utils
  (:use :cl
        :rove)
  (:export :expected-kind-seq-p
           :expected-client-messages-p
           :make-dummy-message
           :make-dummy-client-message)
  (:import-from :cl-csr/protocol
                :name-to-code
                :code-to-name)
  (:import-from :cl-csr/ws-server
                :client-message
                :make-client-message
                :client-message-client-id
                :client-message-message))
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

(defun expected-kind-p (message expected)
  (eq (code-to-name (gethash :kind message))
      expected))

(deftest test-expected-kind-p
  (ok (expected-kind-p (make-dummy-message :frame-start) :frame-start))
  (ok (not (expected-kind-p (make-dummy-message :frame-start) :frame-end))))

;; --- ;;

(defun expected-kind-seq-p (messages expected)
  (every #'expected-kind-p messages expected))

(deftest test-expected-kind-seq-p
  (let ((messages (list (make-dummy-message :frame-start)
                        (make-dummy-message :draw-rect))))
    (ok (expected-kind-seq-p messages '(:frame-start :draw-rect)))
    (ok (not (expected-kind-seq-p messages '(:draw-rect))))
    (ok (not (expected-kind-seq-p messages '(:frame-start :draw-circle))))))

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