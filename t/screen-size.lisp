(defpackage cl-csr/t/screen-size
  (:use :cl
        :rove
        :cl-csr/screen-size)
  (:import-from :cl-csr/protocol
                :code-to-name)
  (:import-from :cl-csr/ws/mock-server
                :add-mock-client)
  (:import-from :cl-csr/ws/mock-client
                :get-latest-messages)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server
                :with-update-frame
                :expected-kind-seq-p
                :get-default-test-screen-size))
(in-package :cl-csr/t/screen-size)

(defmacro with-one-cycle (&body body)
  `(with-update-frame
     ,@body
     (update-screen-size)))

(deftest test-screen-size
  (with-mock-ws-server (ws-server)
    (multiple-value-bind (def-width def-height) (get-default-test-screen-size)
      (let ((client-table (make-hash-table))
            (test-table `((:desc "Send a screen-size message to a new client"
                                 :new-client-ids (0)
                                 ;; ((client-id (kind...))...)
                                 :exp-kinds ((0 (:frame-start :set-screen-size :frame-end)))
                                 :exp-width ,def-width
                                 :exp-height ,def-height)
                          (:desc "Send no screen-size message to an existing client"
                                 :exp-kinds ((0 (:frame-start :frame-end)))
                                 :exp-width ,def-width
                                 :exp-height ,def-height)
                          (:desc "Send a new screen-size message"
                                 :proc ,(lambda ()
                                          (set-screen-size :width 1000 :height 500))
                                 :exp-kinds ((0 (:frame-start :set-screen-size :frame-end)))
                                 :exp-width 1000
                                 :exp-height 500)
                          (:desc "Send a new screen-size message"
                                 :new-client-ids (1)
                                 :proc ,(lambda ()
                                          (set-screen-size :width 2000 :height 1000))
                                 :exp-kinds ((0 (:frame-start :set-screen-size :frame-end))
                                             ;; TODO: Prevent sending an old message when other messages are sent.
                                             (1 (:frame-start :set-screen-size :set-screen-size :frame-end)))
                                 :exp-width 2000
                                 :exp-height 1000))))
        (dolist (tt test-table)
          (destructuring-bind (&key desc new-client-ids proc exp-kinds exp-width exp-height)
              tt
            (testing desc
              (dolist (id new-client-ids)
                (setf (gethash id client-table)
                      (add-mock-client ws-server id)))
              (with-one-cycle
                (when proc
                  (funcall proc)))
              (dolist (pair exp-kinds)
                (destructuring-bind (id kind-seq) pair
                  (let* ((client (gethash id client-table))
                         (messages (get-latest-messages client)))
                    (assert client)
                    (ok (expected-kind-seq-p messages kind-seq))
                    (dolist (message messages)
                      (when (eq (code-to-name (gethash :kind message))
                                :set-screen-size)
                        (let ((data (gethash :data message)))
                          (ok (= (gethash :width data) exp-width))
                          (ok (= (gethash :height data) exp-height))))))))
              (multiple-value-bind (w h) (get-screen-size)
                (ok (= w exp-width))
                (ok (= h exp-height))))))))))
