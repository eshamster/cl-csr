(defpackage cl-csr/t/mock/ws-client-mock
  (:use :cl
        :rove
        :cl-csr/mock/ws-client-mock)
  (:import-from :cl-csr/t/test-utils
                :expected-kind-seq-p
                :make-dummy-message))
(in-package :cl-csr/t/mock/ws-client-mock)

(deftest test-ws-client-mock
  (let ((wcm (make-instance 'ws-client-mock))
        (test-table '((:desc "At first, both latest messages and buffer are empty"
                       :kind nil
                       :exp-buffer nil
                       :exp-latest nil)
                      (:desc "Error if non :frame-start is received at first"
                       :kind :draw-rect
                       :exp-buffer nil
                       :exp-latest nil
                       :error-exp-p t)
                      (:desc "Receive :frame-start"
                       :kind :frame-start
                       :exp-buffer (:frame-start)
                       :exp-latest nil)
                      (:desc "Receive a message"
                       :kind :draw-rect
                       :exp-buffer (:frame-start :draw-rect)
                       :exp-latest nil)
                      (:desc "Error if receive :frame-start in frame"
                       :kind :frame-start
                       :exp-buffer (:frame-start :draw-rect)
                       :exp-latest nil
                       :error-exp-p t)
                      (:desc "Receive :frame-end"
                       :kind :frame-end
                       :exp-buffer nil
                       :exp-latest (:frame-start :draw-rect :frame-end))
                      (:desc "Start next frame"
                       :kind :frame-start
                       :exp-buffer (:frame-start)
                       :exp-latest (:frame-start :draw-rect :frame-end))
                      (:desc "end next frame"
                       :kind :frame-end
                       :exp-buffer nil
                       :exp-latest (:frame-start :frame-end)))))
    (dolist (tt test-table)
      (destructuring-bind (&key desc kind exp-buffer exp-latest error-exp-p) tt
        (testing desc
          (when kind
            (if error-exp-p
                (ok (signals (receive-message-from-server
                              wcm (make-dummy-message kind))))
                (ok (not (signals (receive-message-from-server
                                   wcm (make-dummy-message kind)))))))
          (ok (expected-kind-seq-p (get-bufferred-messages wcm) exp-buffer))
          (ok (expected-kind-seq-p (get-latest-messages wcm) exp-latest)))))))
