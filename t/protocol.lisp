(defpackage cl-csr/t/protocol
  (:use :cl
        :rove
        :cl-csr/protocol)
  (:import-from :cl-csr/ws/ws
                :*target-client-id-list*)
  (:import-from :cl-csr/ws/mock-client
                :get-bufferred-messages
                :get-latest-messages)
  (:import-from :cl-csr/ws/mock-server
                :add-mock-client)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server
                :expected-kind-seq-p))
(in-package :cl-csr/t/protocol)

(deftest test-for-protocol
  (with-mock-ws-server (ws-server)
    (with-protocol-state ((make-protocol-state :buffer-size 3))
      (let* ((test-table '((:desc "Not send until buffer is full"
                            :messages (:frame-start :draw-rect))
                           (:desc "Send if buffer is full"
                            :messages (:draw-circle)
                            :exp-bufferred (:frame-start :draw-rect :draw-circle))
                           (:desc "Immediately send if frame-end protocol"
                            :messages (:frame-end)
                            :exp-latest (:frame-start :draw-rect :draw-circle :frame-end))
                           (:desc "(prepare)"
                            :messages (:frame-start)
                            :exp-latest (:frame-start :draw-rect :draw-circle :frame-end))
                           (:desc "Immediately send old messages if target is changed"
                            :messages (:draw-rect)
                            :target-client-id-list (0)
                            :exp-bufferred (:frame-start)
                            :exp-latest (:frame-start :draw-rect :draw-circle :frame-end))))
             (client-id 0)
             (client (add-mock-client ws-server client-id)))
        (let ((frame 0)
              (count-in-frame 0))
          (dolist (tt test-table)
            (destructuring-bind (&key desc messages (target-client-id-list :all)
                                      exp-bufferred exp-latest)
                tt
              (testing desc
                (let ((*target-client-id-list* target-client-id-list))
                  (dolist (kind messages)
                    (ecase kind
                      (:frame-start (send-frame-start frame (incf count-in-frame)))
                      (:draw-rect (send-draw-rect frame (incf count-in-frame)))
                      (:draw-circle (send-draw-circle frame (incf count-in-frame)))
                      (:frame-end (send-frame-end frame (incf count-in-frame))
                                  (incf frame)
                                  (setf count-in-frame 0)))))
                (ok (expected-kind-seq-p (get-bufferred-messages client)
                                         exp-bufferred))
                (ok (expected-kind-seq-p (get-latest-messages client)
                                         exp-latest))))))))))
