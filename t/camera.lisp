(defpackage cl-csr/t/camera
  (:use :cl
        :rove
        :cl-csr/camera)
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
(in-package :cl-csr/t/camera)

(defmacro with-one-cycle (&body body)
  `(with-update-frame
     ,@body
     (update-camera-info)))

(deftest test-camera
  (with-mock-ws-server (ws-server)
    (multiple-value-bind (sw sh) (get-default-test-screen-size)
      (let ((client-table (make-hash-table))
            (test-table `((:desc "Send a default camera info to a new client"
                           :new-client-ids (0)
                           ;; ((client-id (kind...))...)
                           :exp-kinds ((0 (:frame-start :set-camera :frame-end)))
                           ;; ((client-id x y scale)...)
                           :exp-camera ((0 ,(/ sw 2) ,(/ sh 2) 1)))
                          (:desc "Send no camera message"
                           :exp-kinds ((0 (:frame-start :frame-end)))
                           :exp-camera ((0 ,(/ sw 2) ,(/ sh 2) 1)))
                          (:desc "Send a setting camera center pos message "
                           :proc ,(lambda ()
                                    (set-camera-center-pos 0 100 200))
                           :exp-kinds ((0 (:frame-start :set-camera :frame-end)))
                           :exp-camera ((0 100 200 1)))
                          (:desc "Send a setting camera scale message "
                           :proc ,(lambda ()
                                    (set-camera-scale 0 0.5))
                           :exp-kinds ((0 (:frame-start :set-camera :frame-end)))
                           :exp-camera ((0 100 200 0.5)))
                          (:desc "Defualt parameter is not affected by other clients"
                           :new-client-ids (1)
                           :exp-kinds ((0 (:frame-start :frame-end))
                                       (1 (:frame-start :set-camera :frame-end)))
                           :exp-camera ((0 100 200 0.5)
                                        (1 ,(/ sw 2) ,(/ sh 2) 1))))))
        (dolist (tt test-table)
          (destructuring-bind (&key desc new-client-ids proc exp-kinds exp-camera)
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
                  (testing (format nil "client: ~D" id)
                    (let* ((client (gethash id client-table))
                           (messages (get-latest-messages client)))
                      (assert client)
                      (ok (expected-kind-seq-p messages kind-seq))))))
              (dolist (exp-camera exp-camera)
                (destructuring-bind (id exp-x exp-y exp-scale) exp-camera
                  (testing (format nil "client: ~D" id)
                    ;; client
                    (let* ((client (gethash id client-table))
                           (messages (get-latest-messages client)))
                      (assert client)
                      (dolist (message messages)
                        (when (eq (code-to-name (gethash :kind message))
                                  :set-camera)
                          (let ((data (gethash :data message)))
                            (ok (= (gethash :center-x data) exp-x))
                            (ok (= (gethash :center-y data) exp-y))
                            (ok (= (gethash :scale data) exp-scale))))))
                    ;; server
                    (multiple-value-bind (x y) (get-camera-center-pos id)
                      (ok (= x exp-x))
                      (ok (= y exp-y)))
                    (ok (= (get-camera-scale id) exp-scale))))))))))))
