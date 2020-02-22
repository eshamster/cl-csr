(defpackage cl-csr/t/graphics
  (:use :cl
        :rove
        :cl-csr/graphics)
  (:import-from :cl-csr/client-list-manager
                :update-client-list)
  (:import-from :cl-csr/frame-counter
                :incf-frame-count
                :get-frame-count
                :reset-frame-count
                :incf-index-in-frame)
  (:import-from :cl-csr/protocol
                :send-frame-start
                :send-frame-end)
  (:import-from :cl-csr/mock/ws-server-mock
                :add-mock-client)
  (:import-from :cl-csr/mock/ws-client-mock
                :get-latest-messages)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server
                :expected-kind-seq-p))
(in-package :cl-csr/t/graphics)

(defmacro with-one-cycle (&body body)
  `(progn (update-client-list)
          (send-frame-start (get-frame-count) (incf-index-in-frame))
          ,@body
          (update-graphics)
          (send-frame-end (get-frame-count) (incf-index-in-frame))
          (incf-frame-count)))

(deftest test-single-client
  (with-mock-ws-server (ws-server)
    (let* ((client-id 0)
           (client (add-mock-client ws-server client-id))
           (test-table `((:desc "Send a draw message"
                          :proc ,(lambda ()
                                   (draw-circle :id 0 :x 1 :y 2 :depth 4 :color 5 :r 6))
                          :exp-kinds (:frame-start :draw-circle :frame-end))
                         (:desc "Not send if exactly same message"
                          :proc ,(lambda ()
                                   (draw-circle :id 0 :x 1 :y 2 :depth 4 :color 5 :r 6))
                          :exp-kinds (:frame-start :frame-end))
                         (:desc "Send if some parameter is updated"
                          :proc ,(lambda ()
                                   (draw-circle :id 0 :x 11 :y 2 :depth 4 :color 5 :r 6))
                          :exp-kinds (:frame-start :draw-circle :frame-end))
                         (:desc "Delete if same id data is not sended"
                                :exp-kinds (:frame-start :delete-draw-object :frame-end))
                         (:desc "Skip sending message"
                          :proc ,(lambda ()
                                   (skip-drawing-in-this-frame)
                                   (draw-circle :id 1 :x 1 :y 2 :depth 4 :color 5 :r 6))
                          :exp-kinds (:frame-start :frame-end))
                         (:desc "Send skipped message and new message"
                          :proc ,(lambda ()
                                   (draw-circle :id 1 :x 1 :y 2 :depth 4 :color 5 :r 6)
                                   (draw-rect :id 2 :x 1 :y 2 :depth 4 :color 5
                                              :width 6 :height 7 :rotate 8))
                          :exp-kinds (:frame-start :draw-circle :draw-rect :frame-end)))))
      (dolist (tt test-table)
        (destructuring-bind (&key desc proc exp-kinds)
            tt
          (testing desc
            (with-one-cycle
              (when proc
                (funcall proc)))
            (ok (expected-kind-seq-p (get-latest-messages client) exp-kinds))))))))

(deftest test-multiple-clients
  (with-mock-ws-server (ws-server)
    (let ((client-table (make-hash-table))
          (test-table `((:desc "Send a draw message"
                         :new-client-ids (0)
                         :proc ,(lambda ()
                                  (draw-circle :id 0 :x 1 :y 2 :depth 4 :color 5 :r 6))
                         ;; ((client-id (kind...))...)
                         :exp-kinds ((0 (:frame-start :draw-circle :frame-end))))
                        (:desc "Same message is sent only to a new client"
                         :new-client-ids (1)
                         :proc ,(lambda ()
                                  (draw-circle :id 0 :x 1 :y 2 :depth 4 :color 5 :r 6))
                         :exp-kinds ((0 (:frame-start :frame-end))
                                     (1 (:frame-start :draw-circle :frame-end)))))))
      (let ()
        (dolist (tt test-table)
          (destructuring-bind (&key desc new-client-ids proc exp-kinds)
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
                  (let ((client (gethash id client-table)))
                    (assert client)
                    (ok (expected-kind-seq-p (get-latest-messages client) kind-seq))))))))))))
