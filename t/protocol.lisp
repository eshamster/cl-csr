(defpackage cl-csr/t/protocol
  (:use :cl
        :rove
        :cl-csr/protocol)
  (:import-from :cl-csr/mock/ws-client-mock
                :get-bufferred-messages)
  (:import-from :cl-csr/mock/ws-server-mock
                :add-mock-client)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server
                :expected-kind-seq-p))
(in-package :cl-csr/t/protocol)

(deftest test-for-protocol
  (with-mock-ws-server (ws-server)
    (let ((client (add-mock-client ws-server 0)))
      (send-frame-start 0 0)
      (ok (expected-kind-seq-p (get-bufferred-messages client)
                               '(:frame-start))))))
