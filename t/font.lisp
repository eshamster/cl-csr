(defpackage cl-csr/t/font
  (:use :cl
        :rove
        :cl-csr/font)
  (:import-from :cl-csr/protocol
                :code-to-name)
  (:import-from :cl-csr/mock/ws-server-mock
                :add-mock-client)
  (:import-from :cl-csr/mock/ws-client-mock
                :get-latest-messages)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server
                :with-update-frame
                :expected-kind-seq-p))
(in-package :cl-csr/t/font)

(defmacro with-one-cycle (&body body)
  `(with-update-frame
     ,@body
     (update-font)))

(deftest test-font-update
  (with-mock-ws-server (ws-server)
    (let ((client-table (make-hash-table))
          (test-table `(;; Known issue: If a client is added at the same frame
                        ;; where a font is loaded, duplicated :load-font
                        ;; messaseges will be sent to the client.
                        ;; 
                        ;; (:desc "Load a font"
                        ;;  :new-client-ids (0)
                        ;;  :proc ,(lambda ()
                        ;;           (load-font :name :sample-font
                        ;;                      :font-name "Arial"))
                        ;;  ((client-id (kind...))...)
                        ;;  :exp-kinds ((0 (:frame-start :load-font :frame-end))))
                        (:desc "Load a font"
                         :proc ,(lambda ()
                                  (load-font :name :sample-font
                                             :font-name "Arial")))
                        (:desc "Send font when a client is added"
                         :new-client-ids (0)
                         :exp-kinds ((0 (:frame-start :load-font :frame-end))))
                        (:desc "Send nothing if there is no client and no font"
                               :exp-kinds ((0 (:frame-start :frame-end))))
                        (:desc "Send a new font to an existing client"
                         :proc ,(lambda ()
                                  (load-font :name :sample-font2
                                             :font-name "Arial"))
                         :exp-kinds ((0 (:frame-start :load-font :frame-end)))))))
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
                (testing (format nil "client: ~D" id)
                  (let* ((client (gethash id client-table))
                         (messages (get-latest-messages client)))
                    (assert client)
                    (ok (expected-kind-seq-p messages kind-seq))))))))))))

(deftest test-font-others
  (with-mock-ws-server (ws-server)
    ;; load a font as :sample-font
    (testing "load-font"
      (dolist (tt '((:sample-font "Arial" nil)
                    ("not a keyword" "Arial" t)))
        (destructuring-bind (name font-name exp-error) tt
          (if exp-error
              (ok (signals (load-font :name name
                                      :font-name font-name)))
              (ok (not (signals (load-font :name name
                                           :font-name font-name))))))))
    (testing "get-font-id"
      (ok (numberp (get-font-id :sample-font)))
      (ok (null (get-font-id :not-exist-font))))))
