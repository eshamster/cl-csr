(defpackage cl-csr/t/mock/ws-server-mock
  (:use :cl
        :rove
        :cl-csr/mock/ws-server-mock)
  (:import-from :cl-csr/ws-server
                :send-from-server
                :*target-client-id-list*
                :pop-new-client-ids
                :pop-deleted-client-ids
                :pop-client-messages
                :make-client-message)
  (:import-from :cl-csr/mock/ws-client-mock
                :get-bufferred-messages)
  (:import-from :cl-csr/t/test-utils
                :make-dummy-message
                :make-dummy-client-message
                :expected-kind-seq-p
                :expected-client-messages-p))
(in-package :cl-csr/t/mock/ws-server-mock)

(defun same-client-ids-p (ids1 ids2)
  (equalp (sort ids1 #'<) (sort ids2 #'<)))

(deftest test-add-and-delete-dummy-client
  (let ((wsm (make-ws-server-mock))
        (test-table '((:desc "Add clients"
                       :add-client-ids (0 1)
                       :exp-new-client-ids (0 1)
                       :exp-client-ids (0 1))
                      (:desc "Add an additional client"
                       :add-client-ids (2)
                       :exp-new-client-ids (2)
                       :exp-client-ids (0 1 2))
                      (:desc "Error if try to add already added client"
                       :add-client-ids (1)
                       :add-error-exp-p t)
                      (:desc "Error if try to delete not added client"
                       :delete-client-ids (100)
                       :delete-error-exp-p t)
                      (:desc "Delete clients"
                       :delete-client-ids (0 2)
                       :exp-deleted-client-ids (0 2)
                       :exp-client-ids (1))
                      (:desc "Add deleted client again"
                       :add-client-ids (0)
                       :exp-new-client-ids (0)
                       :exp-client-ids (0 1)))))
    (dolist (tt test-table)
      (block one-test
        (destructuring-bind (&key desc
                                  add-client-ids
                                  delete-client-ids
                                  add-error-exp-p
                                  delete-error-exp-p
                                  exp-new-client-ids
                                  exp-client-ids
                                  exp-deleted-client-ids)
            tt
          (testing desc
            ;; - add - ;;
            (dolist (id add-client-ids)
              (if add-error-exp-p
                  (ok (signals (add-mock-client wsm id)))
                  (ok (not (signals (add-mock-client wsm id))))))
            (when add-error-exp-p
              (return-from one-test))
            ;; - delete - ;;
            (dolist (id delete-client-ids)
              (if delete-error-exp-p
                  (ok (signals (delete-mock-client wsm id)))
                  (ok (not (signals (delete-mock-client wsm id))))))
            (when delete-error-exp-p
              (return-from one-test))
            ;; - check ids - ;;
            (ok (same-client-ids-p (get-mock-client-ids wsm) exp-client-ids))
            (ok (same-client-ids-p (pop-new-client-ids wsm) exp-new-client-ids))
            (ok (same-client-ids-p (pop-deleted-client-ids wsm) exp-deleted-client-ids))))))))

(deftest test-send-from-server
  (let* ((wsm (make-ws-server-mock))
         (wcm0 (add-mock-client wsm 0))
         (wcm1 (add-mock-client wsm 1)))
    (let ((testing '((:desc "send to all clients by :all keyward"
                      :target-ids :all
                      :kind :frame-start
                      :expect-buffer-0 (:frame-start)
                      :expect-buffer-1 (:frame-start))
                     (:desc "send to multiple clients"
                      :target-ids (0 1)
                      :kind :draw-rect
                      :expect-buffer-0 (:frame-start :draw-rect)
                      :expect-buffer-1 (:frame-start :draw-rect))
                     (:desc "send to a client"
                      :target-ids (1)
                      :kind :draw-circle
                      :expect-buffer-0 (:frame-start :draw-rect)
                      :expect-buffer-1 (:frame-start :draw-rect :draw-circle)))))
      (dolist (tt testing)
        (destructuring-bind (&key desc target-ids kind
                                  expect-buffer-0 expect-buffer-1)
            tt
          (testing desc
            (let ((*target-client-id-list* target-ids))
              (send-from-server wsm (list (make-dummy-message kind)))
              (ok (expected-kind-seq-p (get-bufferred-messages wcm0)
                                       expect-buffer-0))
              (ok (expected-kind-seq-p (get-bufferred-messages wcm1)
                                       expect-buffer-1)))))))))

(deftest test-receive-message-from-client
  (let ((wsm (make-ws-server-mock)))
    (add-mock-client wsm 0)
    (add-mock-client wsm 1)
    (let ((test-table `((:desc "receiven a message"
                         :messages ((0 :key-down))
                         :expected ((0 :key-down)))
                        (:desc "receiven multiple messages"
                         :messages ((0 :key-down)
                                    (0 :mouse-move)
                                    (1 :key-up))
                         :expected ((0 :key-down)
                                    (0 :mouse-move)
                                    (1 :key-up))))))
      (dolist (tt test-table)
        (destructuring-bind (&key desc messages expected)
            tt
          (testing desc
            (dolist (message messages)
              (receive-message-from-client
               wsm (car message) (make-dummy-message (cadr message))))
            (let ((cl-messages (pop-client-messages wsm)))
              (ok (expected-client-messages-p cl-messages expected)))
            (ok (null (pop-client-messages wsm)))))))))
