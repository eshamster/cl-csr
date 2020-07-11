(defpackage cl-csr/t/client-list-manager
  (:use :cl
        :rove
        :cl-csr/client-list-manager)
  (:import-from :cl-csr/ws/mock-client
                :get-bufferred-messages)
  (:import-from :cl-csr/ws/mock-server
                :add-mock-client
                :delete-mock-client)
  (:import-from :cl-csr/ws/ws
                :*target-client-id-list*)
  (:import-from :cl-csr/ws/utils
                :same-target-client-list-p)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server
                :expected-kind-seq-p))
(in-package :cl-csr/t/client-list-manager)

(deftest test-for-client-list-id-getters
  (with-mock-ws-server (ws-server)
    (let ((test-table '((:desc "Add clients"
                         :new-ids (0 1 2 3)
                         :exp-ids (0 1 2 3)
                         :exp-new-ids (0 1 2 3))
                        (:desc "Delete clients"
                         :deleted-ids (0 1)
                         :exp-ids (2 3)
                         :exp-deleted-ids (0 1))
                        (:desc "Add and delete clients"
                         :new-ids (4)
                         :deleted-ids (3)
                         :exp-ids (2 4)
                         :exp-new-ids (4)
                         :exp-deleted-ids (3)))))
      (dolist (tt test-table)
        (destructuring-bind (&key desc
                                  new-ids
                                  deleted-ids
                                  exp-ids
                                  exp-new-ids
                                  exp-deleted-ids)
            tt
          (testing desc
            (dolist (id new-ids)
              (add-mock-client ws-server id))
            (dolist (id deleted-ids)
              (delete-mock-client ws-server id))
            (update-client-list)
            (ok (same-target-client-list-p
                 (get-client-id-list) exp-ids))
            (ok (same-target-client-list-p
                 (get-new-client-id-list) exp-new-ids))
            (ok (same-target-client-list-p
                 (get-deleted-client-id-list) exp-deleted-ids))))))))

(deftest test-for-client-alive-p
  (with-mock-ws-server (ws-server)
    (let ((test-table '((:desc "Befor adding"
                         :exp nil)
                        (:desc "After adding"
                         :add-p t
                         :exp t)
                        (:desc "After deleting"
                         :delete-p t
                         :exp nil))))
      (dolist (tt test-table)
        (destructuring-bind (&key desc
                                  add-p
                                  delete-p
                                  exp)
            tt
          (testing desc
            (let ((client-id 0)
                  (not-added-id 99))
              (when add-p
                (add-mock-client ws-server client-id))
              (when delete-p
                (delete-mock-client ws-server client-id))
              (update-client-list)
              (let ((got (client-alive-p client-id)))
                (ok (or (and got exp)
                        (and (not got) (not exp)))))
              (ok (not (client-alive-p not-added-id))))))))))

(deftest test-for-with-sending-to-new-clients
  (with-mock-ws-server (ws-server)
    (let ((test-table '((:desc "Add clients"
                         :new-ids (0 1 2 3)
                         :exp-new-ids (0 1 2 3))
                        (:desc "Add no client")
                        (:desc "Add and clients again"
                         :new-ids (4 5)
                         :exp-new-ids (4 5)))))
      (dolist (tt test-table)
        (destructuring-bind (&key desc
                                  new-ids
                                  exp-new-ids)
            tt
          (testing desc
            (dolist (id new-ids)
              (add-mock-client ws-server id))
            (update-client-list)
            (with-sending-to-new-clients ()
              (ok (same-target-client-list-p *target-client-id-list* exp-new-ids)))))))))
