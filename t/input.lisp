(defpackage cl-csr/t/input
  (:use :cl
        :rove
        :cl-csr/input)
  (:import-from :cl-csr/protocol
                :name-to-code)
  (:import-from :cl-csr/client-list-manager
                :update-client-list)
  (:import-from :cl-csr/ws/mock-server
                :add-mock-client
                :delete-mock-client
                :receive-message-from-client)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server)
  (:import-from :alexandria
                :plist-hash-table))
(in-package :cl-csr/t/input)

(defun make-input-message (kind data-plist)
  (let ((result (make-hash-table)))
    (setf (gethash :kind result) (name-to-code kind))
    (setf (gethash :data result) (plist-hash-table data-plist))
    result))

(defun make-touch-message (kind data-plist)
  (let ((result (make-hash-table)))
    (setf (gethash :kind result) (name-to-code kind))
    (setf (gethash :data result) (list (plist-hash-table data-plist)))
    result))

;; Note: Internal implementation is tested in detail in cl-csr/t/utils/input.
;; So do only simple test there.
(deftest test-for-keyboard
  (with-mock-ws-server (ws-server)
    (let ((client-id 0))
      (add-mock-client ws-server client-id)
      (update-client-list)
      (testing "key-down"
        (receive-message-from-client
         ws-server client-id (make-input-message :key-down '(:key "a")))
        (update-input)
        (ok (key-down-now-p client-id :a))
        (ok (key-down-p client-id :a)))
      (testing "key-down is kept"
        (update-input)
        (ok (not (key-down-now-p client-id :a)))
        (ok (key-down-p client-id :a)))
      (testing "key-up"
        (receive-message-from-client
         ws-server client-id (make-input-message :key-up '(:key "a")))
        (update-input)
        (ok (key-up-now-p client-id :a))
        (ok (key-up-p client-id :a)))
      (testing "key-up is kept"
        (update-input)
        (ok (not (key-up-now-p client-id :a)))
        (ok (key-up-p client-id :a))))))

(deftest test-for-mouse
  (flet ((exp-mouse-pos-p (client-id exp-x exp-y)
           (multiple-value-bind (x y) (get-mouse-pos client-id)
             (and (= x exp-x)
                  (= y exp-y)))))
    (testing "mouse click"
      (with-mock-ws-server (ws-server)
        (let ((client-id 0))
          (add-mock-client ws-server client-id)
          (update-client-list)
          (testing "mouse-down"
            (receive-message-from-client
             ws-server client-id
             (make-input-message :mouse-down '(:button "left" :x 100 :y 200)))
            (update-input)
            (ok (mouse-down-now-p client-id :left))
            (ok (mouse-down-p client-id :left))
            (ok (exp-mouse-pos-p client-id 100 200)))
          (testing "mouse-down is kept"
            (update-input)
            (ok (not (mouse-down-now-p client-id :left)))
            (ok (mouse-down-p client-id :left))
            (ok (exp-mouse-pos-p client-id 100 200)))
          (testing "mouse-up"
            (receive-message-from-client
             ws-server client-id
             (make-input-message :mouse-up '(:button "left" :x 111 :y 222)))
            (update-input)
            (ok (mouse-up-now-p client-id :left))
            (ok (mouse-up-p client-id :left))
            (ok (exp-mouse-pos-p client-id 111 222)))
          (testing "mouse-up is kept"
            (update-input)
            (ok (not (mouse-up-now-p client-id :left)))
            (ok (mouse-up-p client-id :left))
            (ok (exp-mouse-pos-p client-id 111 222))))))
    (testing "mouse move"
      (with-mock-ws-server (ws-server)
        (let ((client-id 0))
          (add-mock-client ws-server client-id)
          (update-client-list)
          (let ((test-table '((:desc "Move mouse"
                               :move-p t
                               :x 100 :y 200
                               :exp-x 100 :exp-y 200)
                              (:desc "Stay mouse"
                               :exp-x 100 :exp-y 200)
                              (:desc "Move mouse again"
                               :move-p t
                               :x 111 :y 222
                               :exp-x 111 :exp-y 222))))
            (dolist (tt test-table)
              (destructuring-bind (&key desc move-p x y exp-x exp-y) tt
                (testing desc
                  (when move-p
                    (receive-message-from-client
                     ws-server client-id
                     (make-input-message :mouse-move `(:x ,x :y ,y))))
                  (update-input)
                  (ok (exp-mouse-pos-p client-id exp-x exp-y)))))))))
    (testing "mouse wheel"
      (with-mock-ws-server (ws-server)
        (let ((client-id 0))
          (add-mock-client ws-server client-id)
          (update-client-list)
          (let ((test-table '((:desc "Turn wheel once"
                               :delta-ys (3)
                               :exp-delta-y 3)
                              (:desc "Stay wheel"
                               :exp-delta-y 0)
                              (:desc "Turn wheel twice"
                               :delta-ys (-4 -5)
                               :exp-delta-y -9))))
            (dolist (tt test-table)
              (destructuring-bind (&key desc delta-ys exp-delta-y) tt
                (testing desc
                  (dolist (delta-y delta-ys)
                    (receive-message-from-client
                     ws-server client-id
                     (make-input-message :mouse-wheel `(:delta-y ,delta-y))))
                  (update-input)
                  (ok (= (get-wheel-delta-y client-id) exp-delta-y)))))))))))

(deftest test-for-touch
  (testing "summary"
    (with-mock-ws-server (ws-server)
      (let ((client-id 0))
        (add-mock-client ws-server client-id)
        (update-client-list)
        (let ((test-table '((:desc "Start first touch"
                             :raw-touch-id 0
                             :action :start
                             :x 100 :y 200
                             ;; exp-state := (down-now-p down-p up-now-p up-p)
                             :exp-state (t t nil nil)
                             :exp-x 100 :exp-y 200)
                            (:desc "Stay first touch"
                             :exp-state (nil t nil nil)
                             :exp-x 100 :exp-y 200)
                            (:desc "Move first touch"
                             :raw-touch-id 0
                             :action :move
                             :x 200 :y 400
                             :exp-state (nil t nil nil)
                             :exp-x 200 :exp-y 400)
                            (:desc "Start second touch"
                             :raw-touch-id 1
                             :action :start
                             :x 400 :y 800
                             :exp-state (nil t nil nil)
                             ;; average of two touches
                             :exp-x 300 :exp-y 600)
                            (:desc "End first touch"
                             :raw-touch-id 0
                             :action :end
                             :x 300 :y 700
                             :exp-state (nil t nil nil)
                             ;; remain first touch info in a frame
                             :exp-x 350 :exp-y 750)
                            (:desc "End first touch"
                             :exp-state (nil t nil nil)
                             ;; average of two touches
                             :exp-x 400 :exp-y 800)
                            (:desc "End second touch"
                             :raw-touch-id 1
                             :action :end
                             :x 500 :y 900
                             :exp-state (nil nil t t)
                             ;; remain first touch info in a frame
                             :exp-x 500 :exp-y 900)
                            (:desc "Stay"
                             :exp-state (nil nil nil t)
                             ;; remain first touch info in a frame
                             :exp-x nil :exp-y nil))))
          (dolist (tt test-table)
            (destructuring-bind (&key desc raw-touch-id action x y
                                      exp-state exp-x exp-y)
                tt
              (testing desc
                (when action
                  (ecase action
                    (:start (receive-message-from-client
                             ws-server client-id
                             (make-touch-message
                              :touch-start `(:id ,raw-touch-id :x ,x :y ,y))))
                    (:move (receive-message-from-client
                            ws-server client-id
                            (make-touch-message
                             :touch-move `(:id ,raw-touch-id :x ,x :y ,y))))
                    (:end (receive-message-from-client
                           ws-server client-id
                           (make-touch-message
                            :touch-end `(:id ,raw-touch-id :x ,x :y ,y))))))
                (update-input)
                (multiple-value-bind (x y) (get-touch-summary-pos client-id)
                  (if exp-x
                      (ok (= x exp-x))
                      (ok (not x)))
                  (if exp-y
                      (ok (= y exp-y))
                      (ok (not y))))
                (destructuring-bind (down-now-p down-p up-now-p up-p) exp-state
                  (flet ((same-bool-p (a b)
                           (or (and a b)
                               (and (not a) (not b)))))
                    (ok (same-bool-p (touch-summary-down-now-p client-id) down-now-p))
                    (ok (same-bool-p (touch-summary-down-p client-id) down-p))
                    (ok (same-bool-p (touch-summary-up-now-p client-id) up-now-p))
                    (ok (same-bool-p (touch-summary-up-p client-id) up-p))))))))))))
