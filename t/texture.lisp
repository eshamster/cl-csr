(defpackage cl-csr/t/texture
  (:use :cl
        :rove
        :cl-csr/texture)
  (:import-from :cl-csr/protocol
                :code-to-name)
  (:import-from :cl-csr/ws/mock-server
                :add-mock-client)
  (:import-from :cl-csr/ws/mock-client
                :get-latest-messages)
  (:import-from :cl-csr/t/test-utils
                :with-mock-ws-server
                :with-update-frame
                :expected-kind-seq-p))
(in-package :cl-csr/t/texture)

(defmacro with-one-cycle (&body body)
  `(with-update-frame
     ,@body
     (update-texture)))

(defun set-root-test-path ()
  (let* ((system-root (asdf:system-source-directory (asdf:find-system :cl-csr)))
         (resource-root (merge-pathnames "t/resource/" system-root)))
    (set-image-path resource-root "img/")))

(deftest test-texture-update
  (with-mock-ws-server (ws-server)
    (set-root-test-path)
    (let ((client-table (make-hash-table))
          (test-table `(;; Known issue: If a client is added at the same frame
                        ;; where a texture is loaded, duplicated :load-texture
                        ;; messaseges will be sent to the client.
                        ;; 
                        ;; (:desc "Load a texture"
                        ;;  :new-client-ids (0)
                        ;;  :proc ,(lambda ()
                        ;;           (load-texture :name :sample
                        ;;                         :path "sample.png"))
                        ;;  ((client-id (kind...))...)
                        ;;  :exp-kinds ((0 (:frame-start :load-texture :frame-end))))
                        (:desc "Load a texture and an image"
                         :proc ,(lambda ()
                                  (load-texture :name :sample
                                                :path "sample.png")
                                  (load-image :texture-name :sample
                                              :image-name :sample-image)))
                        (:desc "Send texture and image when a client is added"
                         :new-client-ids (0)
                         :exp-kinds ((0 (:frame-start :load-texture :load-image :frame-end))))
                        (:desc "Send nothing if there is no client and no texture"
                               :exp-kinds ((0 (:frame-start :frame-end))))
                        (:desc "Send a new texture to an existing client"
                         :proc ,(lambda ()
                                  (load-texture :name :sample2
                                                :path "sample2.png"))
                         :exp-kinds ((0 (:frame-start :load-texture :frame-end))))
                        (:desc "Send a new image to an existing client"
                         :proc ,(lambda ()
                                  (load-image :texture-name :sample2
                                              :image-name :sample2-image))
                         :exp-kinds ((0 (:frame-start :load-image :frame-end)))))))
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

(deftest test-texture-others
  (with-mock-ws-server (ws-server)
    (set-root-test-path)
    ;; load a texture as :sample
    (testing "load-texture"
      (dolist (tt '((:sample "sample.png" nil)
                    ("name is not keyword" "sample.png" t)
                    (:sample2 "not-exist.png" t)))
        (destructuring-bind (name path exp-error) tt
          (if exp-error
              (ok (signals (load-texture :name name :path path)))
              (ok (not (signals (load-texture :name name :path path))))))))
    ;; load an image as :sample-image
    (testing "load-image"
      (dolist (tt '((:sample-image :sample nil)
                    ("name is not keyword" :sample t)
                    (:sample-image2 :not-exist t)))
        (destructuring-bind (name tex-name exp-error) tt
          (if exp-error
              (ok (signals (load-image :image-name name
                                       :texture-name tex-name)))
              (ok (not (signals (load-image :image-name name
                                            :texture-name tex-name))))))))
    (testing "get-texture-id"
      (ok (numberp (get-texture-id :sample)))
      (ok (null (get-texture-id :not-loaded))))
    (testing "get-image-id"
      (ok (numberp (get-image-id :sample-image)))
      (ok (null (get-image-id :not-loaded))))
    (testing "get-image-size"
      (multiple-value-bind (w h)
          (get-image-size :sample-image)
        (ok (= w 256))
        (ok (= h 128)))
      (testing "with uv"
        (load-image :image-name :sample-image-uv
                    :texture-name :sample
                    :uv (make-image-uv :width 1/2 :height 1/4))
        (multiple-value-bind (w h)
            (get-image-size :sample-image-uv)
          (ok (= w 128))
          (ok (= h 32)))))))
