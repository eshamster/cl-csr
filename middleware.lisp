(defpackage cl-csr/middleware
  (:use :cl)
  (:export :make-client-side-rendering-middleware)
  (:import-from :cl-csr/ws-server
                :make-ws-app)
  (:import-from :cl-csr/client/core
                :output-client-js)
  (:import-from :cl-csr/texture
                :set-image-path)
  (:import-from :cl-csr/utils/dependency
                :ensure-js-files))
(in-package :cl-csr/middleware)

(defun make-client-side-rendering-middleware (&key
                                                resource-root
                                                (image-relarive-path "img/"))
  (ensure-js-files  (merge-pathnames "js/" resource-root))
  (set-image-path resource-root image-relarive-path)
  (lambda (app)
    (lambda (env)
      (output-client-js (merge-pathnames "js/client.js" resource-root))
      (let ((uri (getf env :request-uri)))
        (if (string= uri "/ws")
            (funcall (make-ws-app) env)
            (funcall (make-static-middleware
                      app
                      :resource-root resource-root
                      :image-relarive-path image-relarive-path)
                     env))))))

(defun make-static-middleware (app &key
                                     resource-root
                                     image-relarive-path)
  (funcall lack.middleware.static:*lack-middleware-static*
           app
           :path (lambda (path)
                   (if (ppcre:scan (format nil "^(?:/js/|/json/|/~A)" image-relarive-path)
                                   path)
                        path
                        nil))
           :root resource-root))
