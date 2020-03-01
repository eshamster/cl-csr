#|
  This file is a part of cl-csr project.
  Copyright (c) 2019 eshamster (hamgoostar@gmail.com)
|#

#|
  A client side rendering library in Common Lisp

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem "cl-csr"
  :version "0.1.0"
  :author "eshamster"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :license "MIT"
  :depends-on (:websocket-driver-server
               :websocket-driver-client
               :alexandria
               :bordeaux-threads
               :lack-middleware-static
               :clack
               :dexador
               :cl-markup
               :cl-ppcre
               :jonathan
               :opticl
               :parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-csr/main)
  :description "A client side rendering library in Common Lisp"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cl-csr/t"))))

(defsystem cl-csr/t
  :class :package-inferred-system
  :depends-on (:cl-csr
               :rove
               "cl-csr/t/mock/ws-client-mock"
               "cl-csr/t/mock/ws-server-mock"
               "cl-csr/t/utils/buffered-queue"
               "cl-csr/t/utils/hash-table"
               "cl-csr/t/utils/input"
               "cl-csr/t/utils/list"
               "cl-csr/t/camera"
               "cl-csr/t/client-list-manager"
               "cl-csr/t/frame-counter"
               "cl-csr/t/graphics"
               "cl-csr/t/input"
               "cl-csr/t/protocol"
               "cl-csr/t/screen-size"
               "cl-csr/t/ws-server"
               "cl-csr/t/test-utils")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
