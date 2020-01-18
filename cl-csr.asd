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
               "ps-experiment/t/test-utils"
               "cl-csr/t/utils/buffered-queue"
               "cl-csr/t/utils/input")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))