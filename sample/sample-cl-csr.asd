#|
  This file is a part of cl-csr project.
  Copyright (c) 2019 eshamster (hamgoostar@gmail.com)
|#

#|
  A sample of cl-csr in Common Lisp

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem "sample-cl-csr"
  :version "0.1.0"
  :author "eshamster"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :license "MIT"
  :depends-on (:cl-markup
               :ningle
               :cl-csr
               :sample-cl-csr/main)
  :description "A sample of cl-csr in Common Lisp")
