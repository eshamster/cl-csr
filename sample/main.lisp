(defpackage sample-cl-csr/main
  (:nicknames :sample-cl-csr)
  (:use :cl
        :sample-cl-csr/server
        :sample-cl-csr/admin/admin)
  (:export :start
           :stop))
