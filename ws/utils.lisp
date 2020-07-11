(defpackage cl-csr/ws/utils
  (:use :cl)
  (:export :same-target-client-list-p
           :copy-target-client-id-list
           :calc-common-target)
  (:import-from :cl-csr/ws/ws
                :*target-client-id-list*))
(in-package :cl-csr/ws/utils)

(defun same-target-client-list-p (lst1 lst2)
  (or (and (eq lst1 :all)
           (eq lst2 :all))
      (and (listp lst1)
           (listp lst2)
           (equalp (sort (copy-list lst1) #'<)
                   (sort (copy-list lst2) #'<)))))

(defun copy-target-client-id-list (&optional (lst *target-client-id-list*))
  (if (eq lst :all)
      :all
      (copy-list lst)))

(defun calc-common-target (id-list1 id-list2)
  (cond ((eq id-list1 :all) id-list2)
        ((eq id-list2 :all) id-list1)
        (t (remove-if (lambda (id)
                        (not (find id id-list1)))
                      id-list2))))

