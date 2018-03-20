;;; #1
;;; Определите FUNCALL через функционал APPLY.


(defun -funcall (f &rest args) (apply f args))

(print (funcall #'+ 1 2 3))
(print (funcall #'+ ))
(print (-funcall #'+ 1 2 3))
(print (-funcall #'+ ))