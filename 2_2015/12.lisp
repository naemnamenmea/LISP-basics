;;; #12
;;; Определите функцию, которая возвращает в качестве значения свой вызов.


(defun self (&rest args)
    (list 'apply 'self args))
    
 
 
(print (self 1 2 3))
(print (self 'd))