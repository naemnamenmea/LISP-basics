;;; #2
;;; Определите функцию, возвращающую последний элемент списка.

(defun get-last (lst)
	(cond 	
		((null lst) nil)
   		((null (cdr lst)) (car lst))
       	(t (get-last (cdr lst)) )
	)
)

;;; get-last '(1 2 3 4)
;;; get-last '(1 (2 3 4))
;;; get-last '(4)
;;; get-last ()