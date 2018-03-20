;;; #11
;;; Определите функцию, осуществляющую разделение исходного списка на два подсписка. В первый из них должно попасть указанное количество элементов с начала списка, во второй — оставшиеся элементы.

(defun split-list (lst n)
	(cond
        ((< n 0) nil)
        ((null lst) nil)
        ((= n 0) (list () lst))
        (t
			((lambda (resault)
				(list
	                (cons (car lst) (car resault))
	                (cadr resault)))
			(split-list (cdr lst) (- n 1)))
        )
    )
)

(print (split-list '(1 2 3 4 5) 3))
(print (split-list '(1 2 3 4 5) 10))
(print (split-list '(1 2 3 4 5) -7))
(print (split-list '(-7) 0))
(print (split-list () 0))