;;; #9
;;; Определите функцию, разделяющую исходный список на два подсписка. В первый из них должны попасть элементы с нечетными номерами, во второй — элементы с четными номерами.

(defun cr-even-odd-lst (lst)
	(cond 
		((null lst) nil)
        ((null (cdr lst)) (list lst))
		(t
			((lambda (even-odd-lst)
				(list
	                (cons (car lst) (car even-odd-lst))
	                (cons (cadr lst) (cadr even-odd-lst))))
			(cr-even-odd-lst (cddr lst)))
        )
     )
)

(print (cr-even-odd-lst '(1 2 3 4 5)))
(print (cr-even-odd-lst '(1 2 3 4 5 6)))
(print (cr-even-odd-lst '(8)))
(print (cr-even-odd-lst ()))
(print (cr-even-odd-lst '((1 2) (3 4) (5 6))))
(print (cr-even-odd-lst '((1 2) (3 4) (5 6) (7))))