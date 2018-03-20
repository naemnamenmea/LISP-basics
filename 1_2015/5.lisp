;;; #5
;;; Определите функцию, которая увеличивает элементы исходного списка на единицу.

(defun inc-list-r (arg)
    ( cond 
        ( ( null arg ) nil )
        ( t (cons 
            ( cond
                ( ( listp (car arg) ) (inc-list-r (car arg)) )
                ( ( numberp (car arg) ) (+ 1 (car arg)) )
                ( t (car arg) )
            ) 
            (inc-list-r (cdr arg))
            )
        )
    )
)

(print (inc-list-r '(1 2 3 4)))
(print (inc-list-r '(1 (2 3 4))))
(print (inc-list-r '(3)))
(print (inc-list-r ()))
(print (inc-list-r '(1 2 3 b)))
(print (inc-list-r '(1 (a 3 4))))
(print (inc-list-r '(a (1 3 4))))
(print (inc-list-r '(a (j 3 4))))