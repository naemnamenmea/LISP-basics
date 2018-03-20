;;; #7
;;; Определите функцию, удаляющую из исходного списка элементы с четными номерами.

(defun del-even (lst)
    ( cond
        ((null lst) nil)
        ((null (cdr lst)) lst)
        (t (cons (car lst) (del-even (cddr lst))))
    )
)

(print (del-even '(1 2 3 4 5 6 7 8)))
(print (del-even '(1 2 3 4 5 6 7 8 9)))
(print (del-even '(1 (2 3 4))))
(print (del-even '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))))
(print (del-even '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18))))
(print (del-even '(3)))
(print (del-even '(1 2)))
(print (del-even ()))