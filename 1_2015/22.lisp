;;; #22
;;; Определите функцию, которая обращает список (а b с) и разбивает его на уровни (((с) b) а).

(defun rev-lvls (lst)
    (if (null (cdr lst))
        lst
        (list (rev-lvls (cdr lst)) (car lst))
    )
)
 

(print (rev-lvls '(a b c d)))
(print (rev-lvls '(a b c d e)))
(print (rev-lvls '(a)))
(print (rev-lvls ()))