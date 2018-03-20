;;; #27
;;; Определите функцию, которая, чередуя элементы списков (a b...) и (1 2...), образует новый список (a 1 b 2 ...).


(defun combine-lists (lst1 lst2)
    (cond
        ((null lst1) lst2)
        ((null lst2) lst1)
        (t (cons (car lst1) (cons (car lst2) (combine-lists (cdr lst1) (cdr lst2)))))
    )
)
 

(print (combine-lists '(1 2 3 4) '(a b c d)))
(print (combine-lists '(1 2 3 4 5) '(a b c)))
(print (combine-lists '(1 2 3) '(a b c d e)))
(print (combine-lists '() '()))
(print (combine-lists '() '(a b c d e)))
(print (combine-lists '(1 2 3 4 5) '()))
(print (combine-lists '(1 2 3 4) '(a (b c) d)))