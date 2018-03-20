;;; #2
;;; Определите функционал (MAPLIST fn список) для одного списочного аргумента.


(defun -maplist (f lst)
    (cond 
        ((null lst) nil)
        (t (cons (funcall f lst) (-maplist f (cdr lst))))
    )
)
 
(print (-maplist (lambda (x) (apply '* x)) '(1 2 3 4 5 6)))
(print (maplist (lambda (x) (apply '* x)) '(1 2 3 4 5 6)))
  
(print (-maplist (lambda (list) (cons 0 list)) '(1 2 3 4)))
(print (maplist (lambda (list) (cons 0 list)) '(1 2 3 4)))