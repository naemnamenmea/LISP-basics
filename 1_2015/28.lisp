;;; #28
;;; Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).


(defun count-atoms-r(lst)
    (cond
        ((null lst) 0)
        ((atom lst) 1)
        (t (+ (count-atoms-r (car lst)) (count-atoms-r (cdr lst))))
    )
)

(defun count-atoms(lst)
    (cond
        ((null lst) 0)
        (t (+ (cond
                ((atom (car lst)) 1)
                (t 0))
              (count-atoms (cdr lst))))
    )
)
 

(print (count-atoms '(1 (a (a s)) c d)))
(print (count-atoms '()))
(print (count-atoms '(a b ((a) c) e)))
(print (count-atoms-r '(1 (a (a s)) c d)))
(print (count-atoms-r '()))
(print (count-atoms-r '(a b ((a) c) e)))