;;; #35
;;; Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно множество подмножеством другого. Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.

(defun remove-it-once (it lst)
    (cond
        ((null it) lst)
        ((null lst) 0)
        ((eq it (car lst)) (cdr lst))
        (t ((lambda (prev-lst) (if (eq prev-lst 0) 0 (cons (car lst) prev-lst)) ) (remove-it-once it (cdr lst))))
    )
)

(defun is-subset (lst subset)
    (cond
        ((null subset) t)
        ((null lst) nil)
        (t ((lambda (new-lst) (if (eq new-lst 0) nil (is-subset new-lst (cdr subset)))) (remove-it-once (car subset) lst)))
    )
)


(print (is-subset '(a b c d e) '(c b e))) ;T
(print (is-subset '(a d e) '(e))) ;T
(print (is-subset () '(a))) ;nil
(print (is-subset () ())) ;T
(print (is-subset '(a d e) ())) ;T
(print (is-subset '(a b c d e) '(c b g e))) ;nil
(print (is-subset '(a b c d e) '(d a a c))) ;nil