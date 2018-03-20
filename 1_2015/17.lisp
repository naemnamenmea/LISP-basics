;;; #17
;;; Создайте предикат, порождающий всевозможные перестановки исходного множества.


;; Вставить a во все позиции r:
 
(defun ins-in-all-pos (a l r)
    (cond 
        ((null r) (list (append l (list a))))
        (t (cons (append l (list a) r) (ins-in-all-pos a (append l (list (car r))) (cdr r))))
    )
)
 
;; Перестановки
 
(defun all-perms (lst)
    (cond 
        ((null (cdr lst)) (list lst))
        (t (apply 'append (mapcar (lambda (x) (ins-in-all-pos (car lst) nil x)) (all-perms (cdr lst)))))
    )
)


 

(defun permutate (w)
    (cond 
        ((null w) nil)
        ((null (cdr w)) (list w))
        ((loop for a in w
               nconc (mapcar #'(lambda (e) (cons a e))
                             (permutate (remove a w)))))
    )
)

(defun permutate (w)
    (when w (if (cdr w)
              (loop for a in w
                    nconc (mapcar #'(lambda (e) (cons a e))
                                  (permutate (remove a w))))
              (list w)
            )
    )
)

 

;(print (all-perms '()))
;(print (all-perms '(a b c)))
;(print (all-perms '(a b c d)))
(print (permutate '(a b c d)))