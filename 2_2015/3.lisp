;;; #3
;;; Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка (f1 f2 ... fn) к соответствующему элементу списка x = (x1 x2 ... xn) и возвращает список, сформированный из результатов.


(defun apl-apply (f x)
  (cond 
    ((null f) nil)
    ((null x) nil)
    (t (cons (funcall (car f) (car x)) (apl-apply (cdr f) (cdr x))))
   )
)

(setq fi '(cdr car last))
(setq xi '((a b c) (a b c) (a b c)))
(print (apl-apply fi xi))

(setq fi '(reverse cadr length))
(setq xi '((1 2 3) (1 2 3) (1 2 3)))
(print (apl-apply fi xi))