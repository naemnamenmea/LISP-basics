;;; #1
;;; Определите FUNCALL через функционал APPLY.


(defun -funcall (f &rest args) (apply f args))

(print (funcall #'+ 1 2 3))
(print (funcall #'+ ))
(print (-funcall #'+ 1 2 3))
(print (-funcall #'+ ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #3
;;; Определите функционал (APL-APPLY f x), который применяет каждую функцию fi
;;; списка (f1 f2 ... fn) к соответствующему элементу списка x = (x1 x2 ... xn)
;;; и возвращает список, сформированный из результатов.


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #4
;;; Определите функциональный предикат (КАЖДЫЙ пред список), который истинен в том
;;; и только в том случае, когда, являющейся функциональным аргументом предикат пред
;;; истинен для всех элементов списка список.


(defun -every (p lst)
   (null (mapcan #'(lambda (x) (if (funcall p x) nil (list t)) ) lst)))   
   
  
(print (-every 'evenp '(2 4 6)))  
(print (-every 'evenp '(2 1 4 6)))
(print (-every (lambda (x) (not (= x 0))) '(2 1 4 6)))
(print (-every (lambda (x) (not (= x 0))) '(2 1 4 0 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #5
;;; Определите функциональный предикат (НЕКОТОРЫЙ пред список), который истенен,
;;; когда, являющейся функциональным аргаментом предикат пред истинен хотя бы
;;; для одного элемента списка список.



(defun -try (p lst)
   (not (null (mapcan #'(lambda (x) (if (funcall p x) (list t) nil) ) lst))))

  
(print (-try 'evenp '(1 2 3 5 7)))  
(print (-try 'evenp '(1 3 5 7)))
(print (-try (lambda (x) (not (= x 0))) '(0 0 0 0 1 0)))
(print (-try (lambda (x) (not (= x 0))) '(0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #6
;;; Определите фильтр (УДАЛИТЬ-ЕСЛИ пред список), удаляющий из списка список все элементы,
;;; которые обладают свойством, наличие которого проверяет предикат пред.


(defun del-if (pred lst)
	(mapcan #'(lambda (x) (if (funcall pred x) nil (list x))) lst)
) 
 
(print (del-if #'evenp '(1 2 3 4 5 6 7)))
(print (del-if #'oddp '(1 2 3 4 5 6 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #7
;;; Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы,
;;; которые не обладают свойством, наличие которого проверяет предикат пред.



(defun del-if-not (pred lst)
	(mapcan #'(lambda (x) (if (funcall pred x) (list x) nil)) lst)
) 
 
(print (del-if-not #'evenp '(1 2 3 4 5 6 7)))
(print (del-if-not #'oddp '(1 2 3 4 5 6 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #8
;;; Напишите генератор натуральных чисел: 0, 1, 2, 3, 4, 5, ...



(defun initialize nil (defparameter *n* 0))

(defun gen-nat-num nil
    (prog1 *n* (incf *n*)))

(defun print-n-nat-num (n) 
    (loop repeat n do
        (print (gen-nat-num))))

(initialize)
;(print (gen-nat-num))
(print-n-nat-num 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #9
;;; Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...



(defun initialize nil
    (defparameter *f1* 0)
    (defparameter *f2* nil))

(defun gen-fibo nil
    (if (eq *f2* nil) (progn (setq *f2* 1) *f1*)
    (prog2 (setq r (+ *f1* *f2*))
    (setq *f1* *f2*)
    (setq *f2* r)
    ))
)

(defun print-n-fibo (n) 
    (loop repeat n do
        (print (gen-fibo))))

(initialize)
;(print (gen-fibo))
(print-n-fibo 1)
(print-n-fibo 17)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #10
;;; Напишите генератор, порождающий последовательность (A), (B A), (A B A), (B A B A), ...



(defun initialize nil
    (defparameter *s* '(A)))

(defun gen-list nil
    (prog1
        *s*
        (setq *s* (if (eq (car *s*) 'A) (cons 'B *s*) (cons 'A *s*)))))

(defun print-n-gen (n) 
    (loop repeat n do
        (print (gen-list))))

(initialize)
;(print (gen-list))
(print-n-gen 17)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #11
;;; Определите функционал МНОГОФУН, который использует функции, являющиеся агрументами
;;; по следующей схеме: (МНОГОФУН '(f g ... h) x) <=> (LIST (f x) (g x) ... (h x)).



(defun fmap (f-lst x)
  (mapcar #'(lambda (f) (funcall f x)) f-lst))
  
  
(print (fmap '(sin cos abs) -13))
(print (fmap '(car cadr length listp) '(1 2 3 4 56 7)))
(print (fmap '(car cadr length listp) '()))
(print (fmap '(+ - * /) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #12
;;; Определите функцию, которая возвращает в качестве значения свой вызов.


(defun self (&rest args)
    (list 'apply 'self args))
    
 
 
(print (self 1 2 3))
(print (self 'd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #13
;;; Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).



(defun self-def (&rest args)
    (symbol-function 'self-def))
    

 
(print (self-def))
(print (self-def 1 2 3))
(print (self-def 'd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #14
;;; Определите функцию, которая возвращает в качестве значения форму своего определения (DEFUN).



(defun get-form nil
    ((lambda (x)
        (list 'defun 'get-form nil
            (list x (list 'quote x))))
    '(lambda (x)
        (list 'defun 'get-form nil
            (list x (list 'quote x))))))
    

 
(print (get-form))
