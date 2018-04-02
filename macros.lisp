;;; #1
;;; Определите макрос, который возвращает свой вызов.



(defmacro self (&whole f &rest x) `(quote ,f))



(print (self))
(print (self nil))
(print (self '(1 2 34)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #2
;;; Определите макрос (POP стек), который читает из стека верхний элемент и меняет
;;; значение переменной стека.


(defparameter *stack* '(1 2 3 4))

(defmacro -pop (*stack*)
    `(prog1 (car ,*stack*)
        (setq ,*stack* (cdr ,*stack*))))
        
        
(print (-pop *stack*))
(print (-pop *stack*))
(print *stack*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #3
;;; Определите лисповскую форму (IF условие p q) в виде макроса.



(defmacro -IF (условие р q)
    `(if ,условие ,р ,q))
    
    
    
(setq x '(a b c))
(print (-IF (atom x) 'yes 'no))
(print (-IF (listp x) 'yes 'no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #4
;;; Определите в виде макроса форму (FIF тест отр нуль полож).



(defmacro fif (test neg zero pos)
  (let ((testsym (gensym)))
    `(let ((,testsym ,test))
       (cond ((zerop ,testsym) ,zero)
             ((minusp ,testsym) ,neg)
             (t ,pos)))))
             
             
             
(setq x 1 y 2)
 
(print (FIF (- x y) 'меньше 'равно 'больше))
(print (FIF (- y x) 'меньше 'равно 'больше))
(print (FIF (- x x) 'меньше 'равно 'больше))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ;;; #5
;;; Определите в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.



(defmacro repeat ((&body body) condition)
  (let ((n (gensym)) (name (gensym)))
    `(labels ((,name (,n)
                (if ,condition
                    ,n
                    (,name (progn ,@body)))))
       (,name ()))))
 
(let ((i 0))
       (repeat ((format t "~A " i) 
                (incf i))
         (> i 1)))
;0 1 
;2
(let ((i 0))
       (repeat ((incf i)
                (format t "~A " i))
         (> i 1)))
;1 2 
;NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #6
;;; Разработать "собственный" язык программирования.



