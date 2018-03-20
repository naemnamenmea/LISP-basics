;;; #13
;;; Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).



(defun self-def (&rest args)
    (symbol-function 'self-def))
    

 
(print (self-def))
(print (self-def 1 2 3))
(print (self-def 'd))