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
 