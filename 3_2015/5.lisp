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