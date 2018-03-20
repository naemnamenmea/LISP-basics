;;; #2
;;; Определите макрос (POP стек), который читает из стека верхний элемент и меняет значение переменной стека.


(defparameter *stack* '(1 2 3 4))

(defmacro -pop (*stack*)
    `(prog1 (car ,*stack*)
        (setq ,*stack* (cdr ,*stack*))))
        
        
(print (-pop *stack*))
(print (-pop *stack*))
(print *stack*)