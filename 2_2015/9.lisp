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