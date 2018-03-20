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