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