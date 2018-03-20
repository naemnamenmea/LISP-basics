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