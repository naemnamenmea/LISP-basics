;;; #4
;;; Определите функциональный предикат (КАЖДЫЙ пред список), который истинен в том и только в том случае, когда, являющейся функциональным аргументом предикат пред истинен для всех элементов списка список.


(defun every (p lst)
  (dolist (i lst t) (unless (funcall p i) (return nil))))

  
(print (every 'evenp '(2 4 6)))  
(print (every 'evenp '(2 1 4 6)))
(print (every (lambda (x) (not (= x 0))) '(2 1 4 6)))
(print (every (lambda (x) (not (= x 0))) '(2 1 4 0 6)))