;;; #7
;;; Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы, которые не обладают свойством, наличие которого проверяет предикат пред.



(defun del-if-not (pred lst)
    (let ((r nil))
        (dolist (i lst r)
            (when (funcall pred i) (setq r (append r (list i))))
        )
    )
) 
 
(print (del-if-not #'evenp '(1 2 3 4 5 6 7)))
(print (del-if-not #'oddp '(1 2 3 4 5 6 7)))