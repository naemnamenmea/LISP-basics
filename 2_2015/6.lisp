;;; #6
;;; Определите фильтр (УДАЛИТЬ-ЕСЛИ пред список), удаляющий из списка список все элементы, которые обладают свойством, наличие которого проверяет предикат пред.



(defun del-if (pred lst)
    (let ((r nil))
        (dolist (i lst r)
            (when (Not (funcall pred i)) (setq r (append r (list i))))
        )
    )
) 
 
(print (del-if #'evenp '(1 2 3 4 5 6 7)))
(print (del-if #'oddp '(1 2 3 4 5 6 7)))