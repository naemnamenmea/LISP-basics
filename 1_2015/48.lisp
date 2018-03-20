;;; #48
;;; Функция GET возвращает в качестве результата NIL в том случае, если у символа нет данного свойства, либо если значением этого свойства является NIL. Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством.


(defun has-prop (x prop)
    (find-property prop (symbol-plist x))
)


(defun find-property (prop list)
    (cond
        ((null list) nil)
        ((equal prop (car list)) T)
        (T (find-property prop (cddr list)))
    )
)


(setf (get 'Symbol 'prop1) 100)
(setf (get 'Symbol 'prop2) nil)
(setf (get 'Symbol 'prop3) T)
(setf (get 'Symbol 'prop4) 'name)
(setf (get 'Symbol 'prop5) "any-string")
(setf (get 'Symbol 'prop6) "")



(print (list (get 'Symbol 'prop1) (has-prop 'Symbol 'prop1)))
(print (list (get 'Symbol 'prop2) (has-prop 'Symbol 'prop2)))
(print (list (get 'Symbol 'prop3) (has-prop 'Symbol 'prop3)))
(print (list (get 'Symbol 'prop4) (has-prop 'Symbol 'prop4)))
(print (list (get 'Symbol 'prop5) (has-prop 'Symbol 'prop5)))
(print (list (get 'Symbol 'prop6) (has-prop 'Symbol 'prop6)))
(print (list (get 'Symbol 'prop7) (has-prop 'Symbol 'prop7)))