;;; #1
;;; Определите макрос, который возвращает свой вызов.



(defmacro self (&whole f &rest x) `(quote ,f))



(print (self))
(print (self nil))
(print (self '(1 2 34)))