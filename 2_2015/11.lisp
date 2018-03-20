;;; #11
;;; Определите функционал МНОГОФУН, который использует функции, являющиеся агрументами по следующей схеме: (МНОГОФУН '(f g ... h) x) <=> (LIST (f x) (g x) ... (h x)).



(defun fmap (f-lst x)
  (mapcar #'(lambda (f) (funcall f x)) f-lst))
  
  
(print (fmap '(sin cos abs) -13))
(print (fmap '(car cadr length listp) '(1 2 3 4 56 7)))
(print (fmap '(car cadr length listp) '()))
(print (fmap '(+ - * /) 4))