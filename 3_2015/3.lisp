;;; #3
;;; Определите лисповскую форму (IF условие p q) в виде макроса.



(defmacro -IF (условие р q)
    `(if ,условие ,р ,q))
    
    
    
(setq x '(a b c))
(print (-IF (atom x) 'yes 'no))
(print (-IF (listp x) 'yes 'no))