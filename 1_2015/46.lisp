;;; #46
;;; Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо. Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры или братья, родные или с одним общим родителем.

(defun set-parents (x m f)
    (setf (get x 'father) f)
    (setf (get x 'mother) m)
)

(defun get-parents (x)
    (cons (get x 'father) (list (get x 'mother)))
)

(defun is-siblings (x y)
    ((lambda (p-x p-y) (if (eq (car p-x) (car p-y)) t (if (eq (cadr p-x) (cadr p-y)) t nil))) (get-parents x) (get-parents y))
)

(set-parents 'Peter 'Alice 'Dread)
(set-parents 'Dennis 'Pall 'Jane)
(set-parents 'Colin 'Merry 'Dread)
(set-parents 'Genry 'Alice 'Dread)
(set-parents 'Susie 'Alice 'Mat)


(print (get-parents 'Dan ))
(print (get-parents 'Peter ))

(print (is-siblings 'Peter 'Dennis))
(print (is-siblings 'Peter 'Genry))
(print (is-siblings 'Genry 'Susie))

#|
(set 'a '(+ 2 3))
(defun a (x y) (+ x y))

(get 'a 'mood)
(setf (get 'a 'mood) 'bad)

;(print (get 'a 'mood))
;(print (symbolp 'a))
;(print (symbol-value 'a))
;(print (symbol-function 'a))
;(print (symbol-plist 'a))

(print (concatenate 'string "Dans parents are" (get-parents 'Dan )))
|#