﻿;;; #2
;;; Определите функцию, возвращающую последний элемент списка.

(defun get-last (lst)
	(cond 	
		((null lst) nil)
   		((null (cdr lst)) (car lst))
       	(t (get-last (cdr lst)) )
	)
)

(print get-last '(1 2 3 4))
(print get-last '(1 (2 3 4)))
(print get-last '(4))
(print get-last ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #5
;;; Определите функцию, которая увеличивает элементы исходного списка на единицу.

(defun inc-list-r (arg)
    ( cond 
        ( ( null arg ) nil )
        ( t (cons 
            ( cond
                ( ( listp (car arg) ) (inc-list-r (car arg)) )
                ( ( numberp (car arg) ) (+ 1 (car arg)) )
                ( t (car arg) )
            ) 
            (inc-list-r (cdr arg))
            )
        )
    )
)

(print (inc-list-r '(1 2 3 4)))
(print (inc-list-r '(1 (2 3 4))))
(print (inc-list-r '(3)))
(print (inc-list-r ()))
(print (inc-list-r '(1 2 3 b)))
(print (inc-list-r '(1 (a 3 4))))
(print (inc-list-r '(a (1 3 4))))
(print (inc-list-r '(a (j 3 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #7
;;; Определите функцию, удаляющую из исходного списка элементы с четными номерами.

(defun del-even (lst)
    ( cond
        ((null lst) nil)
        ((null (cdr lst)) lst)
        (t (cons (car lst) (del-even (cddr lst))))
    )
)

(print (del-even '(1 2 3 4 5 6 7 8)))
(print (del-even '(1 2 3 4 5 6 7 8 9)))
(print (del-even '(1 (2 3 4))))
(print (del-even '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))))
(print (del-even '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18))))
(print (del-even '(3)))
(print (del-even '(1 2)))
(print (del-even ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #9
;;; Определите функцию, разделяющую исходный список на два подсписка.
;;; В первый из них должны попасть элементы с нечетными номерами,
;;; во второй — элементы с четными номерами.

(defun cr-even-odd-lst (lst)
	(cond 
		((null lst) nil)
        ((null (cdr lst)) (list lst))
		(t
			((lambda (prev-resault)
				(list
	                (cons (car lst) (car prev-resault))
	                (cons (cadr lst) (cadr prev-resault))))
			(cr-even-odd-lst (cddr lst)))
        )
     )
)

(print (cr-even-odd-lst '(1 2 3 4 5)))
(print (cr-even-odd-lst '(1 2 3 4 5 6)))
(print (cr-even-odd-lst '(8)))
(print (cr-even-odd-lst ()))
(print (cr-even-odd-lst '((1 2) (3 4) (5 6))))
(print (cr-even-odd-lst '((1 2) (3 4) (5 6) (7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #11
;;; Определите функцию, осуществляющую разделение исходного списка на два подсписка.
;;; В первый из них должно попасть указанное количество элементов с начала списка,
;;; во второй — оставшиеся элементы.

(defun split-list (lst n)
	(cond
        ((< n 0) nil)
        ((null lst) nil)
        ((= n 0) (list () lst))
        (t
			((lambda (resault)
				(list
	                (cons (car lst) (car resault))
	                (cadr resault)))
			(split-list (cdr lst) (- n 1)))
        )
    )
)

(print (split-list '(1 2 3 4 5) 3))
(print (split-list '(1 2 3 4 5) 10))
(print (split-list '(1 2 3 4 5) -7))
(print (split-list '(-7) 0))
(print (split-list () 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #17
;;; Создайте предикат, порождающий всевозможные перестановки исходного множества.


;; Вставить a во все позиции r:
 
(defun ins-in-all-pos (a l r)
    (cond 
        ((null r) (list (append l (list a))))
        (t (cons (append l (list a) r) (ins-in-all-pos a (append l (list (car r))) (cdr r))))
    )
)
 
;; Перестановки
 
(defun all-perms (lst)
    (cond 
        ((null (cdr lst)) (list lst))
        (t (apply 'append (mapcar (lambda (x) (ins-in-all-pos (car lst) nil x)) (all-perms (cdr lst)))))
    )
)


 

(defun permutate (w)
    (cond 
        ((null w) nil)
        ((null (cdr w)) (list w))
        (t (loop for a in w
               nconc (mapcar #'(lambda (e) (cons a e))
                             (permutate (remove a w)))))
    )
)

 

;(print (all-perms '()))
;(print (all-perms '(a b c)))
;(print (all-perms '(a b c d)))
(print (permutate '(a b c d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #22
;;; Определите функцию, которая обращает список (а b с) и разбивает его на уровни (((с) b) а).

(defun rev-lvls (lst)
    (if (null (cdr lst))
        lst
        (list (rev-lvls (cdr lst)) (car lst))
    )
)
 

(print (rev-lvls '(a b c d)))
(print (rev-lvls '(a b c d e)))
(print (rev-lvls '(a)))
(print (rev-lvls ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #27
;;; Определите функцию, которая, чередуя элементы списков (a b...) и (1 2...),
;;; образует новый список (a 1 b 2 ...).


(defun combine-lists (lst1 lst2)
    (cond
        ((null lst1) lst2)
        ((null lst2) lst1)
        (t (cons (car lst1) (cons (car lst2) (combine-lists (cdr lst1) (cdr lst2)))))
    )
)
 

(print (combine-lists '(1 2 3 4) '(a b c d)))
(print (combine-lists '(1 2 3 4 5) '(a b c)))
(print (combine-lists '(1 2 3) '(a b c d e)))
(print (combine-lists '() '()))
(print (combine-lists '() '(a b c d e)))
(print (combine-lists '(1 2 3 4 5) '()))
(print (combine-lists '(1 2 3 4) '(a (b c) d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #28
;;; Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).


(defun count-atoms-r(lst)
    (cond
        ((null lst) 0)
        ((atom lst) 1)
        (t (+ (count-atoms-r (car lst)) (count-atoms-r (cdr lst))))
    )
)

(defun count-atoms(lst)
    (cond
        ((null lst) 0)
        (t (+ (cond
                ((atom (car lst)) 1)
                (t 0))
              (count-atoms (cdr lst))))
    )
)
 

(print (count-atoms '(1 (a (a s)) c d)))
(print (count-atoms '()))
(print (count-atoms '(a b ((a) c) e)))
(print (count-atoms-r '(1 (a (a s)) c d)))
(print (count-atoms-r '()))
(print (count-atoms-r '(a b ((a) c) e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #35
;;; Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно множество
;;; подмножеством другого. Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.

(defun -member (&key el lst)
    (cond
        ((null lst) nil)
        ((eq (car lst) el) t)
        (t (-member :el el :lst (cdr lst)))
        )
    )

(defun subsetp (&key set subset)
    (cond
        ((null subset) t)
        ((null set) nil)
        (t (and (-member :el (car subset) :lst set) (subsetp :set set :subset (cdr subset))))
    )
)

(print (subsetp :set '(a b c d e) :subset '(c b e))) ;T
(print (subsetp :set '(a d e) :subset '(e))) ;T
(print (subsetp :set () :subset '(a))) ;nil
(print (subsetp :set () :subset ())) ;T
(print (subsetp :set '(a d e) :subset ())) ;T
(print (subsetp :set '(a b c d e) :subset '(c b g e))) ;nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #46
;;; Предположим, что отец и мать некоторого лица, хранятся как значения
;;; соответствующих свойств у символа, обозначающего это лицо. Напишите функцию
;;; (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и предикат
;;; (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры или братья,
;;; родные или с одним общим родителем.

(defun set-parents (&key child mother father)
    (setf (get child 'father) father)
    (setf (get child 'mother) mother)
)

(defun get-parents (x)
    (cons (get x 'father) (list (get x 'mother)))
)

(defun is-siblings (x y)
    ((lambda (p-x p-y) (or (eq (car p-x) (car p-y)) (eq (cadr p-x) (cadr p-y))))
        (get-parents x) (get-parents y))
)

(set-parents :child 'Peter :mother 'Alice :father 'Dread)
(set-parents :child 'Dennis :mother 'Jane :father 'Pall)
(set-parents :child 'Colin :mother 'Merry :father 'Dread)
(set-parents :child 'Genry :mother 'Alice :father 'Dread)
(set-parents :child 'Susie :mother 'Alice :father 'Mat)


(print (get-parents 'Dan ))
(print (get-parents 'Peter ))

(print (is-siblings 'Peter 'Dennis))
(print (is-siblings 'Peter 'Genry))
(print (is-siblings 'Genry 'Susie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #48
;;; Функция GET возвращает в качестве результата NIL в том случае, если у символа
;;; нет данного свойства, либо если значением этого свойства является NIL.
;;; Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в
;;; списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который
;;; проверяет, обладает ли символ данным свойством.


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
