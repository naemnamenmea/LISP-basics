;;; #1
;;; Пусть матрицы задаются в виде списка списков. Реализовать функции
;;; сложения и умножения матриц, вычисления определителя матрицы.

(defun print-matrix (matrix)
    (format t "~%~%/-----------~%~{~{~3,d~^ ~}~%~%~}\\--------------~%" matrix))

(defun matrix-sum2 (m1 m2)
    (cond
    ((or (null m1) (null m2)) nil)   
    (t ((lambda (v) (if (and (numberp (car m1)) (numberp (car m2)))
            (cons (+ (car m1) (car m2)) v) 
            (cons (matrix-sum (car m1) (car m2)) v)))
        (matrix-sum (cdr m1) (cdr m2))))))

(defun matrix-sum (m1 m2)
    (mapcar #'(lambda (row1 row2) (mapcar '+ row1 row2)) m1 m2))    
    
(print-matrix (matrix-sum '(()) '(()))) ;nil
(print-matrix (matrix-sum '((3)) '((-7))))
(print-matrix (matrix-sum '((1 -1 3)) '((2 1 4))))
(print-matrix (matrix-sum '((-3 6 3) (4 -3 6)) '((-3 6 4) (3 -1 3))))

(defun cols2 (m)
    (let ((m-cols (make-list (length m))))
         (loop for row in m do (setf m-cols (mapcar 'cons row m-cols)))
         (mapcar 'reverse m-cols)))        

(defun cols (m)
    (when (not (null (mapcan #'(lambda (row) (when (not (null (car row))) (list t))) m)))
        (cons (mapcar 'car m) (cols (mapcar 'cdr m)))))


(defun matrix-mul (m1 m2)
    (when (eq (length (car m1)) (length m2))
        ((lambda (-m1) (mapcar #'(lambda (row) (mapcar #'(lambda (col) (apply '+ (mapcar '* row col))) -m1)) m1)) (cols m2))))

(print-matrix (matrix-mul '(()) '(()))) ;nil
(print-matrix (matrix-mul '((3)) '((-7))))
(print-matrix (matrix-mul '((1 -1 3)) '((2 1 4) (1 4 3) (-1 5 0))))
(print-matrix (matrix-mul '((-3 6) (4 -3)) '((-3 6 4) (3 -1 3))))
(print-matrix (matrix-mul '((3)) '((-7) (4)))) ;nil

(defun minor (m &optional (i 1) (j 1))
    ((lambda (-m l) (mapcar #'(lambda (row) (append (subseq row 0 (- j 1)) (subseq row j l))) -m))
     (append (subseq m 0 (- i 1)) (subseq m i (length m))) (apply 'max (mapcar 'length m))))

(defun matrix-det (m)
    (if (and (null (cdr m)) (null (cdar m)))
        (caar m)
        (let ((det 0) (l (apply 'max (mapcar 'length m))) (row (car m)))
                (loop for i from 1 to l do
                 (setf det (+ det (* (nth (- i 1) row) (expt (- 1) (+ 1 i)) (matrix-det (minor m 1 i))))))
                det)))


(print (matrix-det '(()))) ;nil
(print (matrix-det '((3))))
(print (matrix-det '((-1 3) (3 -2))))
(print (matrix-det '((1 -1 3) (3 4 -2) (0 3 5))))
(print (matrix-det '((2 3 0 4 5) (0 1 0 -1 2) (3 2 1 0 1) (0 4 0 -5 0) (1 1 2 -2 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #2
;;; Написать генератор совершенных чисел. Число называется совершенным,
;;; если оно равно сумме своих делителей.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #3
;;; Близнецами называется пара натуральных чисел, каждое из которых равно
;;; сумме делителей другого числа. Так, например, числа 220 и 284 — близнецы.
;;; Написать генератор первых n пар близнецов.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #4
;;; Заданный числовой список разбить на подсписки из возрастающих
;;; подпоследовательностей максимальной длины рядом стоящих чисел.
;;; Так, например, если исходный список состоял из чисел 
;;; [2, 7, 10, 8, 3, 4, 9, 1, 2, 0, 8, 3, 2, 5], то результатом работы
;;; программы должен быть следующий список списков:
;;; [[2, 7, 10], [8], [3, 4, 9], [1, 2], [0, 8], [3], [2, 5]].

(defun f (lst)
    (cond
        ((null lst) nil)
        ((null (cdr lst)) (list lst))
        (t ((lambda (prev-res head) (if (< head (caar prev-res))
                (cons (cons head (car prev-res)) (cdr prev-res))
                (cons (list head) prev-res)))
            (f (cdr lst)) (car lst)))
    )
)

(print (f '()))
(print (f '(2)))
(print (f '(2 7 10 8 3 4 9 1 2 0 8 3 2 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #5
;;; Написать алгоритм быстрой сортировки (сортировки слиянием) согласно
;;; функции сравнения двух элементов и удалить из полученного списка элементы,
;;; не удовлетворяющие заданному условию. Продемонстрировать на примере
;;; списка студентов, где каждый студент имеет свойства «Фамилия», «Имя», 
;;; «Группа» и «Cредний бал». В качестве условия–фильтра рассмотреть
;;; «Средний бал больше 4», порядок сортировки — «Группа–Фамилия–Имя–Средний бал».

(defun list-head (lst n) (if (eq n 0) '() (cons (car lst) (list-head (cdr lst) (- n 1)))))
(defun list-tail (lst n) (if (eq n 0) lst (list-tail (cdr lst) (- n 1))))

(defun _merge (lst-a lst-b)
  (cond ((null lst-a) lst-b)
        ((null lst-b) lst-a)
        ((< (car lst-a) (car lst-b)) (cons (car lst-a) (_merge (cdr lst-a) lst-b)))
        (t (cons (car lst-b) (_merge lst-a (cdr lst-b))))))

(defun mergesort (lst)
  (if (null (cdr lst))
    lst
    (_merge (mergesort (list-head lst (truncate (length lst) 2)))
            (mergesort (list-tail lst (truncate (length lst) 2))))))

       
(defun set-student (id -group -surname -name -ev-mark)
    (setf (get id 'group) -group)
    (setf (get id 'surname) -surname)
    (setf (get id 'name) -name)
    (setf (get id 'ev-mark) -ev-mark)
)

(defun get-student (x)
    (list (get x 'group) (get x 'surname) (get x 'name) (get x 'ev-mark))
)

(set-student 'student "402-I" "Green" "Peter" 5)
;(set-student (gen-nat-num) '402-I 'Green 'Peter 5)
;(set-student (gen-nat-num) '401-I 'Crestalise 'Jane 3)
;(set-student (gen-nat-num) '202-I 'Zeinberg 'Roger 4)
;(set-student (gen-nat-num) '103-I 'Crysler 'Gome 2)


(print (get-student 'student ))
;(print (get-student '6 ))
;(print (get-student '3 ))
;(print (get-student '1 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #6
;;; Структура графа задана списками смежности номеров вершин, то есть списком,
;;; элементами которого являются пары, состоящие из номера вершины и 
;;; списка вершин, инцидентных ей. type Graph = (Int, (Int, Int, ...))
;;; Написать функцию, которая выдает длину кратчайшего маршрута между двумя
;;; заданными вершинами. Длиной считать количество вершин, встретившихся 
;;; в данном маршруте. Если маршрута между вершинами не существует,
;;; то функция должна выдавать ноль.

(setq graph '((1 (3 4)) (2 (1 6)) (3 (2)) (4 (1 5 6)) (5 (4 3 1)) (6 (5 4 2))))

;; список вершин, связанных с данной и не посещенных
 
(defun getall (v graph chk) 
 (remove-if #'(lambda (z) (member z chk))
                (cadar (remove-if-not #'(lambda (x) (member v x)) graph))))
 
;; обход в ширину с построением каркаса                       
                        
(defun bfs! (graph chk queue)
   (cond ((null queue) nil)
         (t (let ((n (getall (car queue) graph chk)))
                 (append 
                    (mapcar #'(lambda (x) (list (car queue) x)) n) 
                    (bfs! graph (append chk n) (append (cdr queue) n)))))))
 
;; найти путь в каркасе от вершины n1 до n2
                 
(defun spath (carc n1 n2 &optional (c carc) (r 0))
  (cond ((null carc) nil)
        ((= n1 n2) r)
        ((= (cadar carc) n2) (spath c n1 (caar carc) c (+ 1 r)))
        (t (spath (cdr carc) n1 n2 c r)))) 
                        
;; ведущая программа   
   
(defun  task (graph n1 n2)
  (let ((carc (bfs! graph (list n1) (list n1))))
        (spath carc n1 n2)))
        
      
(print (task graph 3 3))
(print (task graph 3 5423))
(print (task graph 3 2))
(print (task graph 1 6))
(print (task graph 5 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #7
;;; Написать программу для нахождения минимального из чисел, являющихся
;;; максимальными в каждой из строк заданной прямоугольной матрицы. 
;;; Использовать применяющие и/или отображающие функционалы.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #8
;;; В заданном списке списков найти самый длинный подсписок. Использовать
;;; отображающие и применяющие функционалы.

(defun all-longest-sublists (lst)
    ((lambda (maxl) (mapcan #'(lambda (sub-lst) (when (eq (length sub-lst) maxl) (list sub-lst))) lst))
         (apply 'max (mapcar 'length lst)))
)

(print (all-longest-sublists '(())))
(print (all-longest-sublists '((3))))
(print (all-longest-sublists '((3) () (2 1) (4) (42 2))))
(print (all-longest-sublists '((3) () (2 1) (4) (42 2) (1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #9
;;; Выяснить является ли дерево пирамидой — дерево, в котором каждая 
;;; корневая вершина меньше любой своей дочерней.

(setq tree1 '(0 (1 (3 nil nil) nil) (2 (4 nil nil) (5 nil nil)))) ;T
(setq tree2 '(0 (1 (3 nil nil) nil) (4 (2 nil nil) (5 nil nil)))) ;nil

(defun is-pyramid (pyr)
    (cond
        ((null pyr) T)
        ((let ((left (caadr pyr))) (and (not (null left)) (< left (car pyr)))) nil)
        ((let ((rigth (caaddr pyr))) (and (not (null rigth)) (< rigth (car pyr)))) nil)
        (t (and (is-pyramid (cadr pyr)) (is-pyramid (caddr pyr))))
    )
)

(print (is-pyramid tree1))
(print (is-pyramid tree2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #10
;;; Граф с конечным числом вершин, пронумерованных от 1 до N, представлен
;;; парой из количества вершин N и характеристической функции множества его
;;; дуг типа (Int Int) -> Bool, которая выдает T, если аргументы представляют 
;;; собой номера вершин, соединенных дугой, и NIL в противном случае.
;;; type Graph = (Int, function: (Int Int) -> Bool)
;;; Реализовать функцию, которая проверяет, имеется ли в графе маршрут,
;;; соединяющий две заданные вершины.

(setq graph '((1 2) (2 3) (3 4) (4 2) (5 4) (3 7)))

(defun graph (x y)
    (or (find (list x y) graph :test #'equal) 
        (find (list y x) graph :test #'equal)))

(defun get-all-linked-nodes (x g)
    (cond
        ((null g) nil)
        (t ((lambda (prev-res) (cond 
                                   ((eq x (caar g)) (cons (cadar g) prev-res)) 
                                   ((eq x (cadar g)) (cons (caar g) prev-res)) 
                                   (t prev-res)))
           (get-all-linked-nodes x (cdr g))))
        )
    )

(defun exist-way (x y)
    (dfs y nil (list x)))

(defun subtr-lst-with-dupl (lst subtract)
    (cond
        ((null lst) nil)
        (t ((lambda (prev-res) (if (find (car lst) subtract :test #'equal) prev-res (cons (car lst) prev-res)))
            (subtr-lst-with-dupl (cdr lst) subtract)))
    )
)

(defun dfs (y cn on)
    (cond
        ((null on) nil)
        ((graph (car on) y) (list (car on) y))
        (t ((lambda (new-on new-cn) ((lambda (prev-res) (if (null prev-res) nil (cons (car on) prev-res)))(dfs y new-cn new-on)))
                 (remove-duplicates (append (subtr-lst-with-dupl (get-all-linked-nodes (car on) graph) cn) (cdr on)))
                 (cons (car on) cn)))
        )
    )


(print (exist-way 1 31))
(print (exist-way 1 1))
(print (exist-way 1 3))
(print (exist-way 5 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #11
;;; Сформировать список разностей соседних элементов в последовательности 
;;; квадратов натуральных чисел. Убедиться, что эта последовательность 
;;; представляет собой список последовательных нечетных чисел. 
;;; Использовать применяющие и/или отображающие функционалы.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #12
;;; Бесконечная упорядоченная последовательность целых чисел составлена 
;;; из степеней двойки и чисел вида 2 ¢ 3n. 
;;; Написать генератор такой последователь-ности.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #13
;;; «Бесконечная» матрица содержит все натуральные числа в «диагональном» порядке.
;;; Первые несколько элементов матрицы выглядят следующим образом:
;;; 1 2 4 7 11 16 . . .
;;; 3 5 8 12 17 . . .
;;; 6 9 13 18 . . .
;;; 10 14 19 . . .
;;; Построить эту матрицу в виде бесконечного списка бесконечных списков и
;;; вычислить сумму первых 10 элементов 5-го столбца матрицы.

#|
(defun range (n &key (from 0) (step 1))
   (let ((res nil)) (dotimes (i n) (push (+ from (* step i)) res)) (reverse res)))
;(print (range 10 :from 2 :step 3))

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))
;(print (range 10 :min 1 :step 2))
|#

(defun sum-mtrx-rect (m &key row-from row-to col-from col-to)
    (apply '+ (mapcar #'(lambda (row) (apply '+ (subseq row col-from col-to))) (subseq m row-from row-to))))

(defun matrix ()
    (let ((m '((1))) (n 1))
         (lambda () (prog1
            (reverse (mapcar 'reverse m))
            (setf m ((lambda (tm) (cons (list (+ 1 (caar tm))) tm))
                     (mapcar #'(lambda (row) (cons (+ n (car row)) row)) m)))
            (incf n)))))


(setq m-gen (matrix))
(setq m-static (let ((res nil)) (dotimes (i 14 res) (setf res (funcall m-gen)))))


(print-matrix m-static t)
(print (sum-mtrx-rect m-static :row-from 0 :row-to 10 :col-from 4 :col-to 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; #14
;;; Реализовать функцию, порождающую булеан (множество всех подмножеств) заданного множества.


