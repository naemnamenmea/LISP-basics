;;; #6
;;; Структура графа задана списками смежности номеров вершин, то есть спис-ком, элементами которого являются пары, состоящие из номера вершины и списка вершин, инцидентных ей. type Graph = (Int, (Int, Int, ...))
Написать функцию, которая выдает длину кратчайшего маршрута между дву-мя заданными вершинами. Длиной считать количество вершин, встретивших-ся в данном маршруте. Если маршрута между вершинами не существует, то функция должна выдавать ноль.


