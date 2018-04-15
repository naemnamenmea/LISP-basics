;; format an invoice
(loop for (code desc quant price) in
'((42 "House" 1 110e3) (333 "Car" 2 15000.99) (7 "Candy bar" 12 1/4))
do (format t "~3,'0D ~10A ~3D @ $~10,2,,,'*F~%" code desc quant price))

;; format an invoice again, one-liner
(format t "~:{~3,'0D ~10A ~3D @ $~10,2,,,'*F~%~}"
'((42 "House" 1 110e3) (333 "Car" 2 15000.99) (7 "Candy bar" 12 1/4)))

;; comma-separated list
(loop for i from 1 to 4 do
(format t "~{~A~^, ~}~%" (subseq '(1 2 3 4) 0 i)))

;; comma-separated list again, but cleverer
;; (using things I didn’t mention above :-)
(loop for i from 1 to 4 do
(format t "~{~A~#[~; and ~:;, ~]~}~%" (subseq '(1 2 3 4) 0 i)))