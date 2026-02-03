
(= (nth $n $list)
    (if (== $n 1)
        (car_atom $list)
        (nth (- $n 1) (cdr_atom $list))))
		
		
		
		
		