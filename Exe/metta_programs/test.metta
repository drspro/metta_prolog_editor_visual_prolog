





(= (contains_symbol $list $sym) 
    (if (== $list ())
        False
        (if (== (car-atom $list) $sym)
            True
            (contains_symbol (cdr-atom $list) $sym))))



;!(contains_symbol (1 3 4 5 8 9 5) (5) )