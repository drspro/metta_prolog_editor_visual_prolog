(= (contains_symbol $list $sym)
  (if (== $list ())  False
  (if (== (car-atom $list) $sym)
  True
  (contains_symbol (cdr-atom $list) $sym))))




(= (append_lis $lis_els2 $lis_els1)
 (if (== $lis_els2 ()) $lis_els1
       (let_star2 (groupe2 ($head (car-atom $lis_els2))
           ($tail (cdr-atom $lis_els2))
; ( aaa
        ($list_nettx (cons_atom $head $lis_els1))
          
     ($tail_new (append_lis $tail $list_nettx))
)
 $list_nettx
)
)
)


