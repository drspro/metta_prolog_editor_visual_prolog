(= (contains_symbol $list $sym)
  (if (== $list ())  False
  (if (== (car-atom $list) $sym)
  True
  (contains_symbol (cdr-atom $list) $sym))))


(= (add_elem_if_not_in_listy $list $sym $lis_crea)
  (if (contains_symbol $list $sym )  $lis_crea  (cons_atom $sym $lis_crea)  )
  )



(= (difference_lis $lis_els2 $lis_els1 $lis_result)
 (if (== $lis_els2 ()) $lis_result
       (let_star2 (groupe2 ($head (car-atom $lis_els2))
           ($tail (cdr-atom $lis_els2))
        ($list_nettx (add_elem_if_not_in_listy $lis_els1 $head $lis_result))
     ($tail_new (difference_lis $tail $lis_els1 $list_nettx))
)
 $list_nettx
)
)
)


; test with:  difference_lis( [4,2,5,1] , [1,1,2] , [], Outp )