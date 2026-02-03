(= (contains_symbol $list $sym)
  (if (== $list ())  False
  (if (== (car-atom $list) $sym)
  True
  (contains_symbol (cdr-atom $list) $sym))))


(= (add_elem_if_not_in_listx $list $sym)
  (if (contains_symbol $list $sym )  $list  (cons_atom $sym $list)  )
  )



(= (remove_dups $list_netto $expr)
 (if (== $expr ()) $list_netto
       (let_star2 (groupe2 ($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($list_nettx (add_elem_if_not_in_listx $list_netto $head))
     ($tail_new (remove_dups $list_nettx $tail))
)
 $list_nettx
)
)
)

; test with:  remove_dups( [] , [1,1,2], Outp )


