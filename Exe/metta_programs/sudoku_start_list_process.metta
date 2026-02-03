(= (contains_symbol $list $sym)
  (if (== $list ())  False
  (if (== (car-atom $list) $sym)
  True
  (contains_symbol (cdr-atom $list) $sym))))


(= (elem_is_not_in_list $list $sym)
  (if (contains_symbol $list $sym )  0  $sym )  
  )


(= (nums_are_not_in_list $expr $nums_list)
 (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($head_new (elem_is_not_in_list $nums_list $head))
     ($tail_new (nums_are_not_in_list $tail $nums_list))
)
(cons-atom $head_new $tail_new)
)
)
)