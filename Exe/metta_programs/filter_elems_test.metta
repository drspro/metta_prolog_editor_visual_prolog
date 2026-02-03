

(= (filter_elem $va)  (if (bigger_then $va 0) $va () ) ) 


(= (filter_elems $expr)
 (if (== $expr ()) ()
       (let_star3_2 (groupe5_cat3_4 
        ($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($head_new (filter_elem $head))
     ($tail_new (filter_elems $tail))
)
(cons-atom $head_new $tail_new)  )  )  )

