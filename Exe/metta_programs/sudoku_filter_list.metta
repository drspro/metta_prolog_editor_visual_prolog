
(= (filter_elem $va)  (if (> $va 0) $va () ) ) 

(= (filter_elems $expr)
 (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($head_new (filter_elem $head))
     ($tail_new (filter_elems $tail))
)
(cons-atom $head_new $tail_new)
)
)
)