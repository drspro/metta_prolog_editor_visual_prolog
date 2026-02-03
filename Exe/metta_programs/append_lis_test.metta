
(= (append_lis $lis_els1 $lis_elsx2)
 (if (== $lis_els1 ()) $lis_elsx2
; doubt here temp     
       (let_star3_2 
        (groupe5_cat3_4 
;        (groupe5_3 
        ($head (car-atom $lis_els1))
        ($tail (cdr-atom $lis_els1))
        ($list_nettx (cons_atom $head $lis_elsx2))
          
        ($tail_new (append_lis $tail $list_nettx))
)
 $tail_new  )  )  )
