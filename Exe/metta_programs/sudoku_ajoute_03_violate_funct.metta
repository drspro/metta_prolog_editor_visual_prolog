

(mem_candidates 0 0 () )

; new predicate   which we need to implement 
; it  seems to    work like this 
; doubt if we are going to use match_all  ( then we are ready )
;  or make the db predicate return with "False"


(= (solution_is_violated )   
 

  (match &mem_cands 

    (komma (mem_candidates $r $c $cands)   (list_length_is_zero_fail_false $cands)  )

  (is_vio $r  $c)  ) 
 

 )

