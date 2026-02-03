

(= (add_elem_if_not_in_listy $list_netto $list $sym)
  (if (contains_symbol $list $sym )  $list_netto  (cons_atom $sym $list_netto)  )
  )



(= (subtract_list $list_netto $list_subtract $expr)
 (if (== $expr ()) $list_netto
       (let_star3 (groupe3 ($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($list_nettx (add_elem_if_not_in_listy $list_netto $list_subtract $head))
     ($tail_new (subtract_list $list_nettx $list_subtract $tail))
)
 $list_nettx
)
)
)

; false_candidate( Level , Row , Col , H )  , !,

(= (all_false_cands_in_level $lev $r $c) 
    (match_all &false_cands (false_candidate $lev $r $c $elem ) 
     $elem )     )

; todo list_get_nth plus verify if it works : find_first_field_which_had_alternatives
(= (has_unused_cands $lev $r $c $cands)
 (let_star7  (groupe12

      ($false_cands (all_false_cands_in_level $lev $r $c))
      ($netlist (subtract_list () $false_cands $cands) )
      ($le (list_length $netlist 0) )
      ($Is_big_enough (if (biggerthan $le 2) True False) )
      (is_equal_fail $Is_big_enough True)
      ; it must fail here if not true , find a solution  
      ($last_cand (list_get_nth $netlist 0) )
     )
     $last_cand 
  )
    )

(=  (had_alternatives $lev $r $c )
 (match &mem_cands 
   (komma (mem_candidates $r $c $cands  ) 
 
     ($last_cand (has_unused_cands $lev $r $c $cands) )
     )
  $last_cand
    )
 )

; todo:  verif if it can work 
(= (find_first_field_which_had_alternatives )
 (match &trace_reverse 
    (komma (trace_field_assigned_reverse $lev $r $c)   ($last_cand (had_alternatives $lev $r $c)) )  
  (alternative_field $lev $r $c $last_cand) 
 )
 )

