


(sudoku_number 1 )
(sudoku_number 2 )
(sudoku_number 3 )
(sudoku_number 4 )
(sudoku_number 5 )
(sudoku_number 6 )
(sudoku_number 7 )
(sudoku_number 8 )
(sudoku_number 9 )



(sudoku_puzzle_state 1 1 1 5  )
(sudoku_puzzle_state 1 2 1 3  )
(sudoku_puzzle_state 1 3 1 0  )
(sudoku_puzzle_state 1 4 2 0  )
(sudoku_puzzle_state 1 5 2 7  )
(sudoku_puzzle_state 1 6 2 0  )
(sudoku_puzzle_state 1 7 3 0  )
(sudoku_puzzle_state 1 8 3 0  )
(sudoku_puzzle_state 1 9 3 0  )
(sudoku_puzzle_state 2 1 1 6  )
(sudoku_puzzle_state 2 2 1 0  )
(sudoku_puzzle_state 2 3 1 0  )
(sudoku_puzzle_state 2 4 2 1  )
(sudoku_puzzle_state 2 5 2 9  )
(sudoku_puzzle_state 2 6 2 5  )
(sudoku_puzzle_state 2 7 3 0  )
(sudoku_puzzle_state 2 8 3 0  )
(sudoku_puzzle_state 2 9 3 0  )
(sudoku_puzzle_state 3 1 1 0  )
(sudoku_puzzle_state 3 2 1 9  )
(sudoku_puzzle_state 3 3 1 8  )
(sudoku_puzzle_state 3 4 2 0  )
(sudoku_puzzle_state 3 5 2 0  )
(sudoku_puzzle_state 3 6 2 0  )
(sudoku_puzzle_state 3 7 3 0  )
(sudoku_puzzle_state 3 8 3 6  )
(sudoku_puzzle_state 3 9 3 0  )
(sudoku_puzzle_state 4 1 4 8  )
(sudoku_puzzle_state 4 2 4 0  )
(sudoku_puzzle_state 4 3 4 0  )
(sudoku_puzzle_state 4 4 5 0  )
(sudoku_puzzle_state 4 5 5 6  )
(sudoku_puzzle_state 4 6 5 0  )
(sudoku_puzzle_state 4 7 6 0  )
(sudoku_puzzle_state 4 8 6 0  )
(sudoku_puzzle_state 4 9 6 3  )
(sudoku_puzzle_state 5 1 4 4  )
(sudoku_puzzle_state 5 2 4 0  )
(sudoku_puzzle_state 5 3 4 0  )
(sudoku_puzzle_state 5 4 5 8  )
(sudoku_puzzle_state 5 5 5 0  )
(sudoku_puzzle_state 5 6 5 3  )
(sudoku_puzzle_state 5 7 6 0  )
(sudoku_puzzle_state 5 8 6 0  )
(sudoku_puzzle_state 5 9 6 1  )
(sudoku_puzzle_state 6 1 4 7  )
(sudoku_puzzle_state 6 2 4 0  )
(sudoku_puzzle_state 6 3 4 0  )
(sudoku_puzzle_state 6 4 5 0  )
(sudoku_puzzle_state 6 5 5 2  )
(sudoku_puzzle_state 6 6 5 0  )
(sudoku_puzzle_state 6 7 6 0  )
(sudoku_puzzle_state 6 8 6 0  )
(sudoku_puzzle_state 6 9 6 6  )
(sudoku_puzzle_state 7 1 7 0  )
(sudoku_puzzle_state 7 2 7 6  )
(sudoku_puzzle_state 7 3 7 0  )
(sudoku_puzzle_state 7 4 8 0  )
(sudoku_puzzle_state 7 5 8 0  )
(sudoku_puzzle_state 7 6 8 0  )
(sudoku_puzzle_state 7 7 9 2  )
(sudoku_puzzle_state 7 8 9 8  )
(sudoku_puzzle_state 7 9 9 0  )
(sudoku_puzzle_state 8 1 7 0  )
(sudoku_puzzle_state 8 2 7 0  )
(sudoku_puzzle_state 8 3 7 0  )
(sudoku_puzzle_state 8 4 8 4  )
(sudoku_puzzle_state 8 5 8 1  )
(sudoku_puzzle_state 8 6 8 9  )
(sudoku_puzzle_state 8 7 9 0  )
(sudoku_puzzle_state 8 8 9 0  )
(sudoku_puzzle_state 8 9 9 5  )
(sudoku_puzzle_state 9 1 7 0  )
(sudoku_puzzle_state 9 2 7 0  )
(sudoku_puzzle_state 9 3 7 0  )
(sudoku_puzzle_state 9 4 8 0  )
(sudoku_puzzle_state 9 5 8 8  )
(sudoku_puzzle_state 9 6 8 0  )
(sudoku_puzzle_state 9 7 9 0  )
(sudoku_puzzle_state 9 8 9 7  )
(sudoku_puzzle_state 9 9 9 9  )


; START  set_board_state 

; tested 
; append_lis([4,5,6,7],[9,8,7,6],Ou)
; TODO:: if you let remove metta_vars  it should maintain ony the variabel
; that is used in the let_star as last argument 
; can we make this logically deduced 
;  IF the group predicate is Inside a let_star , MAIntain the variabel from
; the letstar 
; Hopefully this will work  to apply this in the 2 places where the constant 
; remove_metta_output_and_created is used (203)
; current_transpile_model(0,201,203,203,1).
; line 3751 , 3774  ,  5216 5147 , Tag is: current_transpile_model_get(
; in this predicate : vars_slist_remove_pred_out_vars( line 5149  you have a predname there also 5269
; it must be able to maintain  the one variabel 
; predicate_nesting_ordered(3,2,1,1,2,"let_star3_2",["$tail_new"],[2],5,1,2).
; nested_pred_memory(3,2,1,1,2,"let_star3_2",2,"let_star3_2_groupe5_cat3_4",["$tail_new"],[2,3],5,["let_star3_2","groupe5_cat3_4"]).


; groupe5_cat3_4_car_atom_cdr_atom_cons_atom_append_lis
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



(= (filter_elem $va)  (if (bigger_than $va 0) $va () ) ) 

; tested these 2 works 
; filter_elems( [9,8,7,0,6,5,4,3,0, 2,1], Kg )
(= (filter_elems $expr)
 (if (== $expr ()) ()
;       (let_star3_2 (groupe5_cat3_4 
       (let_star3_2 (groupe5_2 
        ($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($head_new (filter_elem $head))
     ($tail_new (filter_elems $tail))
)
(cons-atom $head_new $tail_new)  )  )  )


(= (list_length $list $init_len)
  (if (== $list ())  $init_len
  (list_length (cdr-atom $list) (plus $init_len 1) )  ) )




(= (contains_symbol $list $sym)
  (if (== $list ())  False
  (if (== (car-atom $list) $sym)
  True
  (contains_symbol (cdr-atom $list) $sym) ) ) )


(= (add_elem_if_not_in_listy $list_netto $list $sym)
  (if (contains_symbol $list $sym )  $list_netto  (cons_atom $sym $list_netto)  )  )


;call like this:
; subtract_list([], [2,4], [8,9,1,2,2,21,34,4], Og )


(= (subtract_list $list_netto $list_subtract $expr)
 (if (== $expr ()) $list_netto
       (let_star3_2 (groupe5_3 ($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($list_nettx (add_elem_if_not_in_listy $list_netto $list_subtract $head))
     ($tail_new (subtract_list $list_nettx $list_subtract $tail))
)
 $tail_new ) ) )


(= (add_elem_if_not_in_listx $list $sym)
  (if (contains_symbol $list $sym )  $list  (cons_atom $sym $list)  )  )

; 5_2 is arbitrair
; remove_dups( [] ,[6,4,7,4,3,2,3,1,1], Wq )
(= (remove_dups $list_netto $expr)
 (if (== $expr ()) $list_netto
       (let_star3_2 (groupe5_2 
        ($head (car-atom $expr))
           ($tail (cdr-atom $expr))
        ($list_nettx (add_elem_if_not_in_listx $list_netto $head))
     ($tail_new (remove_dups $list_nettx $tail))
)
 $tail_new ) )  )


(= (get_quad $r $c )
 (match &quads (sudoku_puzzle_state $r $c $q $state )  $q ) )

(= (get_row_state $r )
 (match_all &rows (sudoku_puzzle_state $r $c $q $state )  $state ) )

(= (get_col_state $c )
 (match_all &cols (sudoku_puzzle_state $r $c $q $state )  $state ) )

(= (get_quad_state $q )
 (match_all &quadstate (sudoku_puzzle_state $r $c $q $state )  $state ) )



; everything should be let_star3_2   group doesnt matter should only be the right arity
 (= (cell_candidates $r $c) 
        (let_star3_2  (groupe9_8
         ($row_sta  (get_row_state $r)   )
         ($col_sta  (get_col_state $c)   )
         ($q (get_quad $r $c) )
         ($q_sta  (get_quad_state $q)   )
         ($app1 (append_lis $row_sta $col_sta) )
          ($app2 (append_lis $app1 $q_sta) )
          ($lis3 (filter_elems $app2) )
          ($lis4 (remove_dups () $lis3) )
          (println $lis4)
         ) 
         $lis4         )            )


 (= (set_board_state )
 (match_all &self 
    (komma (sudoku_puzzle_state $r $c $q $state)   ($cands (cell_candidates $r $c) )  )
  (add_atom (mem_candidates $r $c $cands  ) ) 
 ) )

; END  set_board_state 

;(= (list_length_is_zero $list)
;  (if (== (list_length $list 0) 0)  True
;  False  ) )


; START  make_backtrack 



(xcount 1)

(= (get_count )  (match &counts (xcount $co)    $co    )   )

(= (increment_count )    
 (let_star3_2  (groupe5_3
      ($was (get_count ) )
      ($we  (remove_atom &counts (xcount $was)) )
      ($cx  (plus $was 1) )
      ($re (add_atom &counts (xcount $cx ))   )
     )
     $cx 
  )   )


(= (get_state $r $c $q )
 (match &state (sudoku_puzzle_state $r $c $q $state )  $state ) )


; false_candidate( Level , Row , Col , H )  , !,

(= (all_false_cands_in_level $lev $r $c) 
    (match_all &false_cands (false_candidate $lev $r $c $elem ) 
     $elem )     )

; todo list_get_nth plus verify if it works : find_first_field_which_had_alternatives
(= (has_unused_cands $lev $r $c $cands)
 (let_star3_2  (groupe7_6

      ($false_cands (all_false_cands_in_level $lev $r $c))
      ($netlist (subtract_list () $false_cands $cands) )
      ($le (list_length $netlist 0) )
      ($Is_big_enough (if (bigger_than $le 2) True False) )
      (is_equal_fail $Is_big_enough True)
      ; it must fail here if not true , find a solution  
      ($last_cand (list_get_nth $netlist 0) )
     )
     $last_cand   )    )

(=  (had_alternatives $lev $r $c )
 (match &mem_cands 
   (komma (mem_candidates $r $c $cands  ) 
      ($last_cand (has_unused_cands $lev $r $c $cands) )
     )
  $last_cand     ) )

; todo:  verif if it can work 
(= (find_first_field_which_had_alternatives )
 (match &trace_reverse 
    (komma (trace_field_assigned_reverse $lev $r $c)   ($last_cand (had_alternatives $lev $r $c)) )  
  (alternative_field $lev $r $c $last_cand)  ) )

; END  make_backtrack 



; START apply_action process_sudoku

(=  (create_action_stack)
  (match_all &act_stack 
   (komma  (sudoku_puzzle_state  $r $c $q $state )     (is_equal_fail $state 0) )
  (add_atom (action_stack $r $c) )    )	)

; these 2 works also    simplest form that works 
; alternative 
;(= (get_possible_candidates $r $c)
(= (get_possible_candidates (sudoku_candidate_cell $r $c) )
 (match &cands_db (komma (mem_candidates $r $c $cands) (list_length_bigger_than_zero_fail $cands) )
  	 $cands  ) )

(= (get_head_candidate $candsx)
  (car_atom $candsx)   )


; like this it works , you have to make a predicate for remove_atom
;alternative 
(= (remove_action_stack $r $c )
 (remove_atom (action_stack $r $c)) )

; alternative
(= (get_action_from_stack )
 (match &act_stack_db   (komma (action_stack $r $c) (remove_action_stack $r $c) ) 
 	(sudoku_candidate_cell $r $c)  ) )

(= (get_quad $r $c )
 (match &quads (sudoku_puzzle_state $r $c $q $state )  $q ) )

(= (get_state $r $c $q)
 (match &sudok (sudoku_puzzle_state $r $c $q $state )  $state ) )


;  add get_quad  get_state  as predicate 
;(= (apply_candidate (sudoku_candidate_cell $r $c) $val)    
(= (apply_candidate (sudoku_candidate_cell $r $c) $val )    
 (let_star3_2  (groupe6_2
      ($q (get_quad $r $c) )
      ($state (get_state $r $c $q))
      (remove_atom &candx (sudoku_puzzle_state $r $c $q $state  )) 
      (dummyx 14)
       (add_atom &candx (sudoku_puzzle_state $r $c $q $val  ) )   
     )
     $state 
  )   )

;  als je nul , dus geen variabelen mee-geeft  in de head (in process_program)
; dan zet hij het transpile model anders / different
;  met deze dymmy variabele   lijkt hij het nu wel volledig goed te doen 

(= (process_program $a) 
 
 (let_star3_2 (groupe5_2
  ($act (get_action_from_stack) )
  ($candsx (get_possible_candidates $act) )
  ($head_cand (get_head_candidate $candsx) )
  
  (apply_candidate $act $head_cand)
    ) 
  $head_cand 

  )

 )


; END apply_action process_sudoku



;  ($q (get_quad $r $c) )
;  ($state (apply_candidate $r $c $q 0))
