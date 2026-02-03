


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

; START apply_action process_sudoku

(=  (create_action_stack)
  (match_all &act_stack 
   (komma  (sudoku_puzzle_state  $r $c $state )     (is_equal_fail $state 0) )
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
 (let_star6  (groupe11
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
 
 (let_star9 (groupe15
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
