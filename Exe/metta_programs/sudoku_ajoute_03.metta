

(=  (trace_remove_apply_cell $lev $r $c)

 (let_star8 (groupe14
  (add_atom (trace_remove $lev $r $c))
  ($q (get_quad $r $c) )
  ($state (apply_candidate $r $c $q 0))
    ) $state )
  )

(= (rewind_to_level_level $flev)  
 (match_all &trace 
   (komma (trace_field_assigned_reverse $lev $r $c) (is_bigger_than_equal_fail $lev $flev)     )
  (trace_remove_apply_cell $lev $r $c)
    )
 )
  

(= (rewind_to_level $flev $row $col $lastused_candidate)
 (let_star7 (groupe13
  ($lev1 (rewind_to_level_level $flev) ) 
  ($lev2 (rewind_to_level_trace_field $flev) ) 
  ($lev3 (rewind_to_level_trace_remove $flev) ) 
  ($lev4 (rewind_to_level_trace_assigned $flev) ) 
  ($lev5 (rewind_to_level_false_cand $flev $row $col $lastused_candidate) ) 
  ) 
 $lev5

  )

  )

; new predicate   which we need to implement 
(= (empty_mem_candidates )   
 (remove_atoms  mem_candidates 3  )
 )


;empty_mem_candidates( Outp0 ):-
;  remove_atoms( "mem_candidates" , 3 , Outp0 ) ,  
;  ! .




;rewind_to_level( Current_level, Field_level , _Row , _Col , _LastUsed_candidate ):-
; write( "RESET BACK TO LEVEL: " ), 
; Field_level2 is Field_level - 1 , 
; write( Field_level2 ), write( " FROM LEVEL: " ), write( Current_level ),
;  nl, 
;  retractall( level( _ ) ), assert( level( Field_level2 ) ), fail, ! .

;rewind_to_level( _, Field_level , _Row , _Col , _LastUsed_candidate ):-  retractall( trace_remove( _, _, _  ) ),
; trace_field_assigned_reverse( Level, Row, Col ) ,  Level >= Field_level , 
; write_term( reset_field_above_level( Row, Col )   , [] ) , nl,
; assert( trace_remove( Level, Row, Col ) ),
; assign_sudoku_cell_with_number( Row, Col , 0 ), fail , ! .
 

;rewind_to_level( _, _Field_level , _Row , _Col , _LastUsed_candidate ):-
; trace_remove( Level, Row, Col ), retract( trace_field_assigned_reverse( Level, Row, Col ) ), fail, !.

;rewind_to_level( _, Field_level , _Row , _Col , _LastUsed_candidate ):-
; trace_field_assigned( Level, Row, Col ) , 
; Level >= Field_level ,
; retract( trace_field_assigned( Level, Row, Col ) ),
; write_term( re_asserta_stack( Row, Col )  , [] ) , nl,
; asserta( action_stack( Row, Col ) ), fail , ! .

;rewind_to_level( _, Field_level , Row , Col , LastUsed_candidate ):-
;  write( "ASSERT FALSE CANDIDATE: \n" ), 
;  write_term( false_candidate( Row , Col ), [] ), nl , 
;  assert( false_candidate( Field_level , Row , Col , LastUsed_candidate ) ) , !.

;rewind_to_level( _, _Field_level , _Row , _Col , _LastUsed_candidate ):-!.
