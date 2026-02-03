 in the first idea we are going to use


* use these 
 todo rename names for predicates 



(= (is_not_equal_and_not_present_in_quad_segment $rstate $cstate $num $nxq) 
 (match &self (komma (sudoku_puzzle_state $nyr $nyc $nxq $qstate ) (is_not_equal $rstate $cstate $qstate $num ) )
 $num)
    )

(= (is_not_equal_and_not_present_in_col_quad_segment $rstate $num $nc $nq) 
 (match &self (komma (sudoku_puzzle_state $nxr $nc $nxq $cstate ) (is_not_equal_and_not_present_in_quad_segment $rstate $cstate $num $nxq) )
 $num)
    )

(= (is_not_present_in_row_col_quad_segment $r $c $num) 
 (match &self (komma (sudoku_puzzle_state $r $nc $nq $rstate ) (is_not_equal_and_not_present_in_col_quad_segment $rstate $num $nc $nq) )
 $num)
    )
; Test design  before to implement 
; later  add also not prsent in col segment and quad segment 
(= (get_valid_candidate $r $c)
  
 (match &self (komma (sudoku_number $num ) (is_not_present_in_row_col_quad_segment $r $c $num) )
 $num
 )
 )



 in the match as first argument

 then the second argument would become:

 



----------------------------------------
remains to do

 process walk through a list 

* advanced  action in match result , but maye we are not going to allow this
and we make a simplified , always first generate the list and then process it 

