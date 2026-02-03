
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



(= (is_not_present_in_row $r $c $num) 
 (match &self (komma (sudoku_puzzle_state $r $nc $nq $rstate ) (is_not_equal $rstate $num) )
 $num)
    )



(= (get_valid_candidate $r $c)
  
 (match &self (komma (sudoku_number $num ) (is_not_present_in_row $r $c $num) )
 $num
 )
 )


