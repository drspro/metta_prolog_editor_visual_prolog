
(= (G)
  (match &self (game-state $msg)
  % check for game over...
    (if (xor (==  checkmate   $msg) (==  resigned $msg))
      % then
      (println! "Game over. Please reset to play again (enter r).")
      %else keep playing...
      (progn
        % get the current board
        (match &self (board-state $starting_board) $starting_board)
        % Add each piece to &self atomspace to work with individually with form eq '(square 1 1 s r)'.
        % These atoms are used to "envision" moves only. So if you alter these atoms, when done immediately
        % reverse back to the starting state.
        % You can use move_piece and reset_pieces to respectively envision a move, then reverse the move.
        (reset-pieces $starting_board)
        % determine next AI move
        (let $move (decide_greedy_move) NIL )
        (if  (== $move ())
          (let_star
            % if all attempts fail, locate AI king and see if he is in checkmate
            (
            ($king_square (xy_box (g k)))
            ($full_king_sq (return_entire_box $king_square))
            ($KingCompromised (take_dest $full_king_sq s))
            )
            (if (== $KingCompromised True)
              (progn
                (println! "Checkmate! You win!")
                (println! "Winner!  Winner!  Winner!")
                (println! "")
                (change-game-state checkmate)
                )
              (progn
                (println! "Greedy Chess cannot find a good move. Game over. You win!")
                (change-game-state resigned))))
          % else proceed with move
          (progn
            (let $start (nth 1 $move) ())
            (let $destination (nth 2 $move) ()) % this is (x y) only
            % we need the entire present destination box to pass to the board display
            (let $destination_entire (return_entire_box $destination) ())
            % make the move in atomspace using the individual "square" atoms
            (move_piece $start $destination) % the envision move persists in atomspace "square" atoms until end of routine.
            (println! "Greedy Chess moving...")
            % create atom for the updated complete chess board...
            (let $provisional_board (move_piece_on_board $start $destination_entire $starting_board)
                  (add_atom &self (board-state $provisional_board)))
            % remove the old chess board
            (remove_atom &self (board-state $starting_board))
            % display the new board
            (D)
            % announce CHECK or CHECKMATE of human player if applicable
            % Note: The attemptcheckmate routine will set the checkmate status.
            (match &self (game-state $msg2)
              (if (==  checkmate   $msg2)
                (progn
                            (println! "Checkmate! Greedy Chess wins!!!")
                            (println! "Good game!")
                            (println! "")
                          )
                % else look for check...
                (let $Check (checkking s)
                  (if (== $Check True)
                    (println! "Check!")
                    (println! "Your move.")))))
            % since move complete, delete any individual square atoms which only live for a single move.
            %     "
          )
        )
        (delete-pieces)
      )
    )
  )
)
