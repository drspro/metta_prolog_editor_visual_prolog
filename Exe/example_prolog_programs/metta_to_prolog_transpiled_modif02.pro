
bigger_than( A , B , "True" ):- A > B , !.

bigger_than( _ , _ , "False" ):-  !.

dum_db_pred2():-!.

is_not_equal( Vx1 , Vy2 , "True" ):- Vx1 <> Vy2 , !.  

is_not_equal( _ , _ , "False" ):-  !.  

% match5 for first implementations 

match( _ , _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

% new match4 apparantly needed 

match( _ , "True" , _ , "True" ):-  !.

% default match4 for first implementations 

match( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

% default match_all4 for first implementations 

%  ****
match_all3( _ , Outvx1 , Outvx1 ):- !.

match_all( _ , Invx1 , _ , Invx1 ):-  !.

match_all( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

% default match_all5 for first implementations 

match_all( _ , _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

komma15( _ , Vx1 , Vx1 ):- !.   

komma16( _ , Vx1 , Vx1 ):- !.   

% ****
komma102( _ , Vx1 , Vx1 ):- !.   

% we need this one for std functions until now 

let_star0( _ , Outvx2 , Outvx2 ):- ! . 

% we need this one for remove duplicates function 

let_star2( Outvx2 , _ , Outvx2 ):- ! . 

% we need this one for remove duplicates function 

let_star3( Outvx2 , _ , Outvx2 ):- ! . 

% we need this one for remove duplicates function 

let_star4( Outvx2 , _ , Outvx2 ):- ! . 

% we need this one for std functions until now 

let_star( _ , Outvx2 , Outvx2 ):- ! . 

groupe9( _ , _ , _ , _ , _ , _ , _ , Lisx , Lisx  ):- ! .

groupe0( _ , _ , Head_new , "empty_atom" , [ Head_new ]  ):- ! .

groupe0( _ , _ , Head_new , Tail_new , [ Head_new | Tail_new ]  ):- ! .

% we need this one for remove duplicates function 

groupe2( _ , _ , _ , Tail_new ,  Tail_new   ):- ! .  

% we need this one for remove duplicates function 

groupe3( _ , _ , _ , Tail_new ,  Tail_new   ):- ! .  

groupe( _ , _ , Head_new , "empty_atom" , [ Head_new ]  ):- ! .

groupe( _ , _ , Head_new , Tail_new , [ Head_new | Tail_new ]  ):- ! .

% needed to be able to filter lists
cons_atom( "empty_atom" , Lis , Lis  ):- ! . 

cons_atom( Head , "empty_atom" , [ Head ]  ):- ! . 

cons_atom( Head , Lis , [ Head | Lis ]  ):- ! .  



% printing predicate 
println( P_arg , "True" ):- write( P_arg ), write("\n"). 

%std_pred division
division( X , W , Q ):- Q = X / W.

%std_pred plus
plus( X , W , Q ):- Q = X + W.

%std_pred db_dummy
db_dummy( "True" ):- ! . 

%std_pred check_is_equal custom 
check_is_equal( Var1 , Var2 , "True" ):-
  Var1 = [] ,  
  Var2 = "empty_atom" ,  
  ! .
%std_pred check_is_equal
check_is_equal( Var1 , Var2 , "True" ):- Var2 = Var1 , !.

%std_pred check_is_equal catch
check_is_equal( _ , _ , "False" ):- ! . 

%std_pred condition_if
condition_if( Var1 , Var2 , _ , Outvar_e3 ):-
  Var1 = "True" ,  
  ! , Outvar_e3 = Var2 .
%std_pred condition_if
condition_if( _ , _ , Outvx2 , Outvar_e3 ):-
  ! , Outvar_e3 = Outvx2 .
%std_pred condition_if
condition_if( _ , _ , _ , "False" ):- ! . 

%std_pred car_atom
car_atom( [ Head | _ ]  , Head ):- ! . 

%std_pred cdr_atom
cdr_atom( [ _ | Lis ]  , Lis ):- ! . 

%std_pred get_all_db_result
get_all_db_result( Dbarea, Lis , Outvx3 ):-
  retract( db_result( Dbarea,  El ) ) ,  
  ! , get_all_db_result( Dbarea, [ El | Lis ]  , Outvx3 ) .
%std_pred get_all_db_result
get_all_db_result( _, Lisx , Lisx ):- ! . 

sudoku_puzzle_state( 1 , 1 , 1 , 5 ). 

sudoku_puzzle_state( 1 , 2 , 1 , 3 ). 

sudoku_puzzle_state( 1 , 3 , 1 , 0 ). 

sudoku_puzzle_state( 1 , 4 , 2 , 0 ). 

sudoku_puzzle_state( 1 , 5 , 2 , 7 ). 

sudoku_puzzle_state( 1 , 6 , 2 , 0 ). 

sudoku_puzzle_state( 1 , 7 , 3 , 0 ). 

sudoku_puzzle_state( 1 , 8 , 3 , 0 ). 

sudoku_puzzle_state( 1 , 9 , 3 , 0 ). 

sudoku_puzzle_state( 2 , 1 , 1 , 6 ). 

sudoku_puzzle_state( 2 , 2 , 1 , 0 ). 

sudoku_puzzle_state( 2 , 3 , 1 , 0 ). 

sudoku_puzzle_state( 2 , 4 , 2 , 1 ). 

sudoku_puzzle_state( 2 , 5 , 2 , 9 ). 

sudoku_puzzle_state( 2 , 6 , 2 , 5 ). 

sudoku_puzzle_state( 2 , 7 , 3 , 0 ). 

sudoku_puzzle_state( 2 , 8 , 3 , 0 ). 

sudoku_puzzle_state( 2 , 9 , 3 , 0 ). 

sudoku_puzzle_state( 3 , 1 , 1 , 0 ). 

sudoku_puzzle_state( 3 , 2 , 1 , 9 ). 

sudoku_puzzle_state( 3 , 3 , 1 , 8 ). 

sudoku_puzzle_state( 3 , 4 , 2 , 0 ). 

sudoku_puzzle_state( 3 , 5 , 2 , 0 ). 

sudoku_puzzle_state( 3 , 6 , 2 , 0 ). 

sudoku_puzzle_state( 3 , 7 , 3 , 0 ). 

sudoku_puzzle_state( 3 , 8 , 3 , 6 ). 

sudoku_puzzle_state( 3 , 9 , 3 , 0 ). 

sudoku_puzzle_state( 4 , 1 , 4 , 8 ). 

sudoku_puzzle_state( 4 , 2 , 4 , 0 ). 

sudoku_puzzle_state( 4 , 3 , 4 , 0 ). 

sudoku_puzzle_state( 4 , 4 , 5 , 0 ). 

sudoku_puzzle_state( 4 , 5 , 5 , 6 ). 

sudoku_puzzle_state( 4 , 6 , 5 , 0 ). 

sudoku_puzzle_state( 4 , 7 , 6 , 0 ). 

sudoku_puzzle_state( 4 , 8 , 6 , 0 ). 

sudoku_puzzle_state( 4 , 9 , 6 , 3 ). 

sudoku_puzzle_state( 5 , 1 , 4 , 4 ). 

sudoku_puzzle_state( 5 , 2 , 4 , 0 ). 

sudoku_puzzle_state( 5 , 3 , 4 , 0 ). 

sudoku_puzzle_state( 5 , 4 , 5 , 8 ). 

sudoku_puzzle_state( 5 , 5 , 5 , 0 ). 

sudoku_puzzle_state( 5 , 6 , 5 , 3 ). 

sudoku_puzzle_state( 5 , 7 , 6 , 0 ). 

sudoku_puzzle_state( 5 , 8 , 6 , 0 ). 

sudoku_puzzle_state( 5 , 9 , 6 , 1 ). 

sudoku_puzzle_state( 6 , 1 , 4 , 7 ). 

sudoku_puzzle_state( 6 , 2 , 4 , 0 ). 

sudoku_puzzle_state( 6 , 3 , 4 , 0 ). 

sudoku_puzzle_state( 6 , 4 , 5 , 0 ). 

sudoku_puzzle_state( 6 , 5 , 5 , 2 ). 

sudoku_puzzle_state( 6 , 6 , 5 , 0 ). 

sudoku_puzzle_state( 6 , 7 , 6 , 0 ). 

sudoku_puzzle_state( 6 , 8 , 6 , 0 ). 

sudoku_puzzle_state( 6 , 9 , 6 , 6 ). 

sudoku_puzzle_state( 7 , 1 , 7 , 0 ). 

sudoku_puzzle_state( 7 , 2 , 7 , 6 ). 

sudoku_puzzle_state( 7 , 3 , 7 , 0 ). 

sudoku_puzzle_state( 7 , 4 , 8 , 0 ). 

sudoku_puzzle_state( 7 , 5 , 8 , 0 ). 

sudoku_puzzle_state( 7 , 6 , 8 , 0 ). 

sudoku_puzzle_state( 7 , 7 , 9 , 2 ). 

sudoku_puzzle_state( 7 , 8 , 9 , 8 ). 

sudoku_puzzle_state( 7 , 9 , 9 , 0 ). 

sudoku_puzzle_state( 8 , 1 , 7 , 0 ). 

sudoku_puzzle_state( 8 , 2 , 7 , 0 ). 

sudoku_puzzle_state( 8 , 3 , 7 , 0 ). 

sudoku_puzzle_state( 8 , 4 , 8 , 4 ). 

sudoku_puzzle_state( 8 , 5 , 8 , 1 ). 

sudoku_puzzle_state( 8 , 6 , 8 , 9 ). 

sudoku_puzzle_state( 8 , 7 , 9 , 0 ). 

sudoku_puzzle_state( 8 , 8 , 9 , 0 ). 

sudoku_puzzle_state( 8 , 9 , 9 , 5 ). 

sudoku_puzzle_state( 9 , 1 , 7 , 0 ). 

sudoku_puzzle_state( 9 , 2 , 7 , 0 ). 

sudoku_puzzle_state( 9 , 3 , 7 , 0 ). 

sudoku_puzzle_state( 9 , 4 , 8 , 0 ). 

sudoku_puzzle_state( 9 , 5 , 8 , 8 ). 

sudoku_puzzle_state( 9 , 6 , 8 , 0 ). 

sudoku_puzzle_state( 9 , 7 , 9 , 0 ). 

sudoku_puzzle_state( 9 , 8 , 9 , 7 ). 

sudoku_puzzle_state( 9 , 9 , 9 , 9 ). 

sudoku_number( 1 ). 

sudoku_number( 2 ). 

sudoku_number( 3 ). 

sudoku_number( 4 ). 

sudoku_number( 5 ). 

sudoku_number( 6 ). 

sudoku_number( 7 ). 

sudoku_number( 8 ). 

sudoku_number( 9 ). 

%Subclausex  :  part1  add_atom_mem_candidates
add_atom_mem_candidates( R , C , Cands , "True" ):-
  assert( mem_candidates( R , C , Cands ) ) ,  
  ! .
%Subclausex  :  check_is_equal_car_atom
check_is_equal_car_atom( Sym , List , Outvar_e2 ):-
  car_atom( List , Outvx1 ) ,  
  check_is_equal( Outvx1 , Sym , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  condition_if_bigger_than
condition_if_bigger_than( Va , Outvar_e2 ):-
  bigger_than( Va , 0 , Outvx1 ) ,  
  condition_if( Outvx1 , Va , "empty_atom" , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  part1  condition_if_check_is_equal_condition_if
condition_if_check_is_equal_condition_if( List , Sym , "False" ):-
  check_is_equal( List , "empty_atom" , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
%Clause_implementation--  part2  condition_if_check_is_equal_condition_if
condition_if_check_is_equal_condition_if( List , Sym , Outvar_e3 ):-
  condition_if_check_is_equal_contains_symbol( Sym , List , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
%Subclausex  :  part1  condition_if_check_is_equal_contains_symbol
condition_if_check_is_equal_contains_symbol( Sym , List , "True" ):-
  check_is_equal_car_atom( Sym , List , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
%Subclausex  :  part2  condition_if_check_is_equal_contains_symbol
condition_if_check_is_equal_contains_symbol( Sym , List , Outvar_e3 ):-
  contains_symbol_cdr_atom( Sym , List , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
%Clause_implementation--  part1  condition_if_check_is_equal_let_star0
condition_if_check_is_equal_let_star0( Expr , "empty_atom" ):-
  check_is_equal( Expr , "empty_atom" , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
%Clause_implementation--  part2  condition_if_check_is_equal_let_star0
condition_if_check_is_equal_let_star0( Expr , Outvar_e3 ):-
  let_star0_groupe0_cons_atom( Expr , Head , Tail , Head_new , Tail_new , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
%Clause_implementation--  part1  condition_if_check_is_equal_let_star2
condition_if_check_is_equal_let_star2( Lis_els1 , Lis_els2 , Lis_els1 ):-
  check_is_equal( Lis_els2 , "empty_atom" , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
%Clause_implementation--  part2  condition_if_check_is_equal_let_star2
condition_if_check_is_equal_let_star2( Lis_els1 , Lis_els2 , Outvar_e3 ):-
  let_star2_groupe2( List_nettx , Lis_els2 , Head , Tail , Lis_els1 , Tail_new , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
%Clause_implementation--  part1  condition_if_check_is_equal_let_star3
condition_if_check_is_equal_let_star3( List_netto , Expr , List_netto ):-
  check_is_equal( Expr , "empty_atom" , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
%Clause_implementation--  part2  condition_if_check_is_equal_let_star3
condition_if_check_is_equal_let_star3( List_netto , Expr , Outvar_e3 ):-
  let_star3_groupe3( List_nettx , Expr , Head , Tail , List_netto , Tail_new , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
%Clause_implementation--  part1  condition_if_contains_symbol_cons_atom
condition_if_contains_symbol_cons_atom( List , Sym , List ):-
  contains_symbol( List , Sym , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
%Clause_implementation--  part2  condition_if_contains_symbol_cons_atom
condition_if_contains_symbol_cons_atom( List , Sym , Outvar_e3 ):-
  cons_atom( Sym , List , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
%Subclausex  :  contains_symbol_cdr_atom
contains_symbol_cdr_atom( Sym , List , Outvar_e2 ):-
  cdr_atom( List , Outvx1 ) ,  
  contains_symbol( Outvx1 , Sym , Outvar_e2 ) ,  
  ! .
%Subclausex  :  groupe0_car_atom_cdr_atom_filter_elem_filter_elems
groupe0_car_atom_cdr_atom_filter_elem_filter_elems( Expr , Head , Tail , Head_new , Tail_new , Outvar_e5 ):-
  car_atom( Expr , Head ) ,  
  cdr_atom( Expr , Tail ) ,  
  filter_elem( Head , Head_new ) ,  
  filter_elems( Tail , Tail_new ) ,  
  groupe0( Head , Tail , Head_new , Tail_new , Outvar_e5 ) ,  
  ! .
%Subclausex  :  groupe2_car_atom_cdr_atom_cons_atom_append_lis
groupe2_car_atom_cdr_atom_cons_atom_append_lis( Lis_els2 , Head , Tail , Lis_els1 , List_nettx , Tail_new , Outvar_e5 ):-
  car_atom( Lis_els2 , Head ) ,  
  cdr_atom( Lis_els2 , Tail ) ,  
  cons_atom( Head , Lis_els1 , List_nettx ) ,  
  append_lis( Tail , List_nettx , Tail_new ) ,  
  groupe2( Head , Tail , List_nettx , Tail_new , Outvar_e5 ) ,  
  ! .
%Subclausex  :  groupe3_car_atom_cdr_atom_add_elem_if_not_in_listx_remove_dups
groupe3_car_atom_cdr_atom_add_elem_if_not_in_listx_remove_dups( Expr , Head , Tail , List_netto , List_nettx , Tail_new , Outvar_e5 ):-
  car_atom( Expr , Head ) ,  
  cdr_atom( Expr , Tail ) ,  
  add_elem_if_not_in_listx( List_netto , Head , List_nettx ) ,  
  remove_dups( List_nettx , Tail , Tail_new ) ,  
  groupe3( Head , Tail , List_nettx , Tail_new , Outvar_e5 ) ,  
  ! .
%Subclausex  :  groupe9_get_row_state_get_col_state_get_quad_get_quad_state_append_lis_append_lis_filter_elems_remove_dups
groupe9_get_row_state_get_col_state_get_quad_get_quad_state_append_lis_append_lis_filter_elems_remove_dups( R , Row_sta , C , Col_sta , Q , Q_sta , App1 , App2 , Lis3 , Lis4 , Outvar_e9 ):-
  get_row_state( R , Row_sta ) ,  
  get_col_state( C , Col_sta ) ,  
  get_quad( R , C , Q ) ,  
  get_quad_state( Q , Q_sta ) ,  
  append_lis( Row_sta , Col_sta , App1 ) ,  
  append_lis( App1 , Q_sta , App2 ) ,  
  filter_elems( App2 , Lis3 ) ,  
  remove_dups( "empty_atom" , Lis3 , Lis4 ) ,  
  groupe9( Row_sta , Col_sta , Q , Q_sta , App1 , App2 , Lis3 , Lis4 , Outvar_e9 ) ,  
  ! .
%Subclausex  :  komma102_sudoku_puzzle_state102_cell_candidates
komma102_sudoku_puzzle_state102_cell_candidates( R , C , Q , State , Outvar_e2 ):-
  db_dummy( Outv_not_db ) ,  
  sudoku_puzzle_state102_db102( R , C , Q , State , Outvx1 ) ,  
  komma102( Outv_not_db , Outvx1 , Outvar_e2 ) ,  
  ! .
  
  
  
  
%Subclausex  :  let_star0_groupe0_cons_atom
let_star0_groupe0_cons_atom( Expr , Head , Tail , Head_new , Tail_new , Outvar_e3 ):-
  groupe0_car_atom_cdr_atom_filter_elem_filter_elems( Expr , Head , Tail , Head_new , Tail_new , Outvx1 ) ,  
  cons_atom( Head_new , Tail_new , Outvx2 ) ,  
  let_star0( Outvx1 , Outvx2 , Outvar_e3 ) ,  
  ! .
%Subclausex  :  let_star2_groupe2
let_star2_groupe2( List_nettx , Lis_els2 , Head , Tail , Lis_els1 , Tail_new , Outvar_e2 ):-
  groupe2_car_atom_cdr_atom_cons_atom_append_lis( Lis_els2 , Head , Tail , Lis_els1 , List_nettx , Tail_new , Outvx1 ) ,  
  let_star2( Outvx1 , List_nettx , Outvar_e2 ) ,  
  ! .
%Subclausex  :  let_star3_groupe3
let_star3_groupe3( List_nettx , Expr , Head , Tail , List_netto , Tail_new , Outvar_e2 ):-
  groupe3_car_atom_cdr_atom_add_elem_if_not_in_listx_remove_dups( Expr , Head , Tail , List_netto , List_nettx , Tail_new , Outvx1 ) ,  
  let_star3( Outvx1 , List_nettx , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  let_star4_groupe9
let_star4_groupe9( R , C , Outvar_e2 ):-
  groupe9_get_row_state_get_col_state_get_quad_get_quad_state_append_lis_append_lis_filter_elems_remove_dups( R , Row_sta , C , Col_sta , Q , Q_sta , App1 , App2 , Lis3 , Lis4 , Outvx1 ) ,  
  let_star4( Outvx1 , Lis4 , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  match_all_komma102_add_atom
match_all_komma102_add_atom( Outvar_e2 ):-
  komma102_sudoku_puzzle_state102_cell_candidates( R , C , Q , State , Outvx1 ) ,  
% ****
  match_all3( "&board" , Outvx1 , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  match_all_sudoku_puzzle_state93
match_all_sudoku_puzzle_state93( R , Outvar_e2 ):-
  db_dummy( Outv_not_db ) ,  
  sudoku_puzzle_state93_db93( R , C , Q , State , Outvx1 ) ,  
  match_all( "&rows" , Outv_not_db , State , Outvx1 , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  match_all_sudoku_puzzle_state94
match_all_sudoku_puzzle_state94( C , Outvar_e2 ):-
  db_dummy( Outv_not_db ) ,  
  sudoku_puzzle_state94_db94( R , C , Q , State , Outvx1 ) ,  
  match_all( "&cols" , Outv_not_db , State , Outvx1 , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  match_all_sudoku_puzzle_state95
match_all_sudoku_puzzle_state95( Q , Outvar_e2 ):-
  db_dummy( Outv_not_db ) ,  
  sudoku_puzzle_state95_db95( R , C , Q , State , Outvx1 ) ,  
  match_all( "&quads" , Outv_not_db , State , Outvx1 , Outvar_e2 ) ,  
  ! .
%Clause_implementation--  match_sudoku_puzzle_state92
match_sudoku_puzzle_state92( Q , R , C , State , Outvar_e2 ):-
  db_dummy( Outv_not_db ) ,  
  sudoku_puzzle_state92_db92( R , C , Q , State , Outvx1 ) ,  
  match( "&quads" , Outv_not_db , Q , Outvx1 , Outvar_e2 ) ,  
  ! .

% ****    Modifications to make it work
% TODO make this auto , plus make that println doesnt give an output var
%  and prefreably  is not used in the  the pred names , be it an abrev or not at all


%db create std-p  sudoku_puzzle_state102_db102
sudoku_puzzle_state102_db102( R , C , Q , State , Outvx3 ):-
  sudoku_puzzle_state( R , C , Q , State ) ,  
% ****
  cell_candidates( R , C , Cands ) ,  
% ***
  println( Cands , _ ) ,
% *****
  add_atom_mem_candidates( R , C , Cands , Outvx3 ) ,  
  asserta( db_result( "&board" , Outvx3 ) ) ,  
  fail .

  
  
%db create std-qa match_all catch sudoku_puzzle_state102_db102
sudoku_puzzle_state102_db102( R , C , Q , State , Outv_db_result ):-
  get_all_db_result( "&board" , [] , Outv_db_result ) ,  
  ! .
%db create std-z  sudoku_puzzle_state92_db92
sudoku_puzzle_state92_db92( R , C , Q , State , Q ):-
  sudoku_puzzle_state( R , C , Q , State ) ,  
  dum_db_pred2(  ) .
%db create std-z  sudoku_puzzle_state93_db93
sudoku_puzzle_state93_db93( R , C , Q , State , State ):-
  sudoku_puzzle_state( R , C , Q , State ) ,  
  dum_db_pred2(  ) ,  
  asserta( db_result( "&rows" , State ) ) ,  
  fail .
%db create std-qa match_all catch sudoku_puzzle_state93_db93
sudoku_puzzle_state93_db93( R , C , Q , State , Outv_db_result ):-
  get_all_db_result( "&rows" , [] , Outv_db_result ) ,  
  ! .
%db create std-z  sudoku_puzzle_state94_db94
sudoku_puzzle_state94_db94( R , C , Q , State , State ):-
  sudoku_puzzle_state( R , C , Q , State ) ,  
  dum_db_pred2(  ) ,  
  asserta( db_result( "&cols" , State ) ) ,  
  fail .
%db create std-qa match_all catch sudoku_puzzle_state94_db94
sudoku_puzzle_state94_db94( R , C , Q , State , Outv_db_result ):-
  get_all_db_result( "&cols" , [] , Outv_db_result ) ,  
  ! .
%db create std-z  sudoku_puzzle_state95_db95
sudoku_puzzle_state95_db95( R , C , Q , State , State ):-
  sudoku_puzzle_state( R , C , Q , State ) ,  
  dum_db_pred2(  ) ,  
  asserta( db_result( "&quads" , State ) ) ,  
  fail .
%db create std-qa match_all catch sudoku_puzzle_state95_db95
sudoku_puzzle_state95_db95( R , C , Q , State , Outv_db_result ):-
  get_all_db_result( "&quads" , [] , Outv_db_result ) ,  
  ! .

%end_result remove_dups
remove_dups( List_netto , Expr , Outp0 ):-
  condition_if_check_is_equal_let_star3( List_netto , Expr , Outp0 ) ,  
  ! .

%end_result get_row_state
get_row_state( R , Outp0 ):-
  match_all_sudoku_puzzle_state93( R , Outp0 ) ,  
  ! .

%end_result get_quad_state
get_quad_state( Q , Outp0 ):-
  match_all_sudoku_puzzle_state95( Q , Outp0 ) ,  
  ! .

%end_result get_quad
get_quad( R , C , Outp0 ):-
  match_sudoku_puzzle_state92( Q , R , C , State , Outp0 ) ,  
  ! .

%end_result get_col_state
get_col_state( C , Outp0 ):-
  match_all_sudoku_puzzle_state94( C , Outp0 ) ,  
  ! .

%end_result get_board_state
get_board_state( Outp0 ):-
  match_all_komma102_add_atom( Outp0 ) ,  
  ! .

%end_result filter_elems
filter_elems( Expr , Outp0 ):-
  condition_if_check_is_equal_let_star0( Expr , Outp0 ) ,  
  ! .

%end_result filter_elem
filter_elem( Va , Outp0 ):-
  condition_if_bigger_than( Va , Outp0 ) ,  
  ! .

%end_result contains_symbol
contains_symbol( List , Sym , Outp0 ):-
  condition_if_check_is_equal_condition_if( List , Sym , Outp0 ) ,  
  ! .

%end_result cell_candidates
cell_candidates( R , C , Outp0 ):-
  let_star4_groupe9( R , C , Outp0 ) ,  
  ! .

%end_result append_lis
append_lis( Lis_els2 , Lis_els1 , Outp0 ):-
  condition_if_check_is_equal_let_star2( Lis_els1 , Lis_els2 , Outp0 ) ,  
  ! .

%end_result add_elem_if_not_in_listx
add_elem_if_not_in_listx( List , Sym , Outp0 ):-
  condition_if_contains_symbol_cons_atom( List , Sym , Outp0 ) ,  
  ! .