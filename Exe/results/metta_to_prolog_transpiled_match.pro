
%std_pred division
division( X , W , Q ):- Q = X / W.

%std_pred plus
plus( X , W , Q ):- Q = X + W.

%std_pred db_dummy
db_dummy( "True" ):- ! . 

%std_pred match variant 01
match( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

%std_pred match variant 02
match_all( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

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

%std_pred cons_atom
cons_atom( Head , Lis , [ Head | Lis ]  ):- ! . 

%db create std  cell_candidate_db
cell_candidate_db( R , C , Num , condition_if( sub( check_is_equal( sub( exist_as_false_candidate( $levelx , $r , $c , $num ) ) ,  
  "True" ) ) ,  
  "empty" , Num ) ):-
  cell_candidate( R , C , Num ) ,  
  ! .
%Subclausex  :  check_is_equal_exist_as_false_candidate
check_is_equal_exist_as_false_candidate( Levelx , R , C , Num , Outvar_e2 ):-
  exist_as_false_candidate( Levelx , R , C , Num , Outvx1 ) ,  
  check_is_equal( Outvx1 , "True" , Outvar_e2 ) .
%Subclausex  :  condition_if_check_is_equal
condition_if_check_is_equal( Num , Levelx , R , C , Outvar_e2 ):-
  check_is_equal_exist_as_false_candidate( Levelx , R , C , Num , Outvx1 ) ,  
  condition_if( Outvx1 , "empty" , Num , Outvar_e2 ) .
%Clause_implementation--  match_cell_candidate_condition_if
match_cell_candidate_condition_if( R , C , Num , Levelx , Outvar_e3 ):-
  db_dummy( Outv_not_db ) ,  
  cell_candidate_db( R , C , Num , Outvx1 ) ,  
  condition_if_check_is_equal( Num , Levelx , R , C , Outvx2 ) ,  
  match( "&self" , Outv_not_db , Outvx1 , Outvx2 , Outvar_e3 ) .

%end_result get_tmp_candidates_not_false
get_tmp_candidates_not_false( R , C , Levelx , Outp0 ):-
  match_cell_candidate_condition_if( R , C , Num , Levelx , Outp0 ).