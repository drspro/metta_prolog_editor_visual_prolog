
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

%Subclausex  :  check_is_equal_car_atom
check_is_equal_car_atom( Sym , List , Outvar_e2 ):-
  car_atom( List , Outvx1 ) ,  
  check_is_equal( Outvx1 , Sym , Outvar_e2 ) .
%Clause_implementation--  condition_if_check_is_equal_condition_if
condition_if_check_is_equal_condition_if( List , Sym , Outvar_e3 ):-
  check_is_equal( List , "empty_atom" , Outvx1 ) ,  
  condition_if_check_is_equal_contains_symbol( Sym , List , Outvx2 ) ,  
  condition_if( Outvx1 , "False" , Outvx2 , Outvar_e3 ) .
%Subclausex  :  condition_if_check_is_equal_contains_symbol
condition_if_check_is_equal_contains_symbol( Sym , List , Outvar_e3 ):-
  check_is_equal_car_atom( Sym , List , Outvx1 ) ,  
  contains_symbol_cdr_atom( Sym , List , Outvx2 ) ,  
  condition_if( Outvx1 , "True" , Outvx2 , Outvar_e3 ) .
%Subclausex  :  contains_symbol_cdr_atom
contains_symbol_cdr_atom( Sym , List , Outvar_e2 ):-
  cdr_atom( List , Outvx1 ) ,  
  contains_symbol( Outvx1 , Sym , Outvar_e2 ) .

%end_result contains_symbol
contains_symbol( List , Sym , Outp0 ):-
  condition_if_check_is_equal_condition_if( List , Sym , Outp0 ).