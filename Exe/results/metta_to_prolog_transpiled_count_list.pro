
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

%Clause_implementation--  condition_if_check_is_equal_plus
condition_if_check_is_equal_plus( X , Outvar_e3 ):-
  check_is_equal( X , "empty_atom" , Outvx1 ) ,  
  plus_count_list( X , Outvx2 ) ,  
  condition_if( Outvx1 , 0 , Outvx2 , Outvar_e3 ) .
%Subclausex  :  count_list_cdr_atom
count_list_cdr_atom( X , Outvar_e2 ):-
  cdr_atom( X , Outvx1 ) ,  
  count_list( Outvx1 , Outvar_e2 ) .
%Subclausex  :  plus_count_list
plus_count_list( X , Outvar_e2 ):-
  count_list_cdr_atom( X , Outvx1 ) ,  
  plus( Outvx1 , 1 , Outvar_e2 ) .

%end_result count_list
count_list( [] , 0 ):- ! . 


%end_result count_list
count_list( X , Outp0 ):-
  condition_if_check_is_equal_plus( X , Outp0 ).