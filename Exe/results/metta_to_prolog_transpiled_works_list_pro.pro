
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

match_all( _ , Invx1 , _ , Invx1 ):-  !.

match_all( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

% default match_all5 for first implementations 

match_all( _ , _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

komma15( _ , Vx1 , Vx1 ):- !.   

komma16( _ , Vx1 , Vx1 ):- !.   

groupe( _ , _ , Head_new , "empty_atom" , [ Head_new ]  ):- ! .

groupe( _ , _ , Head_new , Tail_new , [ Head_new | Tail_new ]  ):- ! .

cons_atom( Head , "empty_atom" , [ Head ]  ):- ! . 

cons_atom( Head , Lis , [ Head | Lis ]  ):- ! .  



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
get_all_db_result( Lis , Outvx3 ):-
  retract( db_result( El ) ) ,  
  ! , get_all_db_result( [ El | Lis ]  , Outvx3 ) .
%std_pred get_all_db_result
get_all_db_result( Lisx , Lisx ):- ! . 

%std_pred let_star
let_star( _ , Outvx2 , Outvx2 ):- ! . 

%Subclausex  :  check_is_equal_car_atom
check_is_equal_car_atom( Sym , List , Outvar_e2 ):-
  car_atom( List , Outvx1 ) ,  
  check_is_equal( Outvx1 , Sym , Outvar_e2 ) ,  
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
%Clause_implementation--  part1  condition_if_check_is_equal_let_star
condition_if_check_is_equal_let_star( Expr , Nums_list , "empty_atom" ):-
  check_is_equal( Expr , "empty_atom" , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
%Clause_implementation--  part2  condition_if_check_is_equal_let_star
condition_if_check_is_equal_let_star( Expr , Nums_list , Outvar_e3 ):-
  let_star_groupe_cons_atom( Expr , Head , Tail , Nums_list , Head_new , Tail_new , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
%Clause_implementation--  condition_if_contains_symbol
condition_if_contains_symbol( Sym , List , Outvar_e2 ):-
  contains_symbol( List , Sym , Outvx1 ) ,  
  condition_if( Outvx1 , 0 , Sym , Outvar_e2 ) ,  
  ! .
%Subclausex  :  contains_symbol_cdr_atom
contains_symbol_cdr_atom( Sym , List , Outvar_e2 ):-
  cdr_atom( List , Outvx1 ) ,  
  contains_symbol( Outvx1 , Sym , Outvar_e2 ) ,  
  ! .
%Subclausex  :  groupe_car_atom_cdr_atom_elem_is_not_in_list_nums_are_not_in_list
groupe_car_atom_cdr_atom_elem_is_not_in_list_nums_are_not_in_list( Expr , Head , Tail , Nums_list , Head_new , Tail_new , Outvar_e5 ):-
  car_atom( Expr , Head ) ,  
  cdr_atom( Expr , Tail ) ,  
  elem_is_not_in_list( Nums_list , Head , Head_new ) ,  
  nums_are_not_in_list( Tail , Nums_list , Tail_new ) ,  
  groupe( Head , Tail , Head_new , Tail_new , Outvar_e5 ) ,  
  ! .
%Subclausex  :  let_star_groupe_cons_atom
let_star_groupe_cons_atom( Expr , Head , Tail , Nums_list , Head_new , Tail_new , Outvar_e3 ):-
  groupe_car_atom_cdr_atom_elem_is_not_in_list_nums_are_not_in_list( Expr , Head , Tail , Nums_list , Head_new , Tail_new , Outvx1 ) ,  
  cons_atom( Head_new , Tail_new , Outvx2 ) ,  
  let_star( Outvx1 , Outvx2 , Outvar_e3 ) ,  
  ! .

%end_result nums_are_not_in_list
nums_are_not_in_list( Expr , Nums_list , Outp0 ):-
  condition_if_check_is_equal_let_star( Expr , Nums_list , Outp0 ) ,  
  ! .

%end_result elem_is_not_in_list
elem_is_not_in_list( List , Sym , Outp0 ):-
  condition_if_contains_symbol( Sym , List , Outp0 ) ,  
  ! .

%end_result contains_symbol
contains_symbol( List , Sym , Outp0 ):-
  condition_if_check_is_equal_condition_if( List , Sym , Outp0 ) ,  
  ! .