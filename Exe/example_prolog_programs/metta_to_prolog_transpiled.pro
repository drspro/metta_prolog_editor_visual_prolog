
bigger_than( A , B , "True" ):- A > B , !.

bigger_than( _ , _ , "False" ):-  !.

bigger_than_equal( A , B , "True" ):- A >= B , !.

bigger_than_equal( _ , _ , "False" ):-  !.

smaller_than( A , B , "True" ):- A < B , !.

smaller_than( _ , _ , "False" ):-  !.

smaller_than_equal( A , B , "True" ):- A =< B , !.

smaller_than_equal( _ , _ , "False" ):-  !.

bigger_then( A , B , "True" ):- A > B , !.

bigger_then( _ , _ , "False" ):-  !.

bigger_then_equal( A , B , "True" ):- A >= B , !.

bigger_then_equal( _ , _ , "False" ):-  !.

smaller_then( A , B , "True" ):- A < B , !.

smaller_then( _ , _ , "False" ):-  !.

smaller_then_equal( A , B , "True" ):- A =< B , !.

smaller_then_equal( _ , _ , "False" ):-  !.

dum_db_pred2():-!.

is_not_equal( Vx1 , Vy2 , "True" ):- Vx1 <> Vy2 , !.  

is_not_equal( _ , _ , "False" ):-  !.  

match( _ , _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

match( _ , "True" , _ , "True" ):-  !.

match( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

match_all( _ , Outvx1 , Outvx1 ):- !.

match_all( _ , Invx1 , _ , Invx1 ):-  !.

match_all( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

match_all( _ , _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.

let_star3_1( Outvx2 , _ , Outvx2 ):- ! . 

let_star3_2( _ , Outvx2 , Outvx2 ):- ! . 

groupe5_2( _ , Resu , _ , _ , Resu  ):- ! .

groupe5_3( _ , _ , Resu , _ , Resu  ):- ! .

groupe6_2( _ , Resu , _ , _ , _ , Resu  ):- ! .

groupe7_6( _ , _ , _ , _ , _ , Resu , Resu  ):- ! .

groupe5_cat3_4( _ , _ , Head_new , "empty_atom" , [ Head_new ]  ):- ! .

groupe5_cat3_4( _ , _ , Head_new , Tail_new , [ Head_new | Tail_new ]  ):- ! .

groupe5_4( _ , _ , _ , Tail_new ,  Tail_new   ):- ! .  

groupe9_8( _ , _ , _ , _ , _ , _ , _ , Lisx , Lisx  ):- ! .

cons_atom( "empty_atom" , Lis , Lis  ):- ! . 

cons_atom( Head , "empty_atom" , [ Head ]  ):- ! . 

cons_atom( Head , Lis , [ Head | Lis ]  ):- ! .  

println( P_arg , "True" ):- write( P_arg ), write("\n"). 

% 0 std_pred division arity:3
division( X , W , Q ):- Q = X / W.

% 0 std_pred multiplication arity:3
multiplication( X , W , Q ):- Q = X * W.

% 0 std_pred plus arity:3
plus( X , W , Q ):- Q = X + W.

% 0 std_pred minus arity:3
minus( X , W , Q ):- Q = X - W.

% 0 std_pred db_dummy arity:1
db_dummy( "True" ):- ! . 

% 0 std_pred check_is_equal custom  arity:3
check_is_equal( Var1 , Var2 , "True" ):-
  Var1 = [] ,  
  Var2 = "empty_atom" ,  
  ! .
% 0 std_pred check_is_equal arity:3
check_is_equal( Var1 , Var2 , "True" ):- Var2 = Var1 , !.

% 0 std_pred check_is_equal catch arity:3
check_is_equal( _ , _ , "False" ):- ! . 

% 0 std_pred condition_if arity:4
condition_if( Var1 , Var2 , _ , Outvar_e3 ):-
  Var1 = "True" ,  
  ! , Outvar_e3 = Var2 .
% 0 std_pred condition_if arity:4
condition_if( _ , _ , Outvx2 , Outvar_e3 ):-
  ! , Outvar_e3 = Outvx2 .
% 0 std_pred condition_if arity:4
condition_if( _ , _ , _ , "False" ):- ! . 

% 0 std_pred car_atom arity:2
car_atom( [ Head | _ ]  , Head ):- ! . 

% 0 std_pred cdr_atom arity:2
cdr_atom( [ _ | Lis ]  , Lis ):- ! . 

% 0 std_pred get_all_db_result arity:3
get_all_db_result( Db_area , Lis , Outvx3 ):-
  retract( db_result( Db_area , El ) ) ,  
  ! , get_all_db_result( Db_area , [ El | Lis ]  , Outvx3 ) .
% 0 std_pred get_all_db_result arity:3
get_all_db_result( Db_area , Lisx , Lisx ):- ! . 


% 1 end_result simple_deduction_strength_formula arity:6
simple_deduction_strength_formula( As , Bs , Cs , ABs , BCs , Outp0 ):-
  condition_if_and_condition_if( Cs , As , Bs , ABs , BCs , Outp0 ) ,  
  ! .
% 1 Subclausex  :  plus_multiplication_division arity:5
plus_multiplication_division( ABs , BCs , Bs , Cs , Outvar_e3 ):-
  multiplication( ABs , BCs , Outvx1 ) ,  
  division_multiplication_minus( Bs , ABs , Cs , BCs , Outvx2 ) ,  
  plus( Outvx1 , Outvx2 , Outvar_e3 ) ,  
  ! .
% 1 Subclausex  :  multiplication_minus_minus arity:5
multiplication_minus_minus( ABs , Cs , Bs , BCs , Outvar_e3 ):-
  minus( 1 , ABs , Outvx1 ) ,  
  minus_multiplication( Cs , Bs , BCs , Outvx2 ) ,  
  multiplication( Outvx1 , Outvx2 , Outvar_e3 ) ,  
  ! .
% 1 Subclausex  :  minus_multiplication arity:4
minus_multiplication( Cs , Bs , BCs , Outvar_e2 ):-
  multiplication( Bs , BCs , Outvx1 ) ,  
  minus( 1 , ABs , Outvx1 , Outvar_e2 ) ,  
  ! .
% 1 Subclausex  :  minus_multiplication arity:4
minus_multiplication( Cs , Bs , BCs , Outvar_e2 ):-
  multiplication( Bs , BCs , Outvx1 ) ,  
  minus( 1 , ABs , Outvx1 , Outvar_e2 ) ,  
  ! .
% 1 Subclausex  :  division_multiplication_minus arity:5
division_multiplication_minus( Bs , ABs , Cs , BCs , Outvar_e3 ):-
  multiplication_minus_minus( ABs , Cs , Bs , BCs , Outvx1 ) ,  
  minus( 1 , Bs , Outvx2 ) ,  
  division( Outvx1 , Outvx2 , Outvar_e3 ) ,  
  ! .
% 1 Subclausex  :  part1  condition_if_smaller_than_plus arity:5
condition_if_smaller_than_plus( Cs , Bs , ABs , BCs , Cs ):-
  smaller_than( 0.99 , Bs , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
% 1 Subclausex  :  part2  condition_if_smaller_than_plus arity:5
condition_if_smaller_than_plus( Cs , Bs , ABs , BCs , Outvar_e3 ):-
  plus_multiplication_division( ABs , BCs , Bs , Cs , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
% 1 Clause_implementation--  part1  condition_if_and_condition_if arity:6
condition_if_and_condition_if( Cs , As , Bs , ABs , BCs , Outvx2 ):-
  and_conditional_probability_consistency_conditional_probability_consistency( As , Bs , ABs , Cs , BCs , Outvx1 ) ,  
  Outvx1 = "True" ,  
  ! .
% 1 Clause_implementation--  part2  condition_if_and_condition_if arity:6
condition_if_and_condition_if( Cs , As , Bs , ABs , BCs , Outvar_e3 ):-
  condition_if_smaller_than_plus( Cs , Bs , ABs , BCs , Outvx2 ) ,  
  Outvar_e3 = Outvx2 ,  
  ! .
% 1 Subclausex  :  and_conditional_probability_consistency_conditional_probability_consistency arity:6
and_conditional_probability_consistency_conditional_probability_consistency( As , Bs , ABs , Cs , BCs , Outvar_e3 ):-
  conditional_probability_consistency( As , Bs , ABs , Outvx1 ) ,  
  conditional_probability_consistency( Bs , Cs , BCs , Outvx2 ) ,  
  and( Outvx1 , Outvx2 , Outvar_e3 ) ,  
  ! .