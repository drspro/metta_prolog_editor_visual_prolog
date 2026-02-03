% Copyright ©

implement metta_tpro
    open core



class facts - positions
cur_position : ( core::positive, core::positive ) determ.


class facts - parse_errors
tok_error : ( string , cursortoq ).


class facts - db_head_subs
head_subs : ( integer, ilist , slist ).


class facts - db_transpile_start
transpile_start : ( expr ) determ.

class facts - db_is_transp
is_transp : ( integer, integer, operator, ilist, atom_list, atom_list ).

class facts - db_is_trpile
is_trpile : ( integer, integer, operator, ilist, atom_list, atom_list ).

class facts - db_varsx
var_concerned : ( string ).

class facts - db_construct
to_be_subconstructed : ( integer , string ).

class facts - db_debo_org_str
debo_org_str : ( integer , string ).

class facts - weqass
lasts_metta_string : ( string ) determ.
last_file : ( string ) determ.

class facts - pred_levels
pred_level : ( integer, integer, expr ).
tel : ( integer ) determ.


class facts - parsedx
term_parsed : ( integer , expr ).

class predicates

parse : ( tokl, expr ) determ( i , o ).
parse_rest : ( tokl , integer ) determ( i , i ).

clean_var : ( string, string ) determ( i, o ).


%---
write_out_metta : ( integer, expr ) determ( i, i ).
write_out_metta2 : ( integer, atom_list ) determ( i, i ).

has_no_sub_nesting2 : ( atom_list , integer, integer ) determ( i, i, o ).

clauses


set_current_position( Pos , Line ):-
  retractall( cur_position( _ , _ ) ),
  assert( cur_position( Pos , Line ) ), !.


class predicates
get_last_string : ( string, string ) determ( i,o).

clauses


get_last_string( _Def, Last ):- lasts_metta_string( Last ),!.
get_last_string( Def, Def ):- !.


class predicates

write_out : ( slist ) determ( i ).


  expect : ( cursortoq, tokl, tokl ) determ anyflow.
  syntax_error : ( string , tokl ) determ(i,i).


class predicates
  s_atom_list : ( tokl , tokl , atom_list ) determ(i,o,o).
  s_atom : ( tokl , tokl , atom ) determ(i,o,o).
  s_operator : ( tokl , tokl , operator ) determ(i,o,o).
  s_expr : ( tokl , tokl , expr ) determ(i,o,o).


clauses

set_file( Fn ):-
  retractall( last_file( _ ) ),
  assert( last_file( Fn ) ).



filter_out_comments( AA, Result ):-
     string5x::searchstring( AA, ";" , Pos ), P2 = Pos - 1, string5x::frontstr( P2, AA, Bg, Res ),
	 string5x::searchstring( Res, "\n" , Pos3 ),
	 P32 = Pos3 - 1,
     string5x::frontstr( P32, Res, _Bg3, Res3 ), !,
	 string5x::concat ( Bg,  Res3, C2 ),
    filter_out_comments( C2, Result ),  !.

filter_out_comments( AA, Result ):-
     string5x::searchstring( AA, "%" , Pos ), P2 = Pos - 1, string5x::frontstr( P2, AA, Bg, Res ),
	 string5x::searchstring( Res, "\n" , Pos3 ),
	 P32 = Pos3 - 1,
     string5x::frontstr( P32, Res, _Bg3, Res3 ), !,
	 string5x::concat ( Bg,  Res3, C2 ),
    filter_out_comments( C2, Result ),  !.


filter_out_comments( Resu, Resu ):- !.




start_parse( Metta_str ) :-    retractall( pred_level( _, _, _ ) ),    retractall( is_transp( _, _, _, _, _, _) ),
    retractall( term_parsed( _, _ ) ),
      OutString0 = Metta_str,    filter_out_comments( OutString0, OutString ),
    retractall( lasts_metta_string( _ ) ),  assert( lasts_metta_string( OutString ) ),
      vpiCommonDialogs::note( "start Parsing0" , OutString ),
    tokenize( OutString , TOKENS ),
      Res = toString( TOKENS ) ,
      vpiCommonDialogs::note("start Parsing", Res ),
    parse_report( TOKENS, TERMX, Rest_tokens ) ,
    assert( term_parsed( 1, Termx ) ),
    parse_rest( Rest_tokens, 2 ),
%    display_results(),
    vpiCommonDialogs::note("Ready Parsing", "" ),
    !.

start_parse( _Metta_str ) :- !.

write_out( [] ):-  ! .
write_out( [ H | Rest ] ):-  !,   file5x::write( H ), write_out( Rest  ).


% write_out( _X ):- !.


  s_atom( [ tk( variabel_tk( Str), Bg , End )    | LL ] , LL , variabel( Str, Bg , End ) ):- !.
  s_atom( [  tk(  number_tk( Rea ), Bg , End )    | LL ] , LL , number(  Rea, Bg , End  ) ):- !.
  s_atom([  tk( name_tk( Str ), Bg , End)  | LL ], LL, namex( Str , Bg , End) ):- !.

% qqq
  s_atom([  tk( lpar_tk,  Bg , _ ) , tk( rpar_tk,  _ , End )  | LL ], LL, empty_atom( Bg , End) ):- !.



  % s_atom(LL,_,_):-syntax_error(atom,LL),fail.

  s_atom( LL1 , LL0 , metta_sub( Expr ) ):-	s_expr( LL1 , LL0 , Expr ) , !.
  s_atom( LL , _ , _ ):- syntax_error( "atom" , LL ) , fail.
%--------
  s_operator( [ tk( equal_tk, Bg, End ) | LL] , LL , equal( Bg, End ) ):-!.
  s_operator( [ tk( dbl_equal_tk, Bg, End ) | LL] , LL , dbl_equal( Bg, End ) ):-!.
%  s_operator( [t( simple_deduction_strength_formula , _ )|LL] , LL , simple_deduction_strength_formula ):-!.
  s_operator( [  tk(  conditional_tk, Bg, End ) | LL] , LL , conditional( Bg, End ) ):-!.
  s_operator( [  tk( conjuction_tk, Bg, End ) | LL] , LL , conjuction( Bg, End ) ):-!.
%  s_operator( [t( conditional_probability_consistency , _ )|LL] , LL , conditional_probability_consistency ):-!.
  s_operator( [  tk( smallerthan_tk, Bg, End ) | LL] , LL , smallerthan( Bg, End ) ):-!.
  s_operator( [  tk( plus_tk, Bg, End ) | LL ] , LL , plus( Bg, End ) ):-!.
  s_operator( [  tk( multiplication_tk, Bg, End ) | LL] , LL , multiplication( Bg, End ) ):-!.
  s_operator( [  tk( division_tk, Bg, End ) | LL ] , LL , division( Bg, End ) ):-!.
  s_operator( [  tk( minus_tk, Bg, End ) | LL ] , LL , minus( Bg, End ) ):-!.
  s_operator( [  tk( name_tk( Str), Bg, End )  | LL ], LL, namex( Str , Bg, End) ):- !.
  s_operator( [  tk(  variabel_tk( Str ), Bg, End )  | LL ], LL, variabel( Str , Bg, End ) ):- !.
  s_operator( LL , _ , _ ):- syntax_error( "operator" , LL ) , fail.
%-----
  s_expr( [ tk( exclamation_tk, Bg , _ ), tk( lpar_tk,  _ , _ ) | LL1 ] , LL0 , exclama_atom_list( Operator , Bg, End , Atom_list ) ):-! ,
	s_operator( LL1 , LL2 , Operator ) ,
	s_atom_list( LL2 , LL3 , Atom_list ) ,
	expect( tk( rpar_tk, _ , End ) , LL3 , LL0 ).

  s_expr( [ tk( lpar_tk, Bg , _ ) | LL1 ] , LL0 , par_atom_list( Operator , Bg, End, Atom_list ) ):-! ,
	s_operator( LL1 , LL2 , Operator ) ,
	s_atom_list( LL2 , LL3 , Atom_list ) ,
	expect( tk( rpar_tk, _ , End ) , LL3 , LL0 ).
  s_expr( LL , _ , _ ):- syntax_error( "expr" , LL ) , fail.
%-----
  s_atom_list( LL1 , LL0 , [ Atom | Atom_list] ):-
	s_atom( LL1 , LL2 , Atom ) , ! ,
	s_atom_list( LL2 , LL0 , Atom_list ).
  s_atom_list( LL , LL , [] ).


clauses

  expect(Tok, [ Tok | L ] , L ).

syntax_errors_nd( Type_str, Toq  ):-
     tok_error( Type_str , Toq  ).


empty_errors():- !,
    retractall( tok_error( _ , _ ) ).



 syntax_error( Type_mess , [ Token | _Tok_lis ] ):-
     Sx = toString(  Token  ),
	 assert( tok_error( Type_mess , Token ) ),
     C3 = string::concat( Type_mess, " " , Sx ) ,

     ! .

 syntax_error( _ , _ ).



domains
  number_of_extra_characters 	= integer.
  number_of_spaces		= integer.

class predicates

is_a_space : (char) determ(i).
is_a_space_s : (string) determ(i).
is_a_space_i : (unsigned16) determ(i).

  scan : ( cursorq, source, tokl ) determ(i,i,o).
  skip_spaces : ( string, string, integer, integer ) determ(i,o,i,o).
  string_tokenq : (string, integer, integer, tok) determ(i,i,i,o).
 get_fronttoken : ( string, string, string) determ( i,o,o).

is_alfa : ( core::unsigned16 ) determ( i ).
is_alfa_string : ( string ) determ( i ).

clauses

is_a_space( ' ' ).
is_a_space( '\t' ).
is_a_space( '\n' ).

is_a_space_i( 32 ).
is_a_space_i( 9 ).
is_a_space_i( 10 ).
is_a_space_i( 13 ).

is_a_space_s( " " ).
is_a_space_s( "\t" ).
is_a_space_s( "\n" ).

is_alfa( V ):- V >= 97, V <= 122 , ! .
is_alfa( V ):- V >= 65, V <= 90 , ! .

is_alfa_string( Str  ):- Str = "", !.
is_alfa_string( Str  ):-
 string::frontChar( Str , Cha , Str2 ),
 Va = string::getCharValue( Cha ),
 is_alfa( Va ), !,
 is_alfa_string( Str2  ).

% add manually 11:24 8-9-2024

get_fronttoken( Source, Bg, Res2 ):-
  string5x::fronttoken( Source, Fronttoken, Rest),   Fronttoken = "\"" , !,
  string5x::searchstring( Rest, "\"", Pos ), P2 = Pos - 1, string5x::frontstr( P2, Rest, Bg, Res ),
  string5x::frontstr( 1, Res, _, Res2 ).


get_fronttoken( Source, Bg3, Rest2 ):-
  string5x::fronttoken( Source, Fronttoken, Rest),   Fronttoken = "$",
  string5x::fronttoken( Rest, Fronttoken2, Rest2), !,
  string5x::concat( "$", Fronttoken2, Bg3 ).


get_fronttoken( Source, "==", Rest3 ):-
  string5x::fronttoken( Source, Fronttoken, Rest),   Fronttoken = "=",
  string5x::fronttoken( Rest, Fronttoken2, Rest3 ),  Fronttoken2 = "=", !.


get_fronttoken( Source , Qw , Rest4 ):-
     string5x::fronttoken( Source , Fronttoken , Rest ) ,
	 is_alfa_string( Fronttoken  ),
	  string5x::fronttoken( Rest , Fronttoken2 , Rest3 ) ,  Fronttoken2 = "-" ,

	   string5x::fronttoken( Rest3 , Fronttoken4 , Rest4 ),
	    is_alfa_string( Fronttoken4  ),
		Qw = string::concat( Fronttoken, "-", Fronttoken4 ),	 ! .

get_fronttoken( Source , Fronttoken , Rest ):-
     string5x::fronttoken( Source , Fronttoken , Rest ) , ! .




get_fronttoken( Source, Bg3, Res2 ):-
  string5x::searchstring( Source, " ", Pos ), Pos > 1 , P2 = Pos - 1,
  string5x::frontstr( P2, Source, Bg3, Res ),
  string5x::frontstr( 1, Res, _, Res2 ), !.

get_fronttoken( Source, Fronttoken, Rest ):- string5x::fronttoken( Source, Fronttoken, Rest), !.


% arrange for the Quote here
scan( Starting_Position, Source, [ tk(Token, Location_Of_Token, Xend_Position ) | Tail ]) :-
	skip_spaces( Source, New_Source, 0, Number_Of_Spaces),
	Location_Of_Token = Starting_Position + Number_Of_Spaces,
	get_fronttoken( New_Source, Fronttoken, Rest ),	!,
	Lower_Case_Fronttoken = Fronttoken,
    string5x::str_len( Fronttoken, Length_Of_Fronttoken),
	string_tokenq( Lower_Case_Fronttoken, Location_Of_Token, Length_Of_Fronttoken, Token),
	Xend_Position = Location_Of_Token + Length_Of_Fronttoken,
    scan( Xend_Position, Rest, Tail ).
  scan( _, _, [] ).
%-----

  skip_spaces( Source, New_Source, NSpaces, Resu_spaces) :-
      string::frontChar(Source, Char, Source1), Va = string::getCharValue(Char),
    Zq = string::charToString( Char ),
     is_a_space_i( Va ),	!,
	 NSpaces2 = NSpaces + 1,
    skip_spaces( Source1, New_Source, NSpaces2 , Resu_spaces ).

  skip_spaces( Source, Source, Resu, Resu):- !.


% Only let uncommented hier the ones that are being used in the
% chosen grammar-file

%  string_tokenq(":", 	colon ) :- !.
%  string_tokenq("=", 	equalsign ) :- !.
  string_tokenq( "=", _ , _ , 	 equal_tk ) :- !.
  string_tokenq( "==", _ , _ , 	 dbl_equal_tk ) :- !.
  % string_tokenq("simple_deduction_strength_formula", 	simple_deduction_strength_formula ) :- !.

  string_tokenq( "if", _ , _, 	 conditional_tk ) :- !.
  string_tokenq( "and", _ , _,	  conjuction_tk ) :- !.

  %string_tokenq("conditional_probability_consistency", 	conditional_probability_consistency ) :- !.
  string_tokenq( "<", _ , _,	 smallerthan_tk ) :- !.
  string_tokenq( "+", _ , _,	 plus_tk ) :- !.
  string_tokenq( "*", _ , _,	 multiplication_tk ) :- !.
  string_tokenq( "!", _ , _,	 exclamation_tk ) :- !.


  string_tokenq( "/", _ , _,	 division_tk ) :- !.
  string_tokenq( "-", _ , _,	 minus_tk ) :- !.

  string_tokenq( "(", _ , _,	 lpar_tk ) :- !.
  string_tokenq( ")",_ , _,	 rpar_tk ) :- !.


% OPTIONAL
%  string_tokenq(";", 	semicolon ) :- !.
%  string_tokenq("?", 	interrogation ) :- !.
%  string_tokenq("\"",	quote ) :- !.
  string_tokenq( Str, _ , _,	 number_tk(Rea )) :- conversion5x::str_real( Str, Rea), !.

  string_tokenq( Str, _ , _,	 variabel_tk(Str ) ) :-
    string5x::frontstr( 1, Str, Bg, _Res ), Bg = "$",	!.

  string_tokenq( Str, _ , _,	 name_tk( Str   ) ) :-  	!.

% DEZE toevoegen zodra je een grammar gebruikt  waarin deze voorkomen

clauses

tokenize( Expr, Tokens ) :- scan( 0, Expr, Tokens ).

parse( Tokens, Term ) :-   	s_expr( Tokens, Unused_Tokens, Term ),  	Rest_Tokens = [].

%----
parse_report( Tokens, Term, Rest_Tokens ) :-   	s_expr( Tokens, Rest_Tokens, Term ).



%-----
parse_rest( Rest_tokens, _Count ):- Rest_Tokens = [] , ! .

parse_rest( List_tokens, Count ):-
 parse_report( List_tokens, Termx, Rest_Tokens ),

 assert( term_parsed( Count, Termx ) ),
 %conversion5x::term_str( expr , Termx , Res2 ),
 Res2 = toString( Termx ),
 write_out( [ Res2 , "\n" ] ),
 Count2 = Count + 1,
 parse_rest( Rest_Tokens, Count2 ).

%---
class predicates

string_replace_tag : (string, string, string, string ) determ( i,i,i,o).
string_replace_tag_in_pos : ( string, integer, string, string, string ) determ( i, i, i, i , o ).

change_adhoc : ( string, string ) determ( i, o ).
str_after : ( string, string, string, string ) determ( i,i,o,o).

which_list_for_second : ( integer, slist, slist, slist ) determ( i, i,i,o).


assert_var : ( string ) determ(i).
assert_vars : ( integer, atom_list ) determ( i, i ).
assert_variabels_concerned : ( integer ) determ( i ).
assert_variabels_concerned_list : ( ilist ) determ( i ).

add_when_not_in_list : ( slist, slist , slist ) determ( i,i,o).
filter_which_are_not_in_list : ( slist, slist , slist, slist ) determ( i,i,o,o).

is_member : ( string , slist ) determ( i , i ).
is_not_member : ( string , slist ) determ( i , i ).
reverse_slist : ( slist, slist, slist ) determ( i, i, o ).

clean_var_list : ( slist , slist ) determ( i,o).

position_of_last_occurance : ( string , string, integer , integer ) determ( i, i,i,o ).

first_up : ( string , string ) determ( i,o).

trim_tags : ( string, string, string ) determ( i,i,o).

trim_string : ( string, string ) determ( i, o ).

concat_list : ( slist , string , string ) determ( i, i, o ).

funct_constr_name_or_std_pred : ( operator, string ) determ( i,o).

komma_separator_if_more_resting : ( atom_list, string ) procedure( i,o ).

%---

debug_commment_str : ( string, string ) procedure( i, o ).

clauses

reverse_slist( [], Varslist2 , Varslist2 ):- !.
reverse_slist( [ Varx | Vars_str_list ], Varslist , Varslist2 ):-
  reverse_slist(  Vars_str_list , [ Varx | Varslist ] , Varslist2 ).


is_member( Varx, [ Varx | _Varslist ] ):- !.
is_member( Varx, [ _ | Varslist ] ):-  is_member( Varx, Varslist  ), !.

is_not_member( Varx, Varslist  ):- is_member( Varx, Varslist  ), !, fail .
is_not_member( _Varx, _Varslist  ):- !.




filter_which_are_not_in_list( [], _Varslist, [] , [] ):- !.
filter_which_are_not_in_list( [ Varx | Vars_str_list ], Varslist, [ Varx | Varslist2 ] , Varslist3 ):-
 is_not_member( Varx, Varslist ), !,
filter_which_are_not_in_list(  Vars_str_list , Varslist,  Varslist2  , Varslist3 ).
% TODO has probably to be reversed somewhere
filter_which_are_not_in_list( [ Varx | Vars_str_list ], Varslist, Varslist2 , [ Varx | Varslist3 ]):-
filter_which_are_not_in_list(  Vars_str_list , Varslist,  Varslist2  , Varslist3 ).


add_when_not_in_list( [], Varslist , Varslist2 ):- reverse_slist( Varslist , [], Varslist2 ), !.
add_when_not_in_list( [ Varx | Vars_str_list ], Varslist , Varslist2 ):-
 is_not_member( Varx, Varslist ), !,
add_when_not_in_list(  Vars_str_list , [ Varx | Varslist ] , Varslist2 ).
% TODO has probably to be reversed somewhere
add_when_not_in_list( [ _Varx | Vars_str_list ], Varslist , Varslist2 ):-
add_when_not_in_list(  Vars_str_list , Varslist  , Varslist2 ).



%---
position_of_last_occurance( Str, Tag, Cou , Rpos ):-   string5x::searchstring( Str , Tag ,  P ),
   string5x::frontstr( P, Str , _, Rest2 ), !, Cou2 = Cou + P,
   position_of_last_occurance( Rest2, Tag, Cou2 , Rpos ).
position_of_last_occurance( _Str, _Tag, Cou , Cou ):- !.

%--
string_replace_tag_in_pos( A, P, Rmvtag, Rep, Aq ):-  P > 0,
  % searchstring( A , Rmvtag ,  P),
  P2 = P - 1,
  string5x::str_len( Rmvtag, Le ), Le > 0, string5x::frontstr( P2, A, Sta, Rest ),
  string5x::frontstr( Le, REst, _, Rest2 ), !,	string5x::concat( Sta, Rep, Z1), string5x::concat ( Z1, Rest2, Aq ).
string_replace_tag_in_pos( A, _, _Rmvtag, _, A ):-!.


%---
string_replace_tag( A, Rmvtag, Rep, Ares ):-  string5x::searchstring( A , Rmvtag ,  P),  P2 = P - 1,
  string5x::str_len( Rmvtag, Le ), Le > 0, string5x::frontstr( P2, A, Sta, Rest ),
  string5x::frontstr( Le, REst, _, Rest2 ), !,	string5x::concat( Sta, Rep, Z1), string5x::concat ( Z1, Rest2, Aq ),
  string_replace_tag( Aq, Rmvtag, Rep, Ares ).
string_replace_tag( A, _Rmvtag, _, A ):-!.


% turn off
change_adhoc( Res , Res ):- !.
change_adhoc( Res4 , Res7 ):-
   string_replace_tag( Res4, ",\"", ",", Res5 ),
   string_replace_tag( Res5, "\"),\n", "),\n", Res6 ),
   string_replace_tag( Res6, "\")])", ")])", Res7 ), !.

% transpilex(22,"conditional( transx(- 6 , [numf(4),numf(5)] -) , transx(- 21 , [numf(8),variabel(\"$Cs\"),numf(20)] -) , 0 ) ")])
% after

assert_var( Sx ):- var_concerned( Sx ), !.
assert_var( Sx ):- assert( var_concerned( Sx ) ) , !.

assert_vars( _Nx, [] ):- !.
assert_vars( Nx, [ variabel( Sx, _, _ ) | Rs ] ):- !, assert_var( Sx ), assert_vars( Nx, Rs ).
% assert_vars( Nx, [ number( _ ) | Rs ] ):-  str_int( Sx, Nx ), !, concat( "Nvar", Sx, Sx2 ) , assert_var( Sx2 ), assert_vars( Nx, Rs ).

assert_vars( _Nx, [ _ | Rs ] ):- !,  assert_vars( _Nx, Rs ).

% assert_vars( Numsf_vars )
%---
assert_variabels_concerned( Nx ):-    is_transp( is_debug , Nx , _Operat , Subnums , Numsf_vars, _ ), !,
  assert_variabels_concerned_list( Subnums ),  assert_vars( Nx, Numsf_vars ).

assert_variabels_concerned_list( [] ):- !.
assert_variabels_concerned_list( [ Num | Nums_sub ] ):- !, assert_variabels_concerned( Num ),
 assert_variabels_concerned_list( Nums_sub ).

%--

which_list_for_second( 0, Varsn_lis2, _Varsn_lis_todo, Varsn_lis2 ):- !.
which_list_for_second( _Count_replace, _Varsn_lis2, Varsn_lis_todo, Varsn_lis_todo ):-!.

first_up( S2, C4 ):- string5x::str_len( S2, Le ), Le > 0, !, string5x::frontstr( 1, S2, Bg, Res ),
 conversion5x::upper_lower( Bg2 , Bg ) , string5x::concat( Bg2, Res, C4 ), !.
first_up( S2, S2 ):- !.

clean_var( H, S3 ) :- !, string_replace_tag( H, "$", "", S2 ), first_up( S2, S3 ) .


clean_var_list( [], [] ):- !.
clean_var_list( [ H | Ter ], [ S3 | Ter2 ] ):- !, clean_var( H , S3 ),  clean_var_list( Ter,  Ter2 ).

concat_list( [] , Res , Res ):- !.
concat_list( [ H | Lis ] , Hs , Res ):- !, string5x::concat( Hs, H, Hs2 ),  concat_list( Lis , Hs2 , Res ).


funct_constr_name_or_std_pred( namex( Fu, _, _ ) , Fu ):-!.
funct_constr_name_or_std_pred( Operat , Sq ):-
 Sq = toString( Operat ),
 !.



%---
% todo imp

trim_string( Bg0, Bg3 ):-  !, trim_tags( Bg0, ")", Bg2 ), trim_tags( Bg2, " ", Bg3 ).
% trim_tags( string, string, string ) - ( i,i,o)

trim_tags( Fie0, Tag, Fie ):- string5x::concat( Bg, Tag, Fie0 ), !, trim_tags( Bg, Tag, Fie ).
trim_tags( Fie0, Tag, Fie ):- string5x::concat( Tag, End, Fie0 ), !, trim_tags( End, Tag, Fie ).
trim_tags( Fie, _, Fie ):- !.

%---
% last_file
%-----

%---
komma_separator_if_more_resting( Rest, " , " ):- Le = list::length( Rest ), Le > 0, !.
komma_separator_if_more_resting( _Rest, "" ):-!.

debug_commment_str( _ , "" ):-!.
%debug_commment_str( Db_str, Db_str ):- !.

class facts - tmpx
count : ( integer ) determ .


class predicates

increment : ( integer ) determ( o ).

increment_count : ( integer ) determ( o ).
write_nspaces : ( integer ) determ( i ).
length_of_list : ( atom_list , integer, integer ) determ( i,i,o).
write_level : ( integer ) determ( i ).
str_before : ( string, string, string ) determ( i,i,o).


clauses


str_after( Varstr, Big_str, Before, Rest ):-
 string5x::str_len( Varstr , Lz ),  string5x::searchstring( Big_str , Varstr , Fp ),
 Fpx = Fp - 1,
 string5x::frontstr( Fpx , Big_str , Before , _Rest0 ),
 Fp2 = Fp + Lz - 1, string5x::frontstr( Fp2 , Big_str , _Sta , Rest ), !.

str_before( Varstr, Jso_big_str, Sta ):-
% str_len( Varstr , Lz ),
 string5x::searchstring( Jso_big_str , Varstr , Fp ),
 Fp2 = Fp - 1, string5x::frontstr( Fp2 , Jso_big_str , Sta , _Rest ), !.



length_of_list( [_|Lis0x ] , C, Le ):- C2 = C + 1, length_of_list( Lis0x  , C2, Le ).
length_of_list( [] , Le, Le ):-!.

increment_count( C2 ) :- retract( count( C) ),!, C2 = C + 1, assert( count( C2 ) ).


%---
write_nspaces( N ):- N > 0, !, write_out( ["  "] ),  N2 = N - 1, write_nspaces( N2 ).
write_nspaces( _N ):-  !.

write_level( Level ):- conversion5x::str_int( Sx, Level), write_out( [ "\n-lev-", Sx , "- " ] ), !.
%----


has_no_sub_nesting2( [] , Res, Res ):-!.

has_no_sub_nesting2( [ metta_sub(  par_atom_list( _Operatorx , _, _, Ato_lis ) ) | _Lis ] , Cou,  Res ):- !, Cou2 = Cou + 1,
    has_no_sub_nesting2(  Ato_lis  , Cou2, Res ), !.
has_no_sub_nesting2( [ _H | Lis ] , Cou, Res ):- !,   has_no_sub_nesting2( Lis  , Cou, Res ),!.



%----

write_out_metta2( _ , [] ) :- !.
write_out_metta2( Level, [ metta_sub( S_expr ) | Lis ] ):- !,   Level2 = Level + 1, write_out( ["("] ),
   write_out_metta( Level2, S_expr ), write_out( [")"] ), write_out_metta2( Level, Lis ).
write_out_metta2( Level, [ H | Lis ] ):- !,   Sx = toString( H ),
   write_level( Level ),   write_out( [Sx , "\n"] ) , write_out_metta2( Level, Lis ).

%-----
write_out_metta( Level, par_atom_list( Operatorx , _, _, Atom_list ) ):-
  Sx = toString( Operatorx ),  write_level( Level ),
  string5x::concat( "par_atox ", Sx, C1 ),  write_out( [ C1 ] ),
  write_out_metta2( Level, Atom_list ).
write_out_metta( Level, exclama_atom_list( Operatorx , _, _, Atom_list ) ):-
  Sx = toString( Operatorx ),  write_level( Level ),
  string5x::concat( "exclamax ", Sx, C1 ),  write_out([ C1 ]),  write_out_metta2( Level, Atom_list ).


%----

increment( C2 ):- retract( tel( C ) ), !, C2 = C + 1, assert( tel( C2 ) ).


%---







end implement metta_tpro
