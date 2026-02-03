% Copyright ©

class metta_tpro
    open core

constants

is_debug = 44.
is_perform = 46.

has_subcalls_in_body = 74.
has_no_subcalls_in_body = 77.

is_term_head = 79.
is_term_body = 80.



domains
  slist = string*.
  ilist = integer*.
  atom = variabel( string, integer, integer );	number( real, integer, integer );	numf( integer, integer, integer );
  namex( string, integer, integer ); metta_sub( expr ); transpiled( integer, string ); transpiledx( string );
   empty_atom( integer, integer )  .

  atom_list       = atom*.
  operator        = equal( integer, integer ); dbl_equal( integer, integer );		    conditional( integer, integer );		    conjuction( integer, integer );
		    smallerthan( integer, integer );		    plus( integer, integer );		    multiplication( integer, integer );
			exclamation( integer, integer );		    division( integer, integer );		    minus( integer, integer );
			namex( string, integer, integer );			variabel( string , integer, integer).
  expr    = par_atom_list( operator , integer, integer, atom_list );
            exclama_atom_list( operator , integer, integer, atom_list ).
  tok		  = variabel_tk( string );                    name_tk( string);
		    number_tk( real  );		    equal_tk;  dbl_equal_tk;
		    conditional_tk;		    conjuction_tk;		    smallerthan_tk;
		    plus_tk;		    multiplication_tk;			exclamation_tk;
		    division_tk;		    minus_tk;		    lpar_tk;
            rpar_tk;		    nill.


domains

cursorq	= integer.
cursortoq 	= tk(tok, cursorq, integer ).
message	= string.
result	= real.
source	= string.
tokl 		= cursortoq*.

predicates

start_parse : ( string ) procedure( i ).
set_file : ( string ) procedure( i ).
% file_for_out : ( string ) determ( o ).
filter_out_comments : ( string, string ) determ( i,  o ).
tokenize : ( source, tokl ) determ(i,o).
parse_report : ( tokl, expr,   tokl ) determ( i , o , o ).
empty_errors : () procedure().
set_current_position : ( core::positive, core::positive ) procedure( i, i ).
syntax_errors_nd : ( string, cursortoq  ) nondeterm( o, o ).

end class metta_tpro
