% Copyright PDC

namespace pfc\pie

interface pie
    open core

% PARSE METTA
constants

is_debug = 44.
is_perform = 46.

has_subcalls_in_body = 74.
has_no_subcalls_in_body = 77.

is_term_head = 79.
is_term_body = 80.

% is_zero = 0.
transpile_default = 0.
preserve_metta_vars = 201.
keep_all = 202.
remove_metta_outpvar_and_created = 203.
preserve_metta_output_var = 204.
dont_allow_output_vars = 205.

is_undefined_pred = 400.
is_database_pred = 402.
is_complex_pred = 403.
is_standard_pred = 404.
is_database_construct_pred = 405.
is_sub_clause = 406.
is_clause_implementation = 407.

query1 = 501.
query2 = 502.


is_db_result_term = 742.
is_db_result_execute = 743.
is_db_result_variabel = 744.



% PARSE METTA
domains
   % transpiled( integer, string );
   % transpiledx( string );
     %sterm;
 % termBase;

  slist = string*.
  ilist = integer*.
  atom = variabel( string, integer, integer );	number( real, integer, integer );	numf( integer, integer, integer );
  namex( string, integer, integer ); metta_sub( expr );   empty_atom( integer, integer )  .

  atom_list       = atom* .
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

% PARSE METTA
domains

cursorq	= integer.
cursortoq 	= tk(tok, cursorq, integer ).
message	= string.
result	= real.
source	= string.
tokl 		= cursortoq*.

domains
  number_of_extra_characters 	= integer.
  number_of_spaces		= integer.




domains
    termBase{VarRep} =
        var(VarRep Var);
        cmp(string Name, termBase{VarRep}* Arguments);
        list(termBase{VarRep} Head, termBase{VarRep} Tail);
        nill;
        atom(string Value);
        int(integer Value);
        int64(integer64 Value);
        real(real Value);
        str(string Value);
        char(char Value);
        object(object Value);
        pointer(pointer Value)
        [presenter(pie::presenter_termBase)].
    % @short termBase is the base domain for both syntactic (#VarRep = string) and semantic terms (#VarRep = variable).
    % @end

domains
    terml = term*.
    term = termBase{variable}.
    % @short semantic terms (i.e. with semantic variables)
    % @end

domains
    sterml = sterm*.
    sterm = termBase{string}.
    % @short Syntactic terms (i.e. with syntactic variables)
    % @end

domains
    outputMode = list; write; display.
    % @short display mode for term writer
    % @end

predicates
    run : (string Goal).
    % @short Run #Goal
    % @end

predicates
    runGoal : (string Goal, environment2 Env) nondeterm.
    % @short Run #Goal using the environment #Env
    % @end

predicates
    runGoalTerm : (sterm GoalTerm, environment2 Env) nondeterm.
    % @short Run #Goal using the environment #Env
    % @end

predicates
    parse : (string Goal) -> sterm GoalTerm.
    % @short Parse #Goal
    % @end

predicates
    assertClauses : (string Clauses).
    % @short Parse and assert #Clauses to the PIE interpreter.
    % @end

domains
    isPredicateSupportedResponder = (pie PIE, string Id) -> invokePredicate Predicate determ.
    % @short User defined callback that returns the invokePredicate for user defined predicates.
    % @end

domains
    invokePredicate = (pie PIE, string Id, term* Args) nondeterm.
    % @short A user defined PIE predicate, i.e. a predicate that can be called from PIE scripts
    % @end

domains
    isFunctionSupportedResponder = (pie PIE, string Id) -> invokeFunction Function determ.
    % @short User defined callback that returns the invokeFunction for user defined functions.
    % @end

domains
    invokeFunction = (pie PIE, string Id, term* Args) -> term Result.
    % @short A user defined PIE function, i.e. a function that can be called from PIE scripts
    % @end

properties
    realDivisionWithZeroResultsInZero : boolean.
    % @short For backwards compatibility.
    % @end

properties
    isPredicateSupportedResponder : isPredicateSupportedResponder.
    % @short The current isPredicateSupportedResponder.
    % @end

properties
    isFunctionSupportedResponder : isFunctionSupportedResponder.
    % @short The current isFunctionSupportedResponder.
    % @end

properties
    outputStream : outputStream.
    % @short The current outputStream.
    % @end

predicates
    evalInt : (term Term) -> integer Value.
    % @short Get the #Value if #Term evaluates to an integer, raise a pie_exception otherwise.
    % @end

predicates
    tryEvalInt : (term Term) -> integer Value determ.
    % @short Get the #Value if #Term evaluates to an integer, fail otherwise.
    % @end

predicates
    evalReal : (term Term) -> real Value.
    % @short Get the #Value if #Term evaluates to a real, raise a pie_exception otherwise.
    % @end

predicates
    tryEvalReal : (term Term) -> real Value determ.
    % @short Get the #Value if #Term evaluates to a real, fail otherwise.
    % @end

predicates
    evalTerm : (term Term) -> term Evaluated.
    % @short #Evaluated is the result of evaluating expressions and functions in #Term.
    % @end

predicates
    retractAll : ().
    % @short Retract all clauses from the the PIE interpreter
    % @end

predicates
    getPredicateNames : () -> string* PredicateNames.
    % @short Get the list of predicate names (of predicates that have clauses)
    % @end

predicates
    reconsult_text : (string Text).
    % @short Reconsult #Text (i.e. #Text is clauses, not a filename).
    % @end

predicates
    terminateExecution : ().
    % @short Request the engine to terminate the execution.
    % @end

properties
    traceOn : boolean.
    % @short The execution is traced if traceOn is true.
    % @end

predicates
    backtrackpoint : () multi.
    % @short Inform the Interpreter that a backtrackpoint has been created.
    % @end

predicates
    writeterml : (outputMode Mode, term* TermList).
    % @short Write #TermList using #Mode
    % @end

predicates
    cut_p : ().
    % @short Perform cut in the engine. To be used in invokePredicate's.
    % @end

predicates
    consult_p : (string Filename) determ.
    % @short consult #Filename into the PIE interpreter. To be used in invokePredicate's.
    % @end

predicates
    reconsult_p : (string Filename).
    % @short reconsult #Filename into the PIE interpreter. To be used in invokePredicate's.
    % @end

predicates
    save_p : (string Filename).
    % @short Save the clauses in PIE interpreter to #Filename. To be used in invokePredicate's.
    % @end

predicates
    halt_p : () failure.
    % @short "halt" the PIE interpreter. To be used in invokePredicate's.
    % @end

predicates
    list_p : ().
    % @short list all clauses. To be used in invokePredicate's.
    % @end

predicates
    list_p : (string Predicate).
    % @short list clauses for #Predicate. To be used in invokePredicate's.
    % @end

predicates
    list_p : (string Predicate, integer Arity).
    % @short list clauses for #Predicate with #Arity. To be used in invokePredicate's.
    % @end

predicates
    retract_p : (term Term) nondeterm.
    % @short retract #Term from the PIE interpreter. To be used in invokePredicate's.
    % @end

predicates
    unify : (term A, term B) determ.
    % @short Unify #A and #B in PIE interpreter. To be used in invokePredicate's.
    % @end

predicates

set_debug : ( boolean ) procedure( i ).
% try_string_to_sterm : ( string [in] , pie::sterm Term [out] ) determ.
try_string_to_sterm : ( string [in] , sterm Term [out] ) determ.
recons_newclause_z :  ( string [in] , sterm Term [in] ) determ.

get_model_type_names : ( string* ) procedure( o ).

set_model_profile : ( string, string* , integer ) procedure( i , i , i ).
get_model_profile : ( string* , integer ) procedure( o , o ).


% expr
% start_parse : ( string [in] , sterm [out] ) determ.
% 24-5-2025
start_parse : ( string [in] , integer [out] , integer [out], integer [out] ) determ.
start_only_parse : ( string [in] , integer [out] , integer [out], integer [out] ) determ.
convert_metta_commandli : ( string [in] , string [out] ) determ.

transpile_metta_sub : ( integer [in], integer [in], integer [in] , sterm [in] , sterm [out] ) determ.
inventarise_metta_sub : ( integer [in] , integer [in] , integer [in] , integer [in] , sterm [in] ) determ.


cmp_clause_to_metta : ( sterm [in] , string [out] ) .


start_pars_pro : ( string [in] , sterm [out] ) determ.

predicates_transpile_variabels : () procedure().
memory_to_prolog_file : () determ().

try_set_consult_model : ( string ) determ( i ).

get_clause_position : ( integer , string, integer, integer ) procedure( i, o, o, o ).

% parse_metta1 : ( source  [in] , sterm [out] ) determ.
parse_metta1 : ( tokl  [in] , sterm [out] , tokl  [out] ) determ.

tokenize : (source [in], tokl [out]) determ.


cmp_element_to_metta_string : (sterm, string [out], string [out]).
cmp_elements_to_metta_string : (sterml, string, string [out]) determ.



properties
    solutionCount : positive (o).
    % @short The number of solutions found to last goal.
    % @end

properties
    clauseBase : clauseBase (o).
    % @short Clauses base.
    % @end

end interface pie
