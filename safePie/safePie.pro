% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement safePie
    open core

class facts
    thePie_fact : (pfc\pie\pie ThePie).

class properties
    thePie : pfc\pie\pie (o).

clauses
    thePie() = Pie :-
        thePie_fact(Pie),
        !.

    thePie() = Pie :-
        Pie = pfc\pie\pie::new(),
        assert(thePie_fact(Pie)).

clauses
    run(S) :-
        safe(predicate_name(), thePie:run, S).

clauses
    reconsult_text(S) :-
        safe(predicate_name(), thePie:reconsult_text, S).

class predicates
    safe : (string PredicateName, predicate{string} Action, string Arg).
clauses
    safe(PredicateName, Action, Arg) :-
        try
            Action(Arg)
        catch TraceId do
            if exception::descriptor(_ci, exception(_, _, Description), _k, Extra, _t, _tid) =
                    exception::tryGetDescriptor(TraceId, pfc\pie\pie::syntaxError_exception)
            then
                stdio::write(Description),
                if Cursor = namedValue::tryGetNamed_integer(Extra, pfc\pie\pie::cursor_parameter) then
                    stdio::write(" at position ", Cursor)
                end if,
                if Arg = namedValue::trygetNamed_string(Extra, exception::errorArguments_parameter) then
                    stdio::write(": ", Arg)
                end if,
                stdio::write("\n")
            else
                exception::continue_unknown(TraceId, PredicateName)
            end if
        end try.

clauses
    getPredicateNames() = thePie:getPredicateNames().

clauses
get_model_type_namesx( Lis ) :- thePie:get_model_type_names( Lis ) , !.
set_model_profilex( Name, Lis , Is_che ) :- thePie:set_model_profile( Name , Lis , Is_che ) , !.
get_model_profilex( Lis , Is_che ) :- thePie:get_model_profile( Lis , Is_che ) , !.

try_string_to_stermx( Bstr , Sxterm ):-     thePie:try_string_to_sterm( Bstr , Sxterm ) , ! .

tokenize_x( Bstr , TOKS ):-     thePie:tokenize( Bstr , TOKS ) , ! .

parse_metta1_x( Bstr , Sxterm , Rest_toks ):-     thePie:parse_metta1( Bstr , Sxterm , Rest_toks ) , ! .

cmp_clause_to_metta_x( Sxterm , Bstr ):-     thePie:cmp_clause_to_metta( Sxterm , Bstr ) , ! .

try_set_consult_modelx( Name ) :-  thePie:try_set_consult_model( Name ) , ! .

%---
recons_newclausex( Sxterm ):-   thePie:recons_newclause_z( "recons_newclausex" , Sxterm ), !.
%----
% 24-5-2025
% start_parse_x( Bstr , Metta_parsed ):-  !,     thePie:start_parse( Bstr, Metta_parsed ).

start_parse_x( Bstr , Result , Line, Lpos ):-  !,     thePie:start_parse( Bstr , Result , Line, Lpos ).
start_only_parse_x( Bstr , Result , Line, Lpos ):-  !,     thePie:start_only_parse( Bstr , Result , Line, Lpos ).

%  convert_metta_commandli
convert_metta_commandline( Bstr , Bstr2 ):-  !,     thePie:convert_metta_commandli( Bstr, Bstr2 ).

memory_to_prolog_filex():- thePie:memory_to_prolog_file() , ! .
memory_to_prolog_filex():- ! .

transpile_metta_sub( Count, Level0, Level, Metta_parsed, Metta_transp ):-  !,     thePie:transpile_metta_sub( Count, Level0, Level, Metta_parsed, Metta_transp ).
% transpile_subcl_metta_tpro_subc(   [ cmp( "sub",  Atom_List2 ) | Lis ] , Result_list  ):- !,
inventarise_metta_sub( Count, Mlevel , Level0, Level, Metta_parsed ):-  !,     thePie:inventarise_metta_sub( Count, Mlevel , Level0, Level, Metta_parsed ).

start_parse_pro( Bstr , Pro_parsed ):-  !,     thePie:start_pars_pro( Bstr, Pro_parsed ).
% try_string_to_sterm( Bstr , Sxterm ):- Sxterm = tryToTerm( Bstr ) , ! .
% try_string_to_sterm( _Bstr , nill ):-  ! .

get_clause_positionx( Inpos , Predname, Bg, End ):- !,
 thePie:get_clause_position( Inpos , Predname, Bg, End ).


clauses
    retractAll() :-
        thePie:retractAll().

clauses
    terminateExecution() :-
        thePie:terminateExecution().

clauses
    traceOn(V) :-
        thePie:traceOn := V.

clauses
    traceOn() = thePie:traceOn.

end implement safePie
