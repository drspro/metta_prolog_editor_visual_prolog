% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

class safePie

predicates
    run : (string Goal).
    % @short Run #Goal
    % @end

predicates
    getPredicateNames : () -> string* PredicateNames.
    % @short Get the list of predicate names (of predicates that have clauses)
    % @end

predicates
try_string_to_stermx : ( string [in] , pfc\pie\pie::sterm Term [out] ) determ.

% parse_metta1_x : ( pfc\pie\pie::source [in] , pfc\pie\pie::sterm Term [out] ) determ.
parse_metta1_x : ( pfc\pie\pie::tokl [in] , pfc\pie\pie::sterm Term [out] , pfc\pie\pie::tokl [out] ) determ.

tokenize_x : ( pfc\pie\pie::source [in] , pfc\pie\pie::tokl [out]  ) determ.

cmp_clause_to_metta_x : ( pfc\pie\pie::sterm Term [in] , string [out] ) determ.

try_set_consult_modelx : ( string [in] ) determ.

get_model_type_namesx : ( string* ) procedure( o ).
set_model_profilex : ( string, string* , integer ) procedure( i, i , i  ).
get_model_profilex : ( string* , integer ) procedure( o , o  ).
% get_model_profilex : ( string* , integer ) procedure( o , o  ).

recons_newclausex : ( pfc\pie\pie::sterm Term [in] ) determ.

%start_parse_x : ( string [in] , pfc\pie\pie::expr [out]) determ.

% 24-5-2025
% start_parse_x : ( string [in] , pfc\pie\pie::sterm [out]) determ.
start_parse_x : ( string [in] , integer [out] , integer [out] , integer [out] ) determ.
start_only_parse_x : ( string [in] , integer [out] , integer [out] , integer [out] ) determ.

convert_metta_commandline : ( string [in] , string [out]) determ.

transpile_metta_sub : ( integer [in], integer [in], integer [in] , pfc\pie\pie::sterm [in] , pfc\pie\pie::sterm [out]) determ.

memory_to_prolog_filex : () procedure().

inventarise_metta_sub : ( integer [in], integer [in], integer [in], integer [in] , pfc\pie\pie::sterm [in] ) determ.

start_parse_pro : ( string [in] , pfc\pie\pie::sterm [out]) determ.

get_clause_positionx : ( integer , string , integer, integer ) procedure( i, o , o , o ).

predicates
    reconsult_text : (string Text).
    % @short Reconsult #Text (i.e. #Text is clauses, not a filename).
    % @end

predicates
    retractAll : ().
    % @short Retract all clauses from the the PIE interpreter
    % @end

predicates
    terminateExecution : ().
    % @short Request the engine to terminate the execution.
    % @end

properties
    traceOn : boolean.
    % @short The execution is traced if traceOn is true.
    % @end

end class safePie