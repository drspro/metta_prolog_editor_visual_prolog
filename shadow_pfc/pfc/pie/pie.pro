% Copyright PDC

namespace pfc\pie

implement pie
    open core

class facts - clause_positions
    express_position : (string, integer, integer).

class facts - positions
    cur_position : (core::positive, core::positive) determ.

class facts - parse_errors
%tok_error : ( string , cursortoq ).
    tok_error : (string, tok, integer, integer).
% tk(tok, cursorq, integer ).
% tok_error( Type_mess , Tk , P1 , P2 ) )

class facts - enums_preds
    enum_pred_mem : (integer, string, integer, string).

class facts - db_head_subs
    head_subs : (integer, ilist, slist).

class facts - db_transpile_start
    transpile_start : (expr) determ.

%is_transp : ( integer, integer, operator, ilist, atom_list, atom_list ).
% 09:38 24-5-2025
% class facts - db_is_transp
% is_transp : ( integer, integer, operator, ilist, terml, terml ).
class facts - db_is_trpile
%is_trpile : ( integer, integer, operator, ilist, atom_list, atom_list ).
    is_trpile : (integer, integer, operator, ilist, terml, terml).

class facts - db_varsx
    var_concerned : (string).
    is_metta_output_var : (string).

class facts - db_construct
    to_be_subconstructed : (integer, string).

class facts - db_debo_org_str
    is_debug : (boolean).
    debo_org_str : (integer, string).

class facts - weqass
    lasts_metta_string : (string) determ.
    last_file : (string) determ.
    previous_pred : (string) determ.

class facts - pred_levels
%pred_level : ( integer, integer, expr ).
% pred_level : ( integer, integer, term ).
    tel : (integer) determ.

% class facts - parsedx
% term_parsed : ( integer , sterm ).
class facts - current_model
    current_transpile_model : (integer, integer, integer, integer, integer) determ.

class facts - tmpx
    count : (integer) determ.

class facts - pred_nestings
    predicate_nesting : (integer, integer, integer, integer, string, string*, integer*, integer, integer, integer).
    predicate_nesting_ordered : (integer, integer, integer, integer, integer, string, string*, integer*, integer, integer, integer).
    nested_pred_memory : (integer, integer, integer, integer, integer, string, integer, string, string*, integer*, integer, string*).
    constr_clauses : (integer, integer, integer, integer, integer, string, string, string*, integer*, integer*, string*).
    is_match_db_pred : (integer, string, integer, integer, integer, integer, string, string, string, sterm, sterm, integer).

class facts - pred_nestings_tmp
    predicate_nesting_ordered_tmp : (integer, integer, integer, integer, integer, string, string*, integer*, integer, integer, integer).

class facts - metta_std_preds_tmp
    std_metta_pred_tmp : (string, integer, sterm).

class facts - metta_std_preds
    std_metta_pred : (string, integer, sterm).

class facts - string_lines
    str_line : (integer, integer, string).

class facts - memory_clauses
    clause_memory : (integer, integer, integer, string, string, sterm).

class facts - memory_sort
    predicate_for_sort : (integer, string, string, string*).

class facts - memory_clauses_tmp
    clause_memory_tmp : (integer, integer, integer, string, string, sterm).

% class facts - memory_clauses_tmp
% clause_memory_tmp : ( integer, string , sterm ).
% copy_data_clause( Type_of_term , TERMX_trans, TERMX_trans2 ):-  Type_of_term = is_database_pred ,
class facts - filesxy
    outp_file : (outputStream) determ.

% asser_pred_nest( Pname, Level0, Level, Level2 , Varslist ):-
%class facts - mai_level
%main_level : ( integer ) determ.
facts
    traceOn : boolean := false.
    stopExecution_flag : () determ.
    topmostStackmark : (programControl::stackMark TopMost) determ.
    outputStream : outputStream.
    solutionCount : positive := 0.
    clauseBase : clauseBase.
    resetPoint_fact : (resetPoint ResetPoint).
    removedForReconsult_fact : (string Predicate).

clauses
    new() :-
        new(stdio::outputStream).

clauses
    new(Stream) :-
        outputStream := Stream,
        clauseBase := clauseBase::new().

class predicates
    raise_pie_exception : (string PredicateName, ...) erroneous.
clauses
    raise_pie_exception(PredicateName, ...) :-
        Description = string::write(...),
        exception::raiseDetailed(pie_exception,
            [
                namedValue(exception::predicateName_parameter, string(PredicateName)),
                namedValue(exception::errorArguments_parameter, string(Description))
            ]).

clauses
    raiseSyntaxError_explicit(ProgramPoint, Cursor, Format, ...) :-
        Msg = string::format(Format, ...),
        exception::raiseDetailed_explicit(ProgramPoint, syntaxError_exception,
            [namedValue(cursor_parameter, integer(Cursor)), namedValue(exception::errorArguments_parameter, string(Msg))]).

clauses
    evalTerm(T) = V :-
        try
            V = evalTerm2(variable::normalize(T))
        catch E do
            exception::continue_unknown(E, "term=", T)
        end try.

predicates
    evalTerm2 : (term) -> term.
clauses
    evalTerm2(cmp(Name, AL)) = evalOuterMost2(cmp(Name, evalTermList(AL))) :-
        !.

    evalTerm2(list(H, T)) = list(evalTerm(H), evalTerm(T)) :-
        !.

    evalTerm2(T) = evalOuterMost2(T).

predicates
    evalTermList : (term* TermList) -> term* EvaluatedList.
clauses
    evalTermList(TermList) = list::map(TermList, evalTerm).

predicates
    evalOuterMost : (term) -> term.
clauses
    evalOuterMost(T) = evalOuterMost2(variable::normalize(T)).

predicates
    evalOuterMost2 : (term Term) -> term Evaluated.
clauses
    evalOuterMost2(cmp(DyadOpStr, [T1, T2])) = ResultNum :-
        GT1 = tryGroundTerm(T1),
        GT2 = tryGroundTerm(T2),
        boundTerm(GT1),
        boundTerm(GT2),
        isDyadOp(DyadOpStr, IntDyadOp, RealDyadOp, StrDyadOp),
        !,
        Num1 = evalOuterMost(GT1),
        Num2 = evalOuterMost(GT2),
        coerceDyadOpArg(DyadOpStr, Num1, Num2, CNum1, CNum2),
        ResultNum = applyDyadOp(IntDyadOp, RealDyadOp, StrDyadOp, CNum1, CNum2).

    evalOuterMost2(cmp(MonadOpStr, [T1])) = ResultNum :-
        GT1 = tryGroundTerm(T1),
        boundTerm(GT1),
        isMonadOp(MonadOpStr, IntMonadOp, RealMonadOp),
        !,
        Num1 = evalOuterMost(GT1),
        ResultNum = applyMonadOp(IntMonadOp, RealMonadOp, Num1).

    evalOuterMost2(cmp(FunctionId, ArgTermList)) = Result :-
        Invoker = isFunctionSupportedResponder(This, FunctionId),
        !,
        Result = trace_FunctionCall(FunctionId, Invoker, evalTermList(ArgTermList)).

    evalOuterMost2(cmp(FunctionId, ArgTermList)) = Result :-
        CallArity = list::length(ArgTermList),
        Invoker = stdPredicate::isStdFunction(FunctionId, CallArity),
        !,
        Result = trace_FunctionCall(FunctionId, Invoker, evalTermList(ArgTermList)).

    evalOuterMost2(Term) = Term.

facts
    realDivisionWithZeroResultsInZero : boolean := false.

predicates
    realDivision : (real X, real Y) -> term.
clauses
    realDivision(_, 0.0) = real(0.0) :-
        !,
        if realDivisionWithZeroResultsInZero = false then
            raise_pie_exception(predicate_name(), "RealDivision by Zero")
        end if.

    realDivision(X, Y) = real(X / Y).

class predicates
    modreal : (real, real) -> real.
clauses
    modreal(X, Y) = X - math::trunc(X / Y) * Y.

class predicates
    int3264 : (integer64 Int) -> term Term.
clauses
    int3264(Int) = int(convert(integer, Int)) :-
        Int >= lowerBound(integer),
        Int <= upperBound(integer),
        !.

    int3264(Int) = int64(Int).

predicates
    isDyadOp : (string, function{integer64, integer64, term} IntOp [out], function{real, real, term} RealOp [out],
        function{string, string, term} StrOp [out]) determ.
clauses
    isDyadOp("-", { (IX, IY) = int3264(IX - IY) }, { (X, Y) = real(X - Y) },
            {  = _ :- raise_pie_exception(predicate_name(), "Subtraction not supported for strings!") }).
    isDyadOp("+", { (IX, IY) = int3264(IX + IY) }, { (X, Y) = real(X + Y) }, { (X, Y) = str(string::concat(X, Y)) }).
    isDyadOp("*", { (IX, IY) = int3264(IX * IY) }, { (X, Y) = real(X * Y) },
            {  = _ :- raise_pie_exception(predicate_name(), "Multiplication not supported for strings!") }).
    isDyadOp("/", {  = _ :- raise_pie_exception(predicate_name(), "Real division not supported for integers!") }, realDivision,
            {  = _ :- raise_pie_exception(predicate_name(), "Division not supported for strings!") }).
    isDyadOp("div", { (IX, IY) = int3264(IX div IY) },
            {  = _ :- raise_pie_exception(predicate_name(), "Integer division not supported for reals!") },
            {  = _ :- raise_pie_exception(predicate_name(), "Division not supported for strings!") }).
    isDyadOp("mod", { (IX, IY) = int3264(IX mod IY) }, { (X, Y) = real(modreal(X, Y)) },
            {  = _ :- raise_pie_exception(predicate_name(), "Modulo not supported for strings!") }).
    isDyadOp("rem", { (IX, IY) = int3264(IX rem IY) }, {  = _ :- raise_pie_exception(predicate_name(), "Remainder not supported for reals!") },
            {  = _ :- raise_pie_exception(predicate_name(), "Remainder not supported for strings!") }).
    isDyadOp("min", { (IX, IY) = int3264(math::min(IX, IY)) }, { (X, Y) = real(math::min(X, Y)) },
            { (X, Y) =
                str(
                    if X < Y then
                        X
                    else
                        Y
                    end if)
            }).
    isDyadOp("max", { (IX, IY) = int3264(math::max(IX, IY)) }, { (X, Y) = real(math::max(X, Y)) },
            { (X, Y) =
                str(
                    if X < Y then
                        Y
                    else
                        X
                    end if)
            }).

class predicates
    isMonadOp : (string, function{integer64, term} IntOp [out], function{real, term} RealOp [out]) determ.
clauses
    isMonadOp("-", { (IX) = int3264(-IX) }, { (X) = real(-X) }).
    isMonadOp("abs",
            { (IX) =
                int3264(
                    if IX < 0 then
                        -IX
                    else
                        IX
                    end if)
            },
            { (X) = real(math::abs(X)) }).
    isMonadOp("trunc", { (IX) = int3264(IX) }, { (X) = int3264(math::trunc(X)) }).
    isMonadOp("round", { (IX) = int3264(IX) }, { (X) = int3264(math::round(X)) }).

class predicates
    coerceDyadOpArg : (string Op, term X, term Y, term CX [out], term CY [out]).
clauses
    coerceDyadOpArg("/", int(X), int(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg("/", int64(X), int64(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg("/", int(X), int64(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg("/", int64(X), int(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg(_, str(X), int(Y), str(X), str(toString(Y))) :-
        !.
    coerceDyadOpArg(_, str(X), real(Y), str(X), str(toString(Y))) :-
        !.
    coerceDyadOpArg(_, int(X), str(Y), str(toString(X)), str(Y)) :-
        !.
    coerceDyadOpArg(_, int64(X), str(Y), str(toString(X)), str(Y)) :-
        !.
    coerceDyadOpArg(_, real(X), str(Y), str(toString(X)), str(Y)) :-
        !.
    coerceDyadOpArg(_, str(X), str(Y), str(X), str(Y)) :-
        !.
    coerceDyadOpArg(_, int(X), int(Y), int(X), int(Y)) :-
        !.
    coerceDyadOpArg(_, int64(X), int64(Y), int64(X), int64(Y)) :-
        !.
    coerceDyadOpArg(_, int(X), int64(Y), int64(X), int64(Y)) :-
        !.
    coerceDyadOpArg(_, int64(X), int(Y), int64(X), int64(Y)) :-
        !.
    coerceDyadOpArg(_, real(X), int(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg(_, int(X), real(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg(_, real(X), int64(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg(_, int64(X), real(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg(_, real(X), real(Y), real(X), real(Y)) :-
        !.
    coerceDyadOpArg(Op, X, Y, _, _) :-
        raise_pie_exception(predicate_name(), "Arguments cannot be coerced to same compatible numerical domain! Op=", Op, ", X = ", termToString(X),
            ", Y = ", termToString(Y)).

class predicates
    applyDyadOp : (function{integer64, integer64, term} IntOp, function{real, real, term} RealOp, function{string, string, term} StrOp, term X,
        term Y) -> term Z.
clauses
    applyDyadOp(IntOp, _, _, int(X), int(Y)) = IntOp(X, Y) :-
        !.
    applyDyadOp(IntOp, _, _, int64(X), int64(Y)) = IntOp(X, Y) :-
        !.
    applyDyadOp(IntOp, _, _, int(X), int64(Y)) = IntOp(X, Y) :-
        !.
    applyDyadOp(IntOp, _, _, int64(X), int(Y)) = IntOp(X, Y) :-
        !.
    applyDyadOp(_, RealOp, _, real(X), real(Y)) = RealOp(X, Y) :-
        !.
    applyDyadOp(_, _, StrOp, str(X), str(Y)) = StrOp(X, Y) :-
        !.
    applyDyadOp(_, _, _, X, Y) = _ :-
        raise_pie_exception(predicate_name(), "Invalid arguments to dyadic operator!", ", X = ", termToString(X), ", Y = ", termToString(Y)).

class predicates
    applyMonadOp : (function{integer64, term} IntOp, function{real, term} RealOp, term X) -> term Y.
clauses
    applyMonadOp(IntOp, _, int(X)) = IntOp(X) :-
        !.
    applyMonadOp(IntOp, _, int64(X)) = IntOp(X) :-
        !.
    applyMonadOp(_, RealOp, real(X)) = RealOp(X) :-
        !.
    applyMonadOp(_, _, X) = _ :-
        raise_pie_exception(predicate_name(), "Invalid arguments to monadic operator!", ", X = ", termToString(X)).

clauses
    tryEvalInt(Term) = X :-
        int(X) = evalOuterMost(Term).

clauses
    evalInt(Term) = X :-
        X = tryEvalInt(Term),
        !.

    evalInt(Term) = _ :-
        raise_pie_exception(predicate_name(), "Not an integer expression! ", termToString(Term)).

clauses
    tryEvalReal(Term) = X :-
        real(X) = evalOuterMost(Term).

clauses
    evalReal(Term) = X :-
        X = tryEvalReal(Term),
        !.

    evalReal(Term) = _ :-
        raise_pie_exception(predicate_name(), "Not a real expression! ", output::termToString(display, Term)).

/*****************************************************************************
    Binary Comparison Evaluation
******************************************************************************/
class predicates
    isBinaryCmp : (string Id) -> predicate_dt{term, term} determ.
clauses
    isBinaryCmp("<") = { (X, Y) :- X < Y }.

    isBinaryCmp(">") = { (X, Y) :- X > Y }.

    isBinaryCmp("=<") = { (X, Y) :- X <= Y }.

    isBinaryCmp("<=") = { (X, Y) :- X <= Y }.

    isBinaryCmp("=>") = { (X, Y) :- X >= Y }.

    isBinaryCmp(">=") = { (X, Y) :- X >= Y }.

    isBinaryCmp("><") = { (X, Y) :- X >< Y }.

    isBinaryCmp("<>") = { (X, Y) :- X <> Y }.

predicates
    applyCmp : (predicate_dt{term, term} Cmp, term T1, term T2) determ.
clauses
    applyCmp(Cmp, T1, T2) :-
        NT1 = evalOuterMost(T1),
        NT2 = evalOuterMost(T2),
        coerceCmpArgs(NT1, NT2, CT1, CT2),
        Cmp(CT1, CT2).

class predicates
    coerceCmpArgs : (term T1, term T2, term CT1 [out], term CT2 [out]).
clauses
    % Integer comparisons
    coerceCmpArgs(int(X), int(Y), int(X), int(Y)) :-
        !.
    coerceCmpArgs(int64(X), int64(Y), int64(X), int64(Y)) :-
        !.
    coerceCmpArgs(int(X), int64(Y), int64(X), int64(Y)) :-
        !.
    coerceCmpArgs(int64(X), int(Y), int64(X), int64(Y)) :-
        !.
        % Real comparisons
    coerceCmpArgs(int(X), real(Y), real(X), real(Y)) :-
        !.
    coerceCmpArgs(int64(X), real(Y), real(X), real(Y)) :-
        !.
    coerceCmpArgs(real(X), int(Y), real(X), real(Y)) :-
        !.
    coerceCmpArgs(real(X), int64(Y), real(X), real(Y)) :-
        !.
    coerceCmpArgs(real(X), real(Y), real(X), real(Y)) :-
        !.
        % Other comparisons
    coerceCmpArgs(str(X), str(Y), str(X), str(Y)) :-
        !.
    coerceCmpArgs(char(X), char(Y), char(X), char(Y)) :-
        !.
    coerceCmpArgs(object(X), object(Y), object(X), object(Y)) :-
        !.
    coerceCmpArgs(pointer(X), pointer(Y), pointer(X), pointer(Y)) :-
        !.
    coerceCmpArgs(T1, T2, _, _) :-
        raise_pie_exception(predicate_name(), "Terms cannot be compared!", ", T1 = ", output::termToString(display, T1), ", T2 = ",
            output::termToString(display, T2)).

/*****************************************************************************
  Free/Bound
******************************************************************************/
clauses
    boundTerm(T) :-
        not(var(_) = variable::normalize(T)).

clauses
    freeTerm(T) :-
        not(boundTerm(T)).

clauses
    tryGroundTerm(var(V)) = tryGroundTerm(variable::normalize(var(V))) :-
        !,
        boundTerm(var(V)).

    tryGroundTerm(cmp(Name, Args)) = cmp(Name, tryGroundTermList(Args)) :-
        !.

    tryGroundTerm(list(Head, Tail)) = list(tryGroundTerm(Head), tryGroundTerm(Tail)) :-
        !.

    tryGroundTerm(T) = T.

clauses
    tryGroundTermList([]) = [].
    tryGroundTermList([H | T]) = [tryGroundTerm(H) | tryGroundTermList(T)].

/*****************************************************************************
            Handle clause listing
******************************************************************************/
clauses
    list_p() :-
        OutputStream = stdio::outputStream,
        foreach PredicateId = clauseBase:getPredicateName_nd() do
            if clauseBase:named_clause(PredicateId, ArgL0, _Body) then
                OutputStream:write("\n % ", PredicateId, '/', list::length(ArgL0), "\n"),
                foreach clauseBase:named_clause(PredicateId, Args, Body) do
                    output::wclause(OutputStream, cmp(PredicateId, Args), Body)
                end foreach
            end if
        end foreach.

clauses
    % list pred for all arities
    list_p(Name) :-
        foreach clauseBase:named_clause(Name, ArgList, Body) do
            output::wclause(outputStream, cmp(Name, ArgList), Body)
        end foreach.

clauses
    % list pred/arity
    list_p(Name, N) :-
        foreach clauseBase:named_clause(Name, ArgList, Body) do
            if N = list::length(ArgList) then
                output::wclause(outputStream, cmp(Name, ArgList), Body)
            end if
        end foreach.

/*****************************************************************************
            Handle assert
******************************************************************************/
class predicates
    convhead : (sterm) -> sterm.
clauses
    convhead(atom(ID)) = cmp(ID, []) :-
        !.

    convhead(T) = T.

predicates
    assertClause : (clauseBase::position, sterm) determ.
clauses
    assertClause(C, cmp(":-", [Head, Body])) :-
        !,
        clauseBase:assertClause(C, convhead(Head), Body).

    assertClause(C, Head) :-
        clauseBase:assertClause(C, convhead(Head), atom("true")).

/*****************************************************************************
            Handle Consult
******************************************************************************/
predicates
    parse_clauses : (scanner::tokl) determ.
clauses
    parse_clauses([]) :-
        !.
    parse_clauses(TokL) :-
        pieParser::s_term(TokL, TokL1, Term),
        assertClause(clauseBase::last, Term),
        parse_clauses(TokL1).

clauses
    consult_p(Filename) :-
        file::existExactFile(Filename),
        TXT = file::readString(Filename),
        scanner::tokl(0, TXT, TokL),
        !,
        parse_clauses(TokL).

    consult_p(_).

clauses
    save_p(Filename) :-
        SavFile = outputStream_file::create(Filename),
        OldStream = stdio::outputStream,
        stdio::outputStream := SavFile,
        try
            list_p()
        finally
            stdio::outputStream := OldStream,
            SavFile:close()
        end try.

/*****************************************************************************
            Handle ReConsult
******************************************************************************/
predicates
    recons_newclause_change : (sterm, sterm) determ.
clauses
    recons_newclause_change(cmp(PredicateId, _TermL), _Body) :-
        not(removedForReconsult_fact(PredicateId)),
        clauseBase:retractAllClauses(PredicateId),
        assert(removedForReconsult_fact(PredicateId)),
        fail.

    recons_newclause_change(cmp(PredicateId, TermL), Body) :-
        clauseBase:assertClause(clauseBase::last, cmp(PredicateId, TermL), Body).

predicates
    recons_newclause : (sterm).
clauses
    recons_newclause(cmp("?-", [Goal])) :-
        !,
        if runGoalTerm(Goal, environment2::new()) then
        end if
        % todo something with the result?
        .

    recons_newclause(cmp(":-", [Head, Body])) :-
        recons_newclause_change(convhead(Head), Body),
        !.

    recons_newclause(Head) :-
        recons_newclause_change(convhead(Head), atom("true")),
        !.

    recons_newclause(Clause) :-
        outputStream:write("\nIllegal Clause "),
        output::wsterm(outputStream, list, Clause),
        outputStream:nl().

predicates
    recons_parse : (scanner::tokl).
% try_string_to_sterm : ( string [in] , pie::sterm Term [out] ) determ.
%   pie::sterm Term [out]
% recons_newclause_metta_std_predicates : () procedure().
    assert_newclause_metta_std_predicates : ().
% clause_memory_to_temp : ( string* ) determ( i ) .
    split_if_condition_clauses : ().
    re_arrange_add_remove_atom_clauses : ().
%clause_memory_to_temp( Slist ) ,
%clause_memory_to_temp( Slist ) ,
    sort_clause_memory_by_fname : ().
    set_transpile_model_type : (sterm).
    is_an_execute_clause : (sterm) determ.
    save_also_to_file : (string).
    last_of_list : (sterml, sterm [out]) determ.
    add_cut_at_the_end_if_there_isnt : (sterm, sterm [out]).
    list_replace_last_elem : (sterml, sterm, sterml [out]) determ.
    choose_fu1_or_fu2_not_dummy : (sterm, sterm, sterm [out]).
    enums_komma_preds : (outputStream).
    write_empty_db_preds : (outputStream).
    clause_memory_to_temp00 : ().
%  Pred_count,
%  clause_memory_tmp( Pred_count, Type_of_pred, Comment , Name_pred, TERMX_trans )  ,
    pred_nest_to_temp_for_sorting : ().
    create_string : (integer, string, string [out]).
    int_to_sort_string : (integer, string [out]).
    ilist_to_sortable_slist : (integer*, string* [out]) determ.
    slist_to_ilist : (string*, integer* [out]) determ.

clauses
    get_clause_position(Pos_in, Predname, Bg, End) :-
        express_position(Predname, Bg, End),
        Pos_in >= Bg,
        Pos_in <= End,
        !.

    get_clause_position(_Pos_in, "", 0, 0) :-
        !.
%---

    create_string(Ar, Hs, Hs2) :-
        Ar = 1,
        Hs2 = string::concat(Hs, " _ "),
        !.

    create_string(Ar, Hs, Underscores_string) :-
        Ar2 = Ar - 1,
        Ar2 > 0,
        Hs2 = string::concat(Hs, "_ , "),
        create_string(Ar2, Hs2, Underscores_string),
        !.
    create_string(_, Hs, Hs) :-
        !.

% start_parse( Metta_str )
    try_string_to_sterm(Bstr, Sxterm) :-
        Sxterm = tryToTerm(Bstr),
        !.
    try_string_to_sterm(_Bstr, nill) :-
        !.

    recons_newclause_z(Tag, Sxterm) :-
        !,
        stdio::write("\n recons ", Tag, "\n"),
        recons_newclause(Sxterm).

%-----
    last_of_list(Flis_body, Last_el) :-
        Lex = list::length(Flis_body),
        Lex > 0,
        Lex2 = Lex - 1,
        Last_el = list::tryGetNth(Lex2, Flis_body),
        !.
%---
    list_replace_last_elem(Lis1, El, Lis2) :-
        Lex = list::length(Lis1),
        Lex > 0,
        Lex2 = Lex - 1,
        Lisfront = list::take(Lex2, Lis1),
        Lis2 = list::append(Lisfront, [El]),
        !.

% temp
    choose_fu1_or_fu2_not_dummy(_Fu1, Fu2, Fu2) :-
        !.

    choose_fu1_or_fu2_not_dummy(Fu1, Fu2, Fu2) :-
        Fu1 = cmp(Fnme_db, Varlis_db_claus),
        Fnme_db = "db_dummy",
        !.
%   Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,
%   Fu3 = cmp( Fnme_db, Varlis_db_claus ) ,  Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,

    choose_fu1_or_fu2_not_dummy(Fu1, Fu2, Fu1) :-
        !.
        % Fu1 = cmp( Fnme_db, Varlis_db_claus ) ,  Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,
        %Fu3 = cmp( Fnme_db, Varlis_db_claus ) ,  Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,

% clause_memory(407,"Clause_implementation-- ","condition_if_check_is_equal_condition_if",
%  cmp(":-",[ cmp("condition_if_check_is_equal_condition_if",[var("List"),var("Sym"),var("Outvar_e3")]),
%    cmp(",",[
%	 cmp("check_is_equal",[var("List"),var("Atomvar#empty_atom"),var("Outvx1")]),
%	  cmp("condition_if_check_is_equal_contains_symbol",[var("Sym"),var("List"),var("Outvx2")]),
%	   cmp("condition_if",[var("Outvx1"),var("Atomvar#False"),var("Outvx2"),var("Outvar_e3")])])
%	   ])).
% TODO
% re arrange this type of clause
% remove_atom_xcount( Was , We ):-
%  xcount( Was , Outvx1 ) ,
%  remove_atom( "&self" , Outvx1 , We ) ,
%  ! .
% Current:
% cmp(":-",[cmp("remove_atom_xcount",[var("Was"),var("We")]),
%   cmp(",",[   cmp("xcount",[var("Was"),var("Outvx1")]),
%     cmp(",",[cmp("remove_atom",[str("&self"),var("Outvx1"),var("We")]),
%  atom("!")])]
% )
% ])
% MUST BE:
%  cmp(":-",[cmp("remove_atom_xcount",[var("Was"),str("True")]),
%     cmp(",",[cmp("retract",[cmp("xcount",[var("Was")])]),atom("!")])])
%
% clause_memory(406,"Subclausex  : ","remove_atom_act_rc",
%  cmp(":-",[ cmp("remove_atom_act_rc",[var("R"),var("C"),var("Outvar_e2")]),
% cmp(",",[cmp("db_dummy",[var("Outv_not_db")]),
% cmp("act_rc_db",[var("R"),var("C"),var("Outvx1")]),
%  cmp("remove_atom",[var("Atomvar#&self"),var("Outv_not_db"),var("Outvx1"),var("Outvar_e2")]),
%  atom("!")])])).
% clause_memory(406,"Subclausex  : ","remove_atom_act_rc",
%cmp(":-",[
% cmp("remove_atom_act_rc",[var("R"),var("C"),var("Outvar_e1")]),
%  cmp(",",[cmp("remove_atom",[var("Atomvar#&self"),var("Outvar_e1")]),atom("!")])])).
%  clause_memory(406,"Subclausex  : ","remove_atom_act_rc",
% cmp(":-",[cmp("remove_atom_act_rc",[var("R"),var("C"),var("Outvar_e1")]),
% cmp(",",[cmp("remove_atom",[var("Atomvar#&self"),var("Outvar_e1")]),atom("!")])])).
% ;empty_mem_candidates( Outp0 ):-
%;  remove_atoms( "mem_candidates" , 3 , Outp0 ) ,
%;  ! .
% first new implement of remove_atom
    write_empty_db_preds(Strea) :-
        clause_memory(_Pred_count, _Type_of_pred1, _Arity, _Comment1, _Fname1, TERMX_trans),
        TERMX_trans = cmp(Operat1, Lis1),
        Operat1 = ":-",
        Lis1 = [El1, El2],
        El1 = cmp(Fnme_head, _Varlis_head),
        % Tag = "remove_atom_" , Lx = string::length( Tag ) ,
        % string::tryFront( Fnme_head,   Lx,    Bg,  Rs_db_tag ) , Bg = Tag ,
        % Le = list::length( Varlis_head ), Le2 = Le - 1 ,
        % Varlis_head2 = list::setNth( Le2, Varlis_head, str("True") ),
        % FirstPart = list::take( Le2, Varlis_head ) ,
        El2 = cmp(_Komma_op, Flis_body),
        Flis_body = [Fu2 | _],
        Fu2 = cmp(Fnme_body, Varlis_body),
        Fnme_body = "remove_atoms",
        Varlis_body = [Db_pred_name, Arityx | _],
        Db_pred_name = atom(Name),
        Arityx = real(Ar),
%	Strea:write( "found empty db pred ", Fnme_head , " ", Name  , " ", toString( Ar ),  "\n" ),
        Ar_i = math::round(Ar),
        create_string(Ar_i, "", Underscores_string),
        Strea:write("\nremove_atoms( \"", Name, "\" , \"True\" ):-  retract( ", Name, "(", Underscores_string, ") ), fail."),
        Strea:write("\nremove_atoms( \"", Name, "\" , \"True\" ):- !.\n"),
        fail,
        !.

    write_empty_db_preds(_Strea) :-
        !.

    re_arrange_add_remove_atom_clauses() :-
        clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, TERMX_trans),
        TERMX_trans = cmp(Operat1, Lis1),
        Operat1 = ":-",
        Lis1 = [El1, El2],
        El1 = cmp(Fnme_head, Varlis_head),
        Tag = "remove_atom_",
        Lx = string::length(Tag),
        string::tryFront(Fnme_head, Lx, Bg, Rs_db_tag),
        Bg = Tag,
        Le = list::length(Varlis_head),
        Le2 = Le - 1,
        Varlis_head2 = list::setNth(Le2, Varlis_head, str("True")),
        FirstPart = list::take(Le2, Varlis_head),
%    El2 = cmp( _Komma_op, Flis_body ) ,    Flis_body = [ Fu1 , Fu2 , _ ] ,
%   El2 = cmp( _Komma_op, Flis_body ) ,    Flis_body = [ Fu1 , Fu2 | _ ] ,
%   choose_fu1_or_fu2_not_dummy( Fu1, Fu2, Fu3 ) ,
        % Fu3 = cmp( Fnme_db, Varlis_db_claus ) ,  Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,
        % Varlis_db_claus2 = list::take( Le_db2 , Varlis_db_claus ) ,
        arity_of_head_vars(Varlis_head2, Arity1),
        Newclause = cmp(":-", [cmp(Fnme_head, Varlis_head2), cmp(",", [cmp("retract", [cmp(Rs_db_tag, FirstPart)]), atom("!")])]),
        retract(clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, _)),
        Com2 = string::concat(Comment1, " part1 "),
        Com3 = string::concat(Comment1, " part2 "),
        asserta(clause_memory(Pred_count, Type_of_pred1, Arity1, Com2, Fname1, Newclause)),
        Newclause2 = cmp(":-", [cmp(Fnme_head, [var("_"), str("False")]), atom("!")]),
        asserta(clause_memory(Pred_count, Type_of_pred1, 2, Com3, Fname1, Newclause2)),
        fail,
        !.

% Copy total with change remove_atom to add_atom
% EXAMPLE  :
% clause_memory(102,406,"Subclausex  : ","add_atom_mem_candidates",
% Use this
% clause_memory(102,406,"Subclausex  : ","add_atom_mem_candidates",cmp(":-",[
%cmp("add_atom_mem_candidates",[var("R"),var("C"),var("Cands"),var("Outvar_e2")]),
%cmp(",",[cmp("mem_candidates",[var("R"),var("C"),var("Cands"),var("Outvx1")]),
%cmp("add_atom",[var("Outvx1"),var("Outvar_e2")]),atom("!")])])).
    re_arrange_add_remove_atom_clauses() :-
        clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, TERMX_trans),
        TERMX_trans = cmp(Operat1, Lis1),
        Operat1 = ":-",
        Lis1 = [El1, El2],
        El1 = cmp(Fnme_head, Varlis_head),
        Tag = "add_atom_",
        Lx = string::length(Tag),
        string::tryFront(Fnme_head, Lx, Bg, Rs_db_tag),
        Bg = Tag,
        Le = list::length(Varlis_head),
        Le2 = Le - 1,
        Varlis_head2 = list::setNth(Le2, Varlis_head, str("True")),
        FirstPart = list::take(Le2, Varlis_head),
%    El2 = cmp( _Komma_op, Flis_body ) ,    Flis_body = [ Fu1 , Fu2 , _ ] ,
%   El2 = cmp( _Komma_op, Flis_body ) ,    Flis_body = [ Fu1 , Fu2 | _ ] ,
%   choose_fu1_or_fu2_not_dummy( Fu1, Fu2, Fu3 ) ,
        % Fu3 = cmp( Fnme_db, Varlis_db_claus ) ,  Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,
        % Varlis_db_claus2 = list::take( Le_db2 , Varlis_db_claus ) ,
        arity_of_head_vars(Varlis_head2, Arity1),
        Newclause = cmp(":-", [cmp(Fnme_head, Varlis_head2), cmp(",", [cmp("assert", [cmp(Rs_db_tag, FirstPart)]), atom("!")])]),
        retract(clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, _)),
        Com2 = string::concat(Comment1, " part1 "),
        Com3 = string::concat(Comment1, " part2 "),
        asserta(clause_memory(Pred_count, Type_of_pred1, Arity1, Com2, Fname1, Newclause)),
        %Newclause2 = cmp(":-",[cmp( Fnme_head ,[var("_"),str("False")]),atom("!")]) ,
        %asserta( clause_memory( Type_of_pred1, Com3 , Fname1, Newclause2 )  ) ,
        fail,
        !.

    re_arrange_add_remove_atom_clauses() :-
        clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, TERMX_trans),
        TERMX_trans = cmp(Operat1, Lis1),
        Operat1 = ":-",
        Lis1 = [El1, El2],
        El1 = cmp(Fnme_head, Varlis_head),
        Tag = "remove_atom_",
        Lx = string::length(Tag),
        string::tryFront(Fnme_head, Lx, Bg, _),
        Bg = Tag,
        Le = list::length(Varlis_head),
        Le2 = Le - 1,
        Varlis_head2 = list::setNth(Le2, Varlis_head, str("True")),
%    El2 = cmp( _Komma_op, Flis_body ) ,    Flis_body = [ Fu1 , Fu2 , _ ] ,
        El2 = cmp(_Komma_op, Flis_body),
        Flis_body = [Fu1, Fu2 | _],
        choose_fu1_or_fu2_not_dummy(Fu1, Fu2, Fu3),
        % Fu1 = cmp( Fnme_db, Varlis_db_claus ) ,  Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,
        Fu3 = cmp(Fnme_db, Varlis_db_claus),
        Le_db = list::length(Varlis_db_claus),
        Le_db2 = Le_db - 1,
        % Fu2 = cmp( _, _ ) , % to secure that it doesnt loop
        Varlis_db_claus2 = list::take(Le_db2, Varlis_db_claus),
        arity_of_head_vars(Varlis_head2, Arity1),
        Newclause = cmp(":-", [cmp(Fnme_head, Varlis_head2), cmp(",", [cmp("assert", [cmp(Fnme_db, Varlis_db_claus2)]), atom("!")])]),
        retract(clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, _)),
        Com2 = string::concat(Comment1, " part1 "),
        Com3 = string::concat(Comment1, " part2 "),
        asserta(clause_memory(Pred_count, Type_of_pred1, Arity1, Com2, Fname1, Newclause)),
%   Newclause2 = cmp(":-",[cmp( Fnme_head ,[var("_"),str("False")]),atom("!")]) ,
%   asserta( clause_memory( Type_of_pred1, Com3 , Fname1, Newclause2 )  ) ,
        fail,
        !.

% clause_memory(406,"Subclausex  : ","add_atom_candidx",cmp(":-",
%  [cmp("add_atom_candidx",[var("R"),var("C"),var("Outvar_e2")]),
%    cmp(",",[
%	 cmp("candidx",[var("R"),var("C"),var("Outvx1")]),
%	  cmp("add_atom",[var("Outvx1"),var("Outvar_e2")]),
%	   atom("!")])])).
    re_arrange_add_remove_atom_clauses() :-
        clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, TERMX_trans),
        TERMX_trans = cmp(Operat1, Lis1),
        Operat1 = ":-",
        Lis1 = [El1, El2],
        El1 = cmp(Fnme_head, Varlis_head),
        Tag = "add_atom_",
        Lx = string::length(Tag),
        string::tryFront(Fnme_head, Lx, Bg, _),
        Bg = Tag,
        Le = list::length(Varlis_head),
        Le2 = Le - 1,
        Varlis_head2 = list::setNth(Le2, Varlis_head, str("True")),
%  El2 = cmp( Komma_op, Flis_body ) ,    Flis_body = [ Fu1 , Fu2 , _ ] ,
        El2 = cmp(Komma_op, Flis_body),
        Flis_body = [Fu1, Fu2 | _],
        choose_fu1_or_fu2_not_dummy(Fu1, Fu2, Fu3),
%   Fu1 = cmp( Fnme_db, Varlis_db_claus ) ,  Le_db = list::length( Varlis_db_claus ), Le_db2 = Le_db - 1,
        Fu3 = cmp(Fnme_db, Varlis_db_claus),
        Le_db = list::length(Varlis_db_claus),
        Le_db2 = Le_db - 1,
        % Fu2 = cmp( _, _ ) ,
        % to secure that it doesnt loop
        Varlis_db_claus2 = list::take(Le_db2, Varlis_db_claus),
        arity_of_head_vars(Varlis_head2, Arity1),
        Newclause =
cmp(":-",
[
cmp(Fnme_head, Varlis_head2),

% todo asserta add-atom-a  add-atom-z
                    cmp(",", [cmp("assert", [cmp(Fnme_db, Varlis_db_claus2)]), atom("!")])
                ]),
        retract(clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, _)),
        Com2 = string::concat(Comment1, " part1 "),
        % Com3 = string::concat( Comment1 , " part2 " ) ,
        asserta(clause_memory(Pred_count, Type_of_pred1, Arity1, Com2, Fname1, Newclause)),
        %Newclause2 = cmp(":-",[cmp( Fnme_head ,[var("_"),str("False")]),atom("!")]) ,
        %asserta( clause_memory( Type_of_pred1, Com3 , Fname1, Newclause2 )  ) ,
        fail,
        !.

    re_arrange_add_remove_atom_clauses() :-
        !.

%---
% uicici cmp(":-",[cmp("condition_if_check_is_equal_condition_if",[var("List"),var("Sym"),str("False")]),
% cmp(",",[cmp("check_is_equal",[var("List"),str("empty_atom"),var("Outvx1")]),
%  cmp(",",[cmp("=",[var("Outvx1"),str("True")]),atom("!")])])])
%  vpiCommondialogs::note( "Succeessss44-1", toString( Clause_if_1 ) ),
% Total copy here but ONLY  Fu1 Fu2 and Fu3
    split_if_condition_clauses() :-
        clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, TERMX_trans),
        TERMX_trans = cmp(Operat1, Lis1),
        Lis1 = [El1, El2],
        Operat1 = ":-",
        El1 = cmp(Fnme_head, Varlis_head),
        El2 = cmp(Komma_op, Flis_body),
        Flis_body = [Fu1, Fu2, Fu3, Last_el | _],
        stdio::write("\naapq: ", toString(Flis_body), "\n"),
        Last_el = cmp(Fnme_cond_if, Lis_cond_if),
        Fnme_cond_if = "condition_if",
        Lis_cond_if = [Var1, At1, Var2, Var3],
        At1 = var(Sx),
        list_replace_last_elem(Varlis_head, At1, Varlis_head2),
        Fu1 = cmp(Funame1, Fu1_varlist),
        last_of_list(Fu1_varlist, Fu1_var_last),
        Clause_line_extra1 = cmp(",", [cmp("=", [Fu1_var_last, atom("True")]), atom("!")]),
        Clause_if_1 = cmp(Operat1, [cmp(Fnme_head, Varlis_head2), cmp(Komma_op, [Fu1, Fu2, Clause_line_extra1])]),
        last_of_list(Varlis_head, Varl_head_last),
        Fu3 = cmp(Funame2, Fu2_varlist),
        last_of_list(Fu2_varlist, Fu2_var_last),
        Clause_line_extra2 = cmp(",", [cmp("=", [Varl_head_last, Fu2_var_last]), atom("!")]),
        Clause_if_2 = cmp(Operat1, [cmp(Fnme_head, Varlis_head), cmp(Komma_op, [Fu3, Clause_line_extra2])]),
        retract(clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, _)),
        Com2 = string::concat(Comment1, " part1 "),
        Com3 = string::concat(Comment1, " part2 "),
        assert(clause_memory(Pred_count, Type_of_pred1, Arity0, Com3, Fname1, Clause_if_2)),
        assert(clause_memory(Pred_count, Type_of_pred1, Arity0, Com2, Fname1, Clause_if_1)),
        fail,
        !.

    split_if_condition_clauses() :-
        clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, TERMX_trans),
        TERMX_trans = cmp(Operat1, Lis1),
        Lis1 = [El1, El2],
        Operat1 = ":-",
        El1 = cmp(Fnme_head, Varlis_head),
        El2 = cmp(Komma_op, Flis_body),
        Flis_body = [Fu1, Fu2, Last_el | _],
        stdio::write("\naapq: ", toString(Flis_body), "\n"),
        Last_el = cmp(Fnme_cond_if, Lis_cond_if),
        Fnme_cond_if = "condition_if",
        Lis_cond_if = [Var1, At1, Var2, Var3],
        At1 = var(Sx),
        list_replace_last_elem(Varlis_head, At1, Varlis_head2),
        Fu1 = cmp(Funame1, Fu1_varlist),
        last_of_list(Fu1_varlist, Fu1_var_last),
        Clause_line_extra1 = cmp(",", [cmp("=", [Fu1_var_last, atom("True")]), atom("!")]),
        Clause_if_1 = cmp(Operat1, [cmp(Fnme_head, Varlis_head2), cmp(Komma_op, [Fu1, Clause_line_extra1])]),
        last_of_list(Varlis_head, Varl_head_last),
        Fu2 = cmp(Funame2, Fu2_varlist),
        last_of_list(Fu2_varlist, Fu2_var_last),
        Clause_line_extra2 = cmp(",", [cmp("=", [Varl_head_last, Fu2_var_last]), atom("!")]),
        Clause_if_2 = cmp(Operat1, [cmp(Fnme_head, Varlis_head), cmp(Komma_op, [Fu2, Clause_line_extra2])]),
        retract(clause_memory(Pred_count, Type_of_pred1, Arity0, Comment1, Fname1, _)),
        Com2 = string::concat(Comment1, " part1 "),
        Com3 = string::concat(Comment1, " part2 "),
        assert(clause_memory(Pred_count, Type_of_pred1, Arity0, Com3, Fname1, Clause_if_2)),
        assert(clause_memory(Pred_count, Type_of_pred1, Arity0, Com2, Fname1, Clause_if_1)),
        fail,
        !.

    split_if_condition_clauses() :-
        !.

%----
% division
%clause_memory_to_temp( [] ):- !.
%clause_memory_to_temp( [ Hs | Slist ] ):-
% retract( clause_memory( Pred_count, Type_of_pred, Comment , Hs, TERMX_trans ) ) , !,
% assert( clause_memory_tmp( Pred_count, Type_of_pred, Comment , Hs, TERMX_trans ) ) ,
%clause_memory_to_temp( Slist ).
%clause_memory_to_temp( [ _Hs | Slist ] ):- !, clause_memory_to_temp( Slist ).
%---
    clause_memory_to_temp00() :-
        retract(clause_memory(Pred_count, Type_of_pred, Arity0, Comment, Hs, TERMX_trans)),
        asserta(clause_memory_tmp(Pred_count, Type_of_pred, Arity0, Comment, Hs, TERMX_trans)),
        fail,
        !.
% assertz( clause_memory_tmp( Pred_count, Type_of_pred, Comment , Hs, TERMX_trans ) ) , fail , !.

    clause_memory_to_temp00() :-
        !.

    int_to_sort_string(Hi, H2s) :-
        Sx = toString(Hi),
        Le = string::length(Sx),
        Diff = 4 - Le,
        Sy = string::create(Diff, "0"),
        H2s = string::concat(Sy, Sx),
        !.

    ilist_to_sortable_slist([], []) :-
        !.
    ilist_to_sortable_slist([H | Cylis2], [H2 | Cylis2_str]) :-
        int_to_sort_string(H, H2),
        ilist_to_sortable_slist(Cylis2, Cylis2_str).

    slist_to_ilist([], []) :-
        !.

    slist_to_ilist([H | Cxlis2p], [Yi | Cxlis2]) :-
        Yi = tryToTerm(integer, H),
        !,
        slist_to_ilist(Cxlis2p, Cxlis2).

%clause_memory_to_temp( Slist ) ,
%clause_memory_to_temp( Slist ) ,
    % file::save( "results\\transpiled_intermediate.pro", memory_clauses ) ,
%  Strea = outputStream_file::createUtf8( "results\\metta_to_prolog_transpiled.pro" ),
    % retractall( outp_file( _ ) ) , assert( outp_file( Strea ) ) ,
    %vpiCommondialogs::note( "UNsorted list ", toString( Fnamelis ) ),
    %vpiCommondialogs::note( "sorted list ", toString(Sxlist) ),
    % sort
%  Sxlist = list::sort( Fnamelis , ascending() ) ,
% intermediate
%  findall( Fname,  clause_memory( _, _Type_of_pred, _Comment , Fname, _TERMX_trans ), Fnamelis ) ,
%  Sxlist = list::sort( Fnamelis , descending() ) ,
%  clause_memory_to_temp( Sxlist ) ,
    %  clause_memory_tmp( Pred_count, Type_of_pred, Comment , Name_pred, TERMX_trans )  ,
    %predicate_for_sort( Pred_count , Pred , Pred_total , Predlist )  ,
    %Le = string::length( Pred ),
    % string::tryFront( Name_pred , Le, Begin, _End) , Begin = Pred ,
    sort_clause_memory_by_fname() :-
        clause_memory_to_temp00(),
        fail.

% is_database_pred
    sort_clause_memory_by_fname() :-
        Clis = [ El_count || clause_memory_tmp(El_count, is_database_pred, _, _, _, _) ],
        Cylis2 = list::removeDuplicates(Clis),
        ilist_to_sortable_slist(Cylis2, Cylis2_str),
        Cxlis2p = list::sort(Cylis2_str, ascending()),
        % vpiCommondialogs::note("aa1", toString( Cxlis2p ) ),
        slist_to_ilist(Cxlis2p, Cxlis2),
        % vpiCommondialogs::note("aa2", toString( Cxlis2 ) ),
        list::memberIndex_nd(Pred_count, _, Cxlis2),
% ACtually this is a little bit double
%  findall( Fname,  clause_memory_tmp( Pred_count, is_database_pred, _ , Fname, _ ), Fnamelis ) ,
%  Fnamelis2 = list::sort( Fnamelis , descending() ) ,
%  list::memberIndex_nd( Name_pred , _ , Fnamelis2 ) ,
        retract(clause_memory_tmp(Pred_count, is_database_pred, Arity0, Comment, Name_pred, TERMX_trans)),
        assertz(clause_memory(Pred_count, is_database_pred, Arity0, Comment, Name_pred, TERMX_trans)),
        fail,
        !.

    sort_clause_memory_by_fname() :-
        Clis = [ El_count || predicate_for_sort(El_count, _, _, _) ],
        Cylis2 = list::removeDuplicates(Clis),
        ilist_to_sortable_slist(Cylis2, Cylis2_str),
        Cxlis2p = list::sort(Cylis2_str, descending()),
        slist_to_ilist(Cxlis2p, Cxlis2),
        list::memberIndex_nd(Pred_count, _, Cxlis2),
        Fnamelis = [ Fname || clause_memory_tmp(Pred_count, _, _, _, Fname, _) ],
        Fnamelis2 = list::sort(Fnamelis, descending()),
        list::memberIndex_nd(Name_pred, _, Fnamelis2),
        retract(clause_memory_tmp(Pred_count, Type_of_pred, Arity0, Comment, Name_pred, TERMX_trans)),
        assertz(clause_memory(Pred_count, Type_of_pred, Arity0, Comment, Name_pred, TERMX_trans)),
        fail,
        !.

    sort_clause_memory_by_fname() :-
        Clis = [ El_count || clause_memory_tmp(El_count, _, _, _, _, _) ],
%  Cxlis = list::sort( Clis , descending() ) ,  Cxlis2 = list::removeDuplicates( Cxlis ),
        Cylis2 = list::removeDuplicates(Clis),
        ilist_to_sortable_slist(Cylis2, Cylis2_str),
        Cxlis2p = list::sort(Cylis2_str, descending()),
        slist_to_ilist(Cxlis2p, Cxlis2),
        list::memberIndex_nd(Pred_count, _, Cxlis2),
        Fnamelis = [ Fname || clause_memory_tmp(Pred_count, _, _, _, Fname, _) ],
        Fnamelis2 = list::sort(Fnamelis, descending()),
        list::memberIndex_nd(Name_pred, _, Fnamelis2),
        retract(clause_memory_tmp(Pred_count, Type_of_pred, Arity0, Comment, Name_pred, TERMX_trans)),
% assertz ?
        assertz(clause_memory(Pred_count, Type_of_pred, Arity0, Comment, Name_pred, TERMX_trans)),
        fail,
        !.

    sort_clause_memory_by_fname() :-
        Fnamelis = [ Fname || clause_memory_tmp(_, _, _, _, Fname, _) ],
        Fnamelis2 = list::sort(Fnamelis, descending()),
        list::memberIndex_nd(Name_pred, _, Fnamelis2),
        retract(clause_memory_tmp(Pred_count, Type_of_pred, Arity0, Comment, Name_pred, TERMX_trans)),
% assertz ?
        assertz(clause_memory(Pred_count, Type_of_pred, Arity0, Comment, Name_pred, TERMX_trans)),
        fail,
        !.

    sort_clause_memory_by_fname() :-
        !.

%   recons_newclause_z( "transpile_metta_sub" , TERMX_trans ) ,
    assert_newclause_metta_std_predicates() :-
%   stdio::write( "\n" ) ,
        retractall(std_metta_pred_tmp(_, _, _)),
        std_metta_pred(Funcname_s, Arity, Func_clause),
%   asserta( clause_memory( is_standard_pred , "std_pred" , Func_clause ) ) ,
        asserta(std_metta_pred_tmp(Funcname_s, Arity, Func_clause)),
        % recons_newclause_z( Funcname_s, Func_clause ) ,
        fail,
        !.

    assert_newclause_metta_std_predicates() :-
%   stdio::write( "\n" ) ,
        std_metta_pred_tmp(Funcname_s, Arity0, Func_clause),
%   asserta( clause_memory( is_standard_pred , "std_pred" , Func_clause ) ) ,
        asserta(clause_memory(0, is_standard_pred, Arity0, "std_pred", Funcname_s, Func_clause)),
        % recons_newclause_z( Funcname_s, Func_clause ) ,
        fail,
        !.
    assert_newclause_metta_std_predicates() :-
        !.

%recons_newclause_metta_std_predicates():-
    %  stdio::write( "\n" ) ,
    %  std_metta_pred( Funcname_s, Func_clause ) ,
    %  recons_newclause_z( Funcname_s, Func_clause ) ,  fail , ! .
%recons_newclause_metta_std_predicates():-!.
    recons_parse([]) :-
        !.
% tryToTerm
    recons_parse(TokL) :-
        pieParser::s_term(TokL, TokL1, Term),
        stdio::write("uicici ", toString(Term), "\n"),
        recons_newclause(Term),
        recons_parse(TokL1).

clauses
    reconsult_p(Filename) :-
        retractAll(removedForReconsult_fact(_)),
        if file::existExactFile(Filename) then
            TXT = file::readString(Filename),
            reconsult_text(TXT)
        end if.

clauses
    reconsult_text(TXT) :-
        retractAll(removedForReconsult_fact(_)),
        scanner::tokl(0, TXT, TokL),
        recons_parse(TokL).

/*****************************************************************************
    Misc help predicates for implementing standard predicates
******************************************************************************/
clauses
    writeterml(Display, L) :-
        foreach T in L do
            output::wterm(outputStream, Display, T)
        end foreach.

/*****************************************************************************
    Implementation of trace
******************************************************************************/
predicates
    report_redo : (string, term*) multi.
clauses
    report_redo(_, _).
    report_redo(PredicateId, TermL) :-
        showtrace("REDO: ", PredicateId, TermL),
        fail.

predicates
    showtrace : (string, string, term*).
clauses
    showtrace(STR, PredicateId, TermL) :-
        outputStream:write("Trace: >> ", STR),
        output::wterm(outputStream, write, cmp(PredicateId, TermL)),
        outputStream:nl(),
        !.

%- backtracking -----------------------------------------------------------------------------------
% resetting variables at backtrack.
predicates
    getCurrentChoicePoint : () -> resetPoint.
clauses
    getCurrentChoicePoint() = RP :-
        resetPoint_fact(RP),
        !.

    getCurrentChoicePoint() = _ :-
        raise_pie_exception(predicate_name(), " Empty stack").

clauses
    backtrackpoint() :-
        RP = resetPointClass::new(), % Stack on forward
        (asserta(resetPoint_fact(RP))
            or % reset on backtrack
            reset(RP)
            and fail).

predicates
    reset : (resetPoint CP).
clauses
    reset(RP) :-
        TRP = getCurrentChoicePoint(),
        retractAll(resetPoint_fact(TRP)),
        if RP = TRP then
            RP:reset()
        else
            TRP:reset(),
            reset(RP)
        end if.

class predicates
    choice : (A, A) -> A multi.
clauses
    choice(A, _) = A.
    choice(_, A) = A.

predicates
    handle_assert : (clauseBase::position, sterm, environment2).
clauses
    handle_assert(Poscode, Term, Env) :-
        CALL = variable::newAnonymous(),
        unify_sterm(CALL, Term, Env),
        STerm = stermCreator::toSTerm(CALL),
        assertClause(Poscode, STerm),
        fail.
        % Remove generated identifiers from environment

    handle_assert(_, _, _).

clauses
    retract_p(cmp(":-", [cmp(ID, TermL), Body])) :-
        !,
        clauseBase:named_clause(ID, STermL, SBody),
        backtrackPoint(),
        Env = environment2::new(),
        unify_sterml(TermL, STermL, Env),
        unify_sterm(Body, SBody, Env),
        clauseBase:retractclause(cmp(ID, STermL), SBody).

    retract_p(cmp(":-", [var(HeadVar), Body])) :-
        !,
        clauseBase:clause(SHead, SBody),
        backtrackPoint(),
        Env = environment2::new(),
        unify_sterm(var(HeadVar), SHead, Env),
        unify_sterm(Body, SBody, Env),
        clauseBase:retractclause(SHead, SBody).

    retract_p(cmp(ID, TermL)) :-
        clauseBase:named_clause(ID, TermL1, atom("true")),
        % facts
        backtrackPoint(),
        Env = environment2::new(),
        unify_sterml(TermL, TermL1, Env),
        clauseBase:retractclause(cmp(ID, TermL1), atom("true")).

predicates
    unify_sterml : (terml, sterml, environment2) determ.
clauses
    unify_sterml([], [], _) :-
        !.
    unify_sterml([Term1 | TL1], [Term2 | TL2], Env) :-
        unify_sterm(Term1, Term2, Env),
        unify_sterml(TL1, TL2, Env).

predicates
    unify_sterm : (term, sterm, environment2) determ.
clauses
    unify_sterm(Term, STerm, Env) :-
        unify(Term, Env:mk_term(STerm)).

clauses
    unify(A, B) :-
        unify_1(evalOuterMost(A), evalOuterMost(B)).

predicates
    unify_1 : (term A, term B) determ.
clauses
    unify_1(list(H1, T1), list(H2, T2)) :-
        !,
        unify(H1, H2),
        unify(T1, T2).

    unify_1(cmp(ID, L1), cmp(ID, L2)) :-
        !,
        unify_list(L1, L2).

    unify_1(T, T) :-
        !.

    unify_1(T, var(Var)) :-
        !,
        Var:setTerm(T, getCurrentChoicePoint()).

    unify_1(var(Var), T) :-
        !,
        Var:setTerm(T, getCurrentChoicePoint()).

    unify_1(atom(T), str(T)).

    unify_1(str(T), atom(T)).

predicates
    unify_list : (term* AL, term* BL) determ.
clauses
    unify_list([], []).
    unify_list([A | AL], [B | BL]) :-
        unify(A, B),
        unify_list(AL, BL).

predicates
    unify_body : (sterm, environment2, programControl::stackMark) nondeterm.
clauses
    unify_body(atom("true"), _, _) :-
        !.

    unify_body(cmp(",", [Term1, Term2]), Env, BTOP) :-
        !,
        unify_body(Term1, Env, BTOP),
        unify_body(Term2, Env, BTOP).

    unify_body(atom("!"), _, BTOP) :-
        !,
        programControl::cutBackTrack(BTOP).

    unify_body(cmp(";", [Term1, Term2]), Env, BTOP) :-
        !,
        Term = choice(Term1, Term2),
        backtrackpoint(),
        unify_body(Term, Env, BTOP).

    unify_body(cmp("not", [Term]), Env, _) :-
        backtrackPoint(),
        RS = getCurrentChoicePoint(),
        BTOP = programControl::getBackTrack(),
        unify_body(Term, Env, BTOP),
        reset(RS),
        !,
        fail.

    unify_body(cmp("not", _), _, _) :-
        !.

    unify_body(cmp("call", [Term]), Env, _) :-
        !,
        BTOP = programControl::getBackTrack(),
        unify_body(Term, Env, BTOP),
        backtrackpoint().

    unify_body(cmp("assert", [Term]), Env, _) :-
        !,
        handle_assert(clauseBase::last, Term, Env).

    unify_body(cmp("asserta", [Term]), Env, _) :-
        !,
        handle_assert(clauseBase::first, Term, Env).

    unify_body(cmp("assertz", [Term]), Env, _) :-
        !,
        handle_assert(clauseBase::last, Term, Env).

    unify_body(cmp(PredicateId, TermL), Env, _) :-
        Args = Env:mk_term_list(TermL),
        trace_call(PredicateId, evalTermList(Args)).

    unify_body(var(ID), Env, _) :-
        !,
        VarTerm = Env:lookUpOrCreate(ID),
        trace_call_var(evalOuterMost(VarTerm)).

    unify_body(atom(PredicateId), _, _) :-
        trace_call(PredicateId, []).

predicates
    trace_call_var : (term) nondeterm.
clauses
    trace_call_var(atom(PredicateId)) :-
        trace_call(PredicateId, []).

    trace_call_var(cmp(PredicateId, TermL)) :-
        trace_call(PredicateId, TermL).

predicates
    trace_call : (string, term*) nondeterm.
clauses
    trace_call(PredicateId, TermL) :-
        if retract(stopExecution_flag) then
            halt_p()
        end if,
        trace_call1(PredicateId, TermL).

predicates
    trace_call1 : (string, term*) nondeterm.
clauses
    trace_call1(PredicateId, TermL) :-
        false = traceOn,
        !,
        CallName = string::toLowerCase(PredicateId),
        call(CallName, TermL).

    trace_call1(PredicateId, TermL) :-
        CallName = string::toLowerCase(PredicateId),
        showtrace("CALL: ", PredicateId, TermL),
        call(CallName, TermL),
        report_redo(CallName, TermL),
        showtrace("RETURN: ", PredicateId, TermL).

    trace_call1(PredicateId, TermL) :-
        showtrace("FAIL: ", PredicateId, TermL),
        fail.

predicates
    trace_FunctionCall : (string FunctionId, invokeFunction Invoker, term* ArgList) -> term Return.
clauses
    trace_functionCall(FunctionId, Invoker, TermL) = ReturnTerm :-
        false = traceOn,
        !,
        CallName = string::toLowerCase(FunctionId),
        ReturnTerm = callFunction(CallName, Invoker, TermL).

    trace_FunctionCall(FunctionId, Invoker, TermL) = ReturnTerm :-
        CallName = string::toLowerCase(FunctionId),
        showtrace("FUNCTION CALL: ", FunctionId, TermL),
        ReturnTerm = callFunction(CallName, Invoker, TermL),
        showtrace("RETURN: ", FunctionId, [ReturnTerm]).

predicates
    callFunction : (string FunctionId, invokeFunction Invoker, term* Args) -> term Result.
clauses
    callFunction(FunctionId, Invoker, ArgList) = ReturnTerm :-
        ReturnTerm = Invoker(This, FunctionId, ArgList).

facts
    isPredicateSupportedResponder : isPredicateSupportedResponder := defaultIsPredicateSupportedResponder.
    isFunctionSupportedResponder : isFunctionSupportedResponder := defaultIsFunctionSupportedResponder.

class predicates
    defaultIsPredicateSupportedResponder : isPredicateSupportedResponder.
clauses
    defaultIsPredicateSupportedResponder(_, _) = _ :-
        fail.

class predicates
    defaultIsFunctionSupportedResponder : isFunctionSupportedResponder.
clauses
    defaultIsFunctionSupportedResponder(_, _) = _ :-
        fail.

predicates
    call : (string PredicateId, term* Args) nondeterm.
clauses
    call(PredicateId, ArgList) :-
        Invoker = isPredicateSupportedResponder(This, PredicateId),
        !,
        Invoker(This, PredicateId, ArgList),
        backtrackpoint().

    call(PredicateId, ArgList) :-
        CallArity = list::length(ArgList),
        Invoker = stdPredicate::isStdPredicate(PredicateId, CallArity),
        !,
        Invoker(This, PredicateId, ArgList),
        backtrackpoint().

    call(CmpId, [T1, T2]) :-
        Cmp = isBinaryCmp(CmpId),
        !,
        applyCmp(Cmp, T1, T2).

    call("clause", [Head, Body]) :-
        !,
        clauseBase:clause(SHead, SBody),
        backtrackpoint(),
        Env = environment2::new(),
        unify_sterm(Head, SHead, Env),
        unify_sterm(Body, SBody, Env).

% ReturnTerm = Invoker(This, FunctionId, ArgList).
    call(ID, TermL) :-
        BTOP = programControl::getBackTrack(),
        ClauseList = clauseBase:named_clauseList(ID),
        !,
        clauseChain::clause(cmp(_Name, TermL1), Body) = getClause_nd(ClauseList),
        Env = environment2::new(),
        unify_sterml(TermL, TermL1, Env),
        unify_body(Body, Env, BTOP).

    /* unknown clause check */
    call(Clause, TermL) :-
        not(clauseBase:clause(cmp(Clause, _), _)),
        !,
        outputStream:write("Unknown clause found "),
        output::wterm(outputStream, write, cmp(Clause, TermL)),
        outputStream:nl(),
        retract(topmostStackmark(TOP)),
        programControl::cutBackTrack(TOP),
        outputStream:write("Execution terminated"),
        outputStream:nl(),
        fail.

predicates
    getClause_nd : (clauseChain::clause* ClauseList) -> clauseChain::clause Clause nondeterm.
clauses
    getClause_nd([C]) = C :-
        !.
    getClause_nd([C | _]) = C :-
        backtrackpoint().
    getClause_nd([_ | L]) = getClause_nd(L).

clauses
    run(L) :-
        retractAll(stopExecution_flag),
        L <> "",
        solutionCount := 0,
        Term = parse(L),
        try
            handle_usergoal(Term)
        finally
            retractAll(topmostStackmark(_))
        end try,
        fail.

    run(_).

predicates
    % add clauses and run goal
    handle_usergoal : (sterm) determ.
clauses
    handle_usergoal(cmp(":-", [Head, Body])) :-
        !,
        Head1 = convhead(Head),
        clauseBase:assertClause(clauseBase::last, Head1, Body),
        outputStream:write("Asserted "),
        output::wsterm(outputStream, write, cmp(":-", [Head1, Body])),
        outputStream:nl().

    handle_usergoal(Term) :-
        Env = environment2::new(),
        BTOP = programControl::getBackTrack(),
        backtrackPoint(),
        retractAll(topmostStackmark(_)),
        assert(topmostStackmark(BTOP)),
        unify_body(Term, Env, BTOP),
        if Env:isEmpty() then
            outputStream:write("True"),
            programControl::cutBackTrack(BTOP)
        else
            Env:write(outputStream)
        end if,
        outputStream:nl(),
        solutionCount := solutionCount + 1,
        fail.

    handle_usergoal(_) :-
        tellSeeSupport::seen(),
        tellSeeSupport::told(),
        wsol(solutionCount).

clauses
    runGoal(GoalStr, Env) :-
        GoalTerm = parse(GoalStr),
        runGoalTerm(GoalTerm, Env).

clauses
    runGoalTerm(GoalTerm, Env) :-
        retractAll(stopExecution_flag),
        BTOP = programControl::getBackTrack(),
        backtrackPoint(),
        retractAll(topmostStackmark(_)),
        assert(topmostStackmark(BTOP)),
        unify_body(GoalTerm, Env, BTOP).

    runGoalTerm(_, _) :-
        closeHandles(),
        retractAll(topmostStackmark(_)),
        fail.

clauses
    cut_p() :-
        closeHandles(),
        retract(topmostStackmark(BTOP)),
        !,
        programControl::cutBackTrack(BTOP).

    cut_p().

class predicates
    closeHandles : ().
clauses
    closeHandles() :-
        tellSeeSupport::seen(),
        tellSeeSupport::told().

clauses
    assertClauses(ClausesStr) :-
        scanner::tokl(0, ClausesStr, TokL),
        assertClauses_tokl(TokL).

predicates
    assertClauses_tokl : (scanner::tokl).
clauses
    assertClauses_tokl([]) :-
        !.

    assertClauses_tokl(TokL) :-
        pieParser::s_term(TokL, RestTokL, Term),
        assertClause(clauseBase::last, Term),
        !,
        assertClauses_tokl(RestTokL).

    assertClauses_tokl(TokL) :-
        outputStream:write(">> Syntax error\n", TokL, "\n").

predicates
    wsol : (integer).
clauses
    wsol(0) :-
        !,
        outputStream:write("No solutions\n").

    wsol(1) :-
        !,
        outputStream:write("1 Solution\n").

    wsol(N) :-
        outputStream:write(N, " Solutions\n").

clauses
    parse(STR) = Term :-
        scanner::tokl(0, STR, TokL),
        pieParser::s_term(TokL, _, Term).

clauses
    terminateExecution() :-
        retractAll(stopExecution_flag),
        assert(stopExecution_flag).

clauses
    getPredicateNames() = list::removeDuplicates([ X || clauseBase:clause(cmp(X, _), _) ]).

clauses
    retractAll() :-
        clauseBase := clauseBase::new().

clauses
    halt_p() :-
        cut_p(),
        fail.

clauses
    normalizeVar(T) = variable::normalize(T).

class predicates
    termToString : (term Term) -> string String.
clauses
    termToString(T) = output::termToString(display, T).

clauses
    presenter_termBase(Value) = presenter::noExpand(Str, toAny(Value)) :-
        Str = output::stermToString(list, uncheckedConvert(sterm, Value)).

% PARSE METTA
class predicates
    set_file : (string).
    filter_out_comments : (string, string [out]) determ.
%    tokenize : (source, tokl [out]) determ.
% parse_report : ( tokl, expr,   tokl ) determ( i , o , o ).
    parse_report : (tokl, sterm [out], tokl [out]) determ.
    empty_errors : ().
    set_current_position : (core::positive, core::positive).
% syntax_errors_nd : ( string, cursortoq  ) nondeterm( o, o ).

predicates
    parse_interpet_metta_program : (tokl, integer) determ.
    parse_metta_program : (tokl, integer) determ.
%    parse_metta1 : ( source  [in] , sterm [out] ) determ.
    append_for_match_all : (string, sterml, sterml, sterml [out]).
    is_a_std_predicate : (string) determ.
    is_not_a_std_predicate : (string) determ.
    the_predicate_can_fail : (string) determ.
    append_comparison_if_it_is_a_procedure_result_pred : (string, sterml, sterml, sterml, sterml [out]).
    assert_catch_for_match_all : (integer, string, integer, string, string, sterm, sterm).
    create_db_arg : (sterm, sterm [out], sterm [out], integer [out]) determ.
    add_non_present_variabels : (string*, string*, string* [out]).
    create_db_predicate : (integer, string*, string, string*, string*, string, string [out], string [out], string [out], sterm [out], sterm [out],
        string [out]) determ.

class predicates
% parse : ( tokl, expr ) determ( i , o ).
% parse : ( tokl, sterm ) determ( i , o ).
% parse_rest : ( tokl , integer ) determ( i , i ).
    clean_var : (string, string [out]) determ.
% has_no_sub_nesting2 : ( atom_list , integer, integer ) determ( i, i, o ).
    amount_of_sub_nesting2 : (sterml, integer, integer [out]) determ.
    amount_of_linear_nesting : (sterml, integer, integer [out]) determ.
    arity_of_list2 : (sterml, integer, integer [out]) determ.
    arity_of_pred : (sterml, integer [out]) determ.
    try_get_first_predname : (sterml, string [out], slist [out], integer* [out], sterml [out]).
    try_peal_off_outer : (sterm, sterm [out]).
    judge_db_output_term : (sterm, integer [out]).
% asser_match_db_predicate2 : ( string, sterml ) determ( i, i ).
% asser_match_db_predicate2 : ( integer, integer, integer, string, sterm, sterm ) determ( i,i,i,i, i , i ).
    asser_match_db_predicate : (integer, integer, integer, integer, sterml) determ.
% change_db_pred_name : ( string, string ) procedure( i, o ).
    add_db_input_list : (string*, string*, string, string* [out]).
    db_var_list : (string*, string*, string, string* [out]).
    hide_fail_if_is_inside_db_pred : (string, string, string, string*, string, string [out]) determ.
%variabels_for_predicate : ( string , integer, integer, string* ) determ( i, i, i, o ).
%variabels_below_level : ( string, integer, integer* , string*, string* ) determ( i, i, i , i , o ).
%variabels_for_level_list : ( integer, integer*, string*, string* ) determ( i, i, i, o ).
    vars_slist_to_vars_slist : (string*, string* [out]) determ.
    vars_slist_to_vars_list : (string*, sterml [out]) determ.
    vars_slist_to_free_slist : (string*, string* [out]) determ.
    metta_vars_list_to_prolog_vars_list : (sterml, sterml [out]) determ.
    vars_list_to_varnames_slist : (sterml, string* [out], string* [out]) determ.
% get_vars : ( sterml, string*, string* ) determ( i,i,o).
    get_vars : (sterml, integer, string* [out], integer* [out], sterml [out], integer* [out]) determ.
    debug_tag : (string, string [out]).
    get_indent : (string [out]).
    element_to_string : (sterm, string [out], string [out]).
    newline_for_pred : (string, sterml, string [out]).
    var_list_to_prolog_vars_string : (sterml, string, string [out]) determ.
%  cmp_element_to_metta_string : (sterm, string [out], string [out]).
%     cmp_elements_to_metta_string : (sterml, string, string [out]) determ.
% get_vars( [ var( Nm ) | Lis ], Cou , [ Nm | Lisvars ], [ Cou | Lisnums ] , Lis_nonvars, Lis_nonnums ):- !,  Cou2 = Cou + 1,
    get_last_string : (string, string [out]) determ.
    write_out : (slist) determ.
    expect : (cursortoq, tokl, tokl) determ anyflow.
    syntax_error : (string, tokl) determ.
    remove_dollar : (string, string [out]).
    first_char_upper : (string, string [out]).

class predicates
    s_atom_list : (tokl, tokl [out], sterml [out]) determ.
    s_atom : (tokl, tokl [out], sterm [out]) determ.
% s_operator : ( tokl , tokl , operator ) determ(i,o,o).
    s_operator : (tokl, tokl [out], string [out]) determ.
    operator_to_name : (string, string ) determ( i , o ) .
    operator_to_atom : (string, tok ) determ( o , i ) .
%s_expr : ( tokl , tokl , expr ) determ(i,o,o).
    s_expr : (tokl, tokl [out], sterm [out]) determ.

class predicates
    is_a_space : (char) determ.
    is_a_space_s : (string) determ.
    is_a_space_i : (unsigned16) determ.
    is_comment_start : (unsigned16) determ.
    read_til_end : (string, string [out], integer [out]) determ.
    is_not_a_space_i : (unsigned16) determ.
    get_frontChar_i : (string, unsigned16 [out], string [out]) determ.
    get_frontChar_2i : (string, unsigned16 [out], unsigned16 [out], string [out]) determ.
    scan : (cursorq, source, tokl [out]) determ.
    skip_spaces : (string, string [out], integer, integer [out]) determ.
    string_tokenq : ( string , integer, integer , tok  ) determ ( i , i , i , o ) .


    get_fronttoken : (string, string [out], string [out], integer [out]) determ.
    is_alfa : (core::unsigned16) determ.
    is_alfa_string : (string) determ.
    string_replace_tag : (string, string, string, string [out]) determ.
    string_replace_tag_in_pos : (string, integer, string, string, string [out]) determ.
    change_adhoc : (string, string [out]) determ.
    str_after : (string, string, string [out], string [out]) determ.
    which_list_for_second : (integer, slist, slist, slist [out]) determ.
% assert_var : ( string ) determ(i).
% assert_vars : ( integer, atom_list ) determ( i, i ).
% assert_variabels_concerned : ( integer ) determ( i ).
% assert_variabels_concerned_list : ( ilist ) determ( i ).
    add_when_not_in_list : (slist, slist, slist [out]) determ.
    filter_which_are_not_in_list : (slist, slist, slist [out], slist [out]) determ.
    is_member : (string, slist) determ.
    is_not_member : (string, slist) determ.
    reverse_slist : (slist, slist, slist [out]) determ.
    clean_var_list : (slist, slist [out]) determ.
    position_of_last_occurance : (string, string, integer, integer [out]) determ.
    first_up : (string, string [out]) determ.
    trim_tags : (string, string, string [out]) determ.
    trim_string : (string, string [out]) determ.
    concat_list : (slist, string, string [out]) determ.
    funct_constr_name_or_std_pred : (operator, string [out]) determ.
% komma_separator_if_more_resting : ( atom_list, string ) procedure( i,o ).
    komma_separator_if_more_resting : (terml, string [out]).
    debug_commment_str : (string, string [out]).
% predicate_nesting_below_primlevel2_name : ( integer, integer, string, string* ) nondeterm( i,i,o,o).
% predicate_nesting_below_primlevel3_level : ( integer, integer, integer ) nondeterm( i,i,o).
    nested_pred_memory_for_level : (integer [out], integer, string [out]) nondeterm.

% sequential_variabels_for_predicate : ( integer, integer , string* , string* ) procedure( i,i,i,o).
%class facts - tmpx
%count : ( integer ) determ .
class predicates
    debug_Note : (string, string).
    increment : (integer [out]) determ.
    increment_count : (integer [out]) determ.
    write_nspaces : (integer) determ.
% length_of_list : ( atom_list , integer, integer ) determ( i,i,o).
    length_of_list : (terml, integer, integer [out]) determ.
    write_level : (integer) determ.
    str_before : (string, string, string [out]) determ.
% predicate_nesting : ( string, integer, integer, integer, string* ).
% asser_pred_nest : ( integer, integer, integer, integer, string, string* , integer ) procedure( i, i,i,i,i,i, i ) .
    pred_nest_to_ordered : (integer).
    enum_predicate : (integer, string, integer, string [out]).
% transpile_metta_sub_tpro_sub  term
% transpile_metta_sub_tpro_sub : ( atom_list, terml ) determ(  i, o ).
    display_errors : ().
% go_to_error_position : () procedure().
    find_error_position : (integer [out], integer [out], integer [out]).
% empty_errors():- !,    retractall( tok_error( _ , _ ) ).
    assert_string_lines : (integer, integer, string) determ.
    calculate_line_position : (integer, integer [out], integer [out]).

% memory_to_prolog_file : () determ().
clauses
% transpile_metta_sub_tpro_sub( [], [] ):- !.
    set_debug(Bo) :-
        retractall(is_debug(_)),
        assert(is_debug(Bo)).

    set_current_position(Pos, Line) :-
        retractall(cur_position(_, _)),
        assert(cur_position(Pos, Line)),
        !.

    get_last_string(_Def, Last) :-
        lasts_metta_string(Last),
        !.
    get_last_string(Def, Def) :-
        !.

    set_file(Fn) :-
        retractall(last_file(_)),
        assert(last_file(Fn)).

% TURN off here!!21-12-2025
    filter_out_comments(Resu, Resu) :-
        !.

    filter_out_comments(AA, Result) :-
        string5x::searchstring(AA, ";", Pos),
        P2 = Pos - 1,
        string5x::frontstr(P2, AA, Bg, Res),
        string5x::searchstring(Res, "\n", Pos3),
        P32 = Pos3 - 1,
        string5x::frontstr(P32, Res, _Bg3, Res3),
        !,
        string5x::concat(Bg, Res3, C2),
        filter_out_comments(C2, Result),
        !.

    filter_out_comments(AA, Result) :-
        string5x::searchstring(AA, "%", Pos),
        P2 = Pos - 1,
        string5x::frontstr(P2, AA, Bg, Res),
        string5x::searchstring(Res, "\n", Pos3),
        P32 = Pos3 - 1,
        string5x::frontstr(P32, Res, _Bg3, Res3),
        !,
        string5x::concat(Bg, Res3, C2),
        filter_out_comments(C2, Result),
        !.

    filter_out_comments(Resu, Resu) :-
        !.

% a fair implement
    start_pars_pro(Prolog_clause_txt, Termz) :-
        scanner::tokl(0, Prolog_clause_txt, TokL),
        pieParser::s_term(TokL, _TokL1, Termz),
        !.
%        stdio::write("uicici ", toString(Term), "\n"),

% uici
    start_pars_pro(_Prolog_clause, cmp("dumzz", [var("Bb")])) :-
        !.

%enum_predicate( Cou, Pred, Pred2 ):-
%   Pred2 = string::concat( Pred, "e" ) , !.
% Question if we should maintain this or not
    enum_predicate(Cou, Pred, Ariti, Pred2) :-
        _P = string::search(Pred, "komma"),
        Pred2 = string::concat(Pred, toString(Cou)),
        assert(enum_pred_mem(Cou, Pred, Ariti, Pred2)),
        !.

%enum_predicate( Cou, Pred, Pred2 ):- _P = string::search( Pred, "groupe" ) , Pred2 = string::concat( Pred, toString( Cou ) ) ,
% assert( enum_pred_mem( Cou, Pred, Pred2 ) ) , !.
% enum_predicate( Cou, Pred, Pred2 ):- _P = string::search( Pred, "match_all" ) , Pred2 = string::concat( Pred, toString( Cou ) ) , !.
%enum_predicate( Cou, Pred, Pred2 ):- _P = string::search( Pred, "udoku_puzzle_state" ) , Pred2 = string::concat( Pred, toString( Cou ) ) , !.
%enum_predicate( Cou, Pred, Pred2 ):- Pred = "sudoku_puzzle_state"  , Pred2 = string::concat( Pred, toString( Cou ) ) , !.
    enum_predicate(Cou, Pred, _Ariti, Pred2) :-
        %  Pred = "sudoku_puzzle_state"  ,
        is_match_db_pred(query1, _Db_el, _, _, _, Cou, Predx1, Type_match, Pname1, _, Db_result1, _Judge),
        Predx1 = Pred,
        Pred2 = string::concat(Pred, toString(Cou)),
        !.

    enum_predicate(_Cou, Pred, _, Pred) :-
        !.

%---
    pred_nest_to_temp_for_sorting() :-
% nested_pred_memory(4,3,2,2,3,"groupe0",5,"groupe0_car_atom_cdr_atom_filter_elem_filter_elems",["$expr","$head","$tail","$head_new","$tail_new"],[3,4,4,4,4],4,["groupe0","car_atom","cdr_atom","filter_elem","filter_elems"]).
        nested_pred_memory(Count, _, _, _, _, Pred, _, Pred_total, _, _, _, Predlist),
        assert(predicate_for_sort(Count, Pred, Pred_total, Predlist)),
        fail,
        !.

    pred_nest_to_temp_for_sorting() :-
        !.

%---
    pred_nest_to_ordered(Count) :-
        retractall(tel(_)),
        assert(tel(0)),
        retractall(predicate_nesting_ordered(_, _, _, _, _, _, _, _, _, _, _)),
        retract(predicate_nesting(Mlevel, Levl, Lev2, Lev3, Pred, Vars, Vpos, Vcou, Lin_nest, Ariti)),
        increment(Ordered),
        enum_predicate(Count, Pred, Ariti, Pred2),
%   Pred2 = string::concat( Pred, toString( Count ) ) ,
        assert(predicate_nesting_ordered(Ordered, Mlevel, Levl, Lev2, Lev3, Pred2, Vars, Vpos, Vcou, Lin_nest, Ariti)),
        fail,
        !.

    pred_nest_to_ordered(Count) :-
        retractall(predicate_nesting_ordered_tmp(_, _, _, _, _, _, _, _, _, _, _)),
        predicate_nesting_ordered(Ord, Mlevel, Levl, Lev2, Lev3, Pred, Vars, Vpos, Vcou, Lin_nest, Ariti),
        assert(predicate_nesting_ordered_tmp(Ord, Mlevel, Levl, Lev2, Lev3, Pred, Vars, Vpos, Vcou, Lin_nest, Ariti)),
        fail,
        !.

    pred_nest_to_ordered(_) :-
        !.

    nested_pred_memory_for_level(Prim_level, Level, Nested_name_sub) :-
        nested_pred_memory(Prim_level, _, _, Levelx, _, _, _, Nested_name_sub, _, _, _, _),
        Levelx = Level.

%  try_string_to_sterm( Bstr , Sxterm ):-
    predicates_transpile_variabels() :-
        retractall(nested_pred_memory(_, _, _, _, _, _, _, _, _, _, _, _)),
        predicate_nesting_ordered(Prim_level, Mlevel, Level0, Level1, Level, Predname, _, _Vpos, Level_of_nesting, _Lin_nest, Ariti),
        % change to  Lin_nest ?
        Level_of_nesting > 0,
        nested_pred(Predname, Prim_level, Level, Predicate_length, Nested_name, Preds_names, Vars_list, Nestlevel, Levelslist),
%     vars_slist_remove_pred_out_vars( Vars_list0 , Vars_list , _ ) ,
        assert(
            nested_pred_memory(Prim_level, Mlevel, Level0, Level1, Level, Predname, Predicate_length, Nested_name, Vars_list, Levelslist, Nestlevel,
                Preds_names)),
        fail,
        !.

    predicates_transpile_variabels() :-
        retractall(constr_clauses(_, _, _, _, _, _, _, _, _, _, _)),
        nested_pred_memory(Prim_level, Mlevel, Level0, Level1, Level, Predname, Predicate_length, Nested_name, _Vars_list, Levelslist, Nestlevel,
            Preds_names),
        Nestlength = Predicate_length - 1,
        Nestlevel > Nestlength,
%  findall( Nested_name_sub , nested_pred_memory( _ , _, _ , Level, _ , _, Nested_name_sub , _ , _ ) , Nested_name_sub_list ),
%  findall( Prim_levx , nested_pred_memory( Prim_levx , _, _ , Level, _ , _, _ , _ , _ ) , Prim_levx_list ),
        Nested_name_sub_list = [ Nested_name_sub || nested_pred_memory_for_level(_, Level, Nested_name_sub) ],
        Prim_levx_list = [ Prim_levx || nested_pred_memory_for_level(Prim_levx, Level, _) ],
        assert(
            constr_clauses(Prim_level, Mlevel, Level0, Level1, Level, Predname, Nested_name, Nested_name_sub_list, Prim_levx_list, Levelslist,
                Preds_names)),
        fail,
        !.

    predicates_transpile_variabels() :-
        !.

    inventarise_metta_sub(Count, Mlevel, Level0, Level, TERMX) :-
        retractFactDb(pred_nestings),
        % retractall( main_level( _ ) ), assert( main_level( 0 ) ),
%   pred_nestings
        % TODO
        inventarise_subcl_metta_tpro_subc(Count, Mlevel, Level0, Level, TERMX),
        pred_nest_to_ordered(Count),
        predicates_transpile_variabels(),
        pred_nest_to_temp_for_sorting(),
        Filen = string::concat("results\\inventarise", toString(Count), ".pro"),
        file::save(Filen, pred_nestings),
        % file::save
        !.
% 09:11 6-6-2025  HERE PRE-transpile  to fit  certain metta constructs

% i,o
%  debug
    pre_transpile(TERMX, TERMX_transp) :-
        TERMX = cmp(Oper1, [El1, El2]),
        Oper1 = "equal",
        El2 = cmp(Oper2, Lis2),
        Oper2 = "sub",
        Lis2 = [El1y],
        El1y = cmp(Oper3, Lis3),
        Oper3 = "let",
        Lis3 = [El1x, El2x, El3x],
        El1x = var(Org_var),
        Sx = string::concat("predicate_outvar", Org_var),
        El2x = cmp(Oper4, Lis4),
        Oper4 = "sub",
        % preserve El3x
        Lis4 = [El5x],
        El5x = cmp(Oper5, Lis6),
        !,
        Transpiled_element = cmp(Sx, [cmp("sub", [cmp(Oper5, Lis6)])]),
        TERMX_transp = cmp("equal", [El1, cmp("sub", [cmp(Oper3, [cmp(Oper4, [Transpiled_element]), El3x])])]),
        debug_Note("PRE-transpiled ", toString(TERMX_transp)),
        !.

    pre_transpile(TERMX, TERMX) :-
        !.
% here transpile  the let   where we change the position of the output variabel
% and add encapsulate 2 parenthesis

% %   stdio::write( "end_result" , Zx2 , "\n" ),
%   cmp(":-",[
%     cmp("get_valid_candidate",[var("R"),var("C"),var("Outp0")]),
%	  cmp("match_komma16",[var("Num"),var("R"),var("C"),var("Outp0")])
%	  ])
    add_cut_at_the_end_if_there_isnt(TERMX_trans, TERMX_trans2) :-
        TERMX_trans = cmp(Op, [Ter_head, Ter_body]),
        !,
        TERMX_trans2 = cmp(Op, [Ter_head, cmp(",", [Ter_body, atom("!")])]).
        % Ter_body = cmp( Op2,[  ] ),
    add_cut_at_the_end_if_there_isnt(TERMX_trans, TERMX_trans) :-
        !.

    transpile_metta_sub(Pred_count, Level0, Level, TERMX, TERMX_trans) :-
%   pre_transpile( TERMX_0 , TERMX ) ,
        set_transpile_model_type(TERMX),
        transpile_subcl_metta_tpro_subc(Pred_count, 0, Level0, Level, TERMX, TERMX_trans, Type_of_predicate, Name_of_pred, Arity),
        add_cut_at_the_end_if_there_isnt(TERMX_trans, TERMX_trans2),
        % stdio::write( "end_result", toString( TERMX_trans ) ,"\n") ,
%   stdio_write_clause( "end_result" , TERMX_trans ) ,
        assert(clause_memory(Pred_count, Type_of_predicate, Arity, "end_result", Name_of_pred, TERMX_trans2)),
% temporary not needed
        % recons_newclause_z( "transpile_metta_sub" , TERMX_trans ) ,
%24-5-2025
%   recons_newclause_metta_std_predicates() ,
        !.

% here change metta !()  to prolog bash
    convert_metta_commandli(Bstr, Bstr) :-
        !.
%----
    assert_string_lines(Cou, CountPos, Metta_str) :-
        Pos = string::search(Metta_str, "\n"),
        string::tryFront(Metta_str, Pos, LinePart, Rs),
        Lex = string::length(LinePart),
        string::tryFront(Rs, 1, _, Rs2),
        !,
        CountPos2 = CountPos + Lex + 1,
        Cou2 = Cou + 1,
        asserta(str_line(Cou2, CountPos2, LinePart)),
        assert_string_lines(Cou2, CountPos2, Rs2).
    assert_string_lines(_, _, _Metta_str) :-
        !.

%----
    % editor displays first character as 1
    %    transpile_metta_sub  outputvars
    % display_errors(),
%    Sxx = toString( TERMX ),    vpiCommonDialogs::note( "Ready metta Parsing" , Sxx ),
%    display_results(),
%        parse_rest( _Rest_tokens, 2 ),
    debug_Note(_, _) :-
        bigstr::get_debug(Sta),
        Sta = true,
        !.
%  is_debug( false ) , !.
    debug_Note(A, B) :-
% TODO implement try catch here
%  clipboard::putString( B ) ,
        try
            clipboard::putString(B)
        catch _ do
            fail
        end try,
        !,
        vpiCommonDialogs::note(A, B).

    debug_Note(A, B) :-
        !,
        vpiCommonDialogs::note(A, B).

%24-5-2025
%   recons_newclause_metta_std_predicates() ,
% At THe End do this somewhere  recons_newclause_metta_std_predicates() ,
% retractall( pred_level( _ , _ , _ ) ),
%   retractall( is_transp( _ , _ , _ , _ , _ , _ ) ),
%   retractall( term_parsed( _ , _ ) ) ,
%   assert( term_parsed( 1 , TERMX ) ) ,
% to_prolog
cmp_clause_to_metta(Cmp_clause, Str) :-    cmp_element_to_metta_string(Cmp_clause, Str),    !.
cmp_clause_to_metta(Cmp_clause, Str) :-    Str = toString(Cmp_clause),     !.

%    parse_metta([], _) :-        !.
% parse_metta1( Bstr, TERMX0 ) :-
parse_metta1( TOKENS , TERMX0 , Rest_tokens ) :-
%    tokenize( Bstr, TOKENS ) ,
    parse_report( TOKENS , TERMX0 , Rest_tokens ) ,
%        Term_parsed_x = toString(TERMX0), Res = toString(Rest_tokens), Sxp = string::concat(Term_parsed_x, "\n--\n", Res),
        !.
        %       Cou2 = Cou + 1,
        %      parse_metta(Rest_tokens, Cou2).
%------

parse_metta_program( [] , _) :-        !.
parse_metta_program( TOKENS, Cou ) :-
        % Res = toString( TOKENS ) ,
        % vpiCommonDialogs::note( "Total tokens" , Res ),
        parse_report( TOKENS, TERMX0, Rest_tokens ),
%        assert_last_token
        Term_parsed_x = toString( TERMX0 ) ,
        Res = toString( Rest_tokens ) ,
        Sxp = string::concat( Term_parsed_x, "\n--\n" , Res ) ,
        % vpiCommonDialogs::note( string::concat( "Parsing metta clause-", toString( Cou ) ) , Sxp  ) ,
        % clipboard::putString( Sxp ),
        debug_Note( string::concat("Parsing metta clause-", toString(Cou), " (is in clipboard)"), Sxp ) ,  !,
        Cou2 = Cou + 1 ,
        % memory
        % everything is in memory
        parse_metta_program( Rest_tokens, Cou2 ).

%---
parse_interpet_metta_program( [] , _ ) :-     !.
parse_interpet_metta_program( TOKENS , Cou ) :-
        % Res = toString( TOKENS ) ,
        % vpiCommonDialogs::note( "Total tokens" , Res ),
        parse_report(TOKENS, TERMX0, Rest_tokens),
        Term_parsed_x = toString( TERMX0 ) ,
        Res = toString( Rest_tokens ) ,
        Sxp = string::concat( Term_parsed_x , "\n--\n" , Res ) ,
        % vpiCommonDialogs::note( string::concat( "Parsing metta clause-", toString( Cou ) ) , Sxp  ) ,
        % clipboard::putString( Sxp ),
        debug_Note( string::concat("Parsing metta clause-", toString(Cou), " (is in clipboard)") , Sxp ) ,
        retractall( is_metta_output_var(_) ) ,   pre_transpile( TERMX0, TERMX02 ) ,
        transpile_output_variabels( TERMX02, TERMX ) ,
        transpile_metta_sub( Cou, 0, 0, TERMX, _TERMX_trans ) ,   !,     Cou2 = Cou + 1,
        % memory
        % everything is in memory
        parse_interpet_metta_program( Rest_tokens , Cou2 ).

%---
% start_parse( Metta_str , TERMX ) :-
    start_only_parse(Metta_str, 0, 0, 0) :-
        empty_errors(),
        retractFactDb(enums_preds),
        Editor_start = 0,
        retractall(str_line(_, _, _)),
        retractall(express_position(_, _, _)),
        assert_string_lines(1, Editor_start, Metta_str),
        OutString0 = Metta_str,
        filter_out_comments(OutString0, OutString),
        retractall(lasts_metta_string(_)),
        assert(lasts_metta_string(OutString)),
        tokenize(OutString, TOKENS),
        Res = toString(TOKENS),
        debug_Note("Total tokens", Res),
        parse_metta_program(TOKENS, 1),
        !.

%start_parse( _Metta_str,  par_atom_list( variabel( "Aa", 0, 0 ) , 0, 0, [] ) ) :- !.
%start_parse( _Metta_str,  cmp( "dumm_Error", [var( "Aa_error_parsing" ) ] ) ) :-
    start_only_parse(_Metta_str, Pos, Line, Linepos) :-
        display_errors(),
        find_error_position(Pos, Line, Linepos),
        !.

%---
% start_parse( Metta_str , TERMX ) :-
start_parse( Metta_str, 0, 0, 0 ) :-
        empty_errors(),
        % class facts - enums_preds
        retractFactDb(enums_preds),
        Editor_start = 0,
        retractall(str_line(_, _, _)),
%21-12-2025
        retractall(express_position(_, _, _)),
        assert_string_lines(1, Editor_start, Metta_str),
%21-12-2025  andere oplossing maken , voor de commentaar, gaat daardoor fout met de positions
        OutString0 = Metta_str,
        filter_out_comments(OutString0, OutString),
        retractall( lasts_metta_string(_) ) ,    assert( lasts_metta_string( OutString ) ) ,
        tokenize( OutString , TOKENS ) ,   Res = toString( TOKENS ) ,
        debug_Note("Total tokens", Res),
        % vpiCommonDialogs::note( "Total tokens" , Res ),
        parse_interpet_metta_program( TOKENS , 1 ) ,
%   empty_errors() ,
%   parse_report( TOKENS , TERMX0 , _Rest_tokens ) ,
%   retractall( is_metta_output_var( _ ) ) ,
%   transpile_output_variabels( TERMX0 , TERMX ) ,
%   transpile_metta_sub( 0, 0, TERMX, _TERMX_trans ) ,
        !.

%start_parse( _Metta_str,  par_atom_list( variabel( "Aa", 0, 0 ) , 0, 0, [] ) ) :- !.
%start_parse( _Metta_str,  cmp( "dumm_Error", [var( "Aa_error_parsing" ) ] ) ) :-
    start_parse(_Metta_str, Pos, Line, Linepos) :-
% , see the positions of the syntax errors in de console window
        display_errors(),
        find_error_position(Pos, Line, Linepos),
% Xs = string::format( "Position: % line: % position in line: % ", Pos , Line , Linepos ),
% Cs = string::concat( " Errors in metta-syntax, the cursor will be placed at the position of the error " , Xs ),
        %  vpiCommonDialogs::note( " Errors in metta-syntax ", Cs  ) ,
%   go_to_error_position(),
% Temp turn off
%   fail,
        !.

write_out([]) :-        !.
write_out([H | Rest]) :-        !,        file5x::write(H),        write_out(Rest).

% write_out( _X ):- !.
% i,o,o
  % tk(equal_tk, _Bg, _End)
s_atom( [ tk( variabel_tk(Str), _Bg, _End) | LL], LL, var(Str) ) :-        !.
%  s_atom( [  tk(  number_tk( Rea ), Bg , End )    | LL ] , LL , number(  Rea, Bg , End  ) ):- !.
s_atom( [ tk( number_tk(Rea), _Bg, _End) | LL], LL, real(Rea) ) :-        !.
%  s_atom([  tk( name_tk( Str ), Bg , End)  | LL ], LL, namex( Str , Bg , End) ):- !.
s_atom( [ tk( name_tk( Str ) , _Bg, _End) | LL], LL, atom(Str) ) :-        !.

%26-12-2025
s_atom( [ tk( Tok  , _Bg, _End) | LL], LL, atom(Str) ) :- operator_to_atom( Str, Tok ) ,     !.


% qqq
%  s_atom([  tk( lpar_tk,  Bg , _ ) , tk( rpar_tk,  _ , End )  | LL ], LL, empty_atom( Bg , End) ):- !.
s_atom( [ tk( lpar_tk, _Bg, _), tk(rpar_tk, _, _End) | LL], LL, atom("empty_atom")) :-        !.

    % s_atom( LL , _ , _ ):- syntax_error( atom, LL ), fail.
% QUESTION if this is correct
%  s_atom( LL1 , LL0 , metta_sub( Expr ) ):-	s_expr( LL1 , LL0 , Expr ) , !.
s_atom( LL1, LL0, cmp("sub", [Expr] ) ) :-        s_expr(LL1, LL0, Expr),        !.
s_atom( LL1, LL0, Expr ) :-        s_expr( LL1, LL0, Expr ),      !.
s_atom( LL, _, _) :-        syntax_error( "atom", LL ),        fail.
%--------
%  s_operator( [t( simple_deduction_strength_formula , _ )|LL] , LL , simple_deduction_strength_formula ):-!.
%  s_operator( [t( conditional_probability_consistency , _ )|LL] , LL , conditional_probability_consistency ):-!.

%  s_operator( [ tk( equal_tk, Bg, End ) | LL] , LL , equal( Bg, End ) ):-!.
%  s_operator( [ tk( dbl_equal_tk, Bg, End ) | LL] , LL , dbl_equal( Bg, End ) ):-!.
%  s_operator( [  tk(  conditional_tk, Bg, End ) | LL] , LL , conditional( Bg, End ) ):-!.
%  s_operator( [  tk( conjuction_tk, Bg, End ) | LL] , LL , conjuction( Bg, End ) ):-!.
    % s_operator( [  tk( smallerthan_tk, Bg, End ) | LL] , LL , smallerthan( Bg, End ) ):-!.
    % s_operator( [  tk( plus_tk, Bg, End ) | LL ] , LL , plus( Bg, End ) ):-!.
    % s_operator( [  tk( multiplication_tk, Bg, End ) | LL] , LL , multiplication( Bg, End ) ):-!.
    % s_operator( [  tk( division_tk, Bg, End ) | LL ] , LL , division( Bg, End ) ):-!.
    % s_operator( [  tk( minus_tk, Bg, End ) | LL ] , LL , minus( Bg, End ) ):-!.
    % s_operator( [  tk( name_tk( Str), Bg, End )  | LL ], LL, namex( Str , Bg, End) ):- !.
    % s_operator( [  tk(  variabel_tk( Str ), Bg, End )  | LL ], LL, variabel( Str , Bg, End ) ):- !.
%  s_operator( [ tk( equal_tk, _Bg, _End ) | LL] , LL , atom("=") ):-!.
%  s_operator( [ tk( dbl_equal_tk, _Bg, _End ) | LL] , LL , atom("==") ):-!.
%  s_operator( [  tk(  conditional_tk, _Bg, _End ) | LL] , LL , atom("if") ):-!.
%  s_operator( [  tk( conjuction_tk, _Bg, _End ) | LL] , LL , atom("and") ):-!.
%  s_operator( [  tk( smallerthan_tk, _Bg, _End ) | LL] , LL , atom("<") ):-!.
%  s_operator( [  tk( plus_tk, _Bg, _End ) | LL ] , LL , atom("+") ):-!.
%  s_operator( [  tk( multiplication_tk, _Bg, _End ) | LL] , LL , atom("*") ):-!.
%  s_operator( [  tk( division_tk, _Bg, _End ) | LL ] , LL , atom("/") ):-!.
%  s_operator( [  tk( minus_tk, _Bg, _End ) | LL ] , LL ,  atom("-") ):-!.
%  s_operator( [  tk( name_tk( Str), _Bg, _End )  | LL ], LL, str( Str ) ):- !.
%  s_operator( [  tk(  variabel_tk( Str ), _Bg, _End )  | LL ], LL, var( Str ) ):- !.
%  s_operator( [ tk( equal_tk, _Bg, _End ) | LL] , LL , "=" ):- !.
% AH   OK source is/was beautified
s_operator( [ tk(equal_tk, _Bg, _End) | LL], LL, "equal" ) :-        !.
s_operator( [ tk(dbl_equal_tk, _Bg, _End) | LL], LL, "==" ) :-        !.
s_operator( [ tk(conditional_tk, _Bg, _End) | LL], LL, "if" ) :-        !.
s_operator( [ tk(conjuction_tk, _Bg, _End) | LL], LL, "and" ) :-        !.
s_operator( [ tk(smallerthan_tk, _Bg, _End) | LL], LL, "<" ) :-        !.
s_operator( [ tk(plus_tk, _Bg, _End) | LL], LL, "+" ) :-        !.
s_operator( [ tk(multiplication_tk, _Bg, _End) | LL], LL, "*" ) :-        !.
s_operator( [ tk(division_tk, _Bg, _End) | LL], LL, "/" ) :-        !.
s_operator( [ tk(minus_tk, _Bg, _End) | LL], LL, "-" ) :-        !.
s_operator( [ tk(name_tk(Str), _Bg, _End) | LL], LL, Str ) :-        !.
s_operator( [ tk(variabel_tk(Str), _Bg, _End) | LL], LL, Str ) :-        !.
s_operator( LL , _, _) :-        syntax_error("operator", LL ),        fail.

% operator_to_name( Op_name , Op_name ) :-!.
%-----
%  s_expr( [ tk( exclamation_tk, Bg , _ ), tk( lpar_tk,  _ , _ ) | LL1 ] , LL0 , exclama_atom_list( Operator , Bg, End , Atom_list ) ):-! ,
%	s_operator( LL1 , LL2 , Operator ) ,
%	s_atom_list( LL2 , LL3 , Atom_list ) ,
%	expect( tk( rpar_tk, _ , End ) , LL3 , LL0 ).
% we cant get it working like this
% maybe we have to change the list elsewhere to change the group name
% s_expr( [ tk( name_tk( _ ), _,_) , tk( lpar_tk, _ , _ ) | LL1 ] , LL0 , cmp( "let_star", [ cmp( "sub" , [cmp( Sx2 ,  Atom_list) ] ) ]) ):-
%s_expr( [ tk( Tok, _,_)  | LL1 ] , LL0 , cmp( "let_star", [ cmp( "sub" , [cmp( Sx2 ,  Atom_list) ] ) ]) ):-
%      Tok = name_tk( Tok2 ) ,
%	  Tok2 = "let_star" ,
%	  LL1 = [ Head , Head2 | LL2 ] , Head = tk( lpar_tk, _ , _ ) ,
%	   Head2 = tk( lpar_tk, _ , _ ) , 	  ! ,
%	  Sx2 = "groupe_let_star" ,
%	s_atom_list( LL1 , LL3 , Atom_list ) ,
%	expect( tk( rpar_tk, _ , _ ) , LL3 , LL0 ).
%	  Sx = "sub" ,
%	s_atom_list( LL2 , LL3 , Atom_list ) ,

s_expr([ tk( lpar_tk, Bg, _) | LL1 ], LL0, cmp( Sx2 , Atom_list ) ) :-
    LL1 = [ Head | LL2 ] ,   Head = tk( lpar_tk, _ , _ ) ,    !,   Sx2 = "groupe",
    s_atom_list( LL1, LL3 , Atom_list ) ,
    expect( tk( rpar_tk, _, End ) , LL3, LL0 ) ,    assert( express_position( Sx2, Bg, End ) ).

% 09:24 28-4-2025  test if we can put the variabel  instead of an operator
% it parses but todo  to arrange it well
% i,o,o , most of the time, allways
s_expr( [ tk( lpar_tk, Bg, _) | LL1], LL0, cmp(Sx, Atom_list ) ) :-
    LL1 = [ Head | LL2 ] ,   Head = tk( variabel_tk(Str) , _, _) ,    !,
    Sx = string::concat( "predicate_outvar", Str ) ,
    s_atom_list( LL2, LL3, Atom_list ) ,   expect( tk( rpar_tk, _, End ), LL3, LL0 ) ,
    assert( express_position( Sx, Bg, End ) ) .

% exclam
%  s_expr( [ tk( exclamation_tk, Bg , _ ), tk( lpar_tk,  _ , _ ) | LL1 ] , LL0 , exclama_atom_list( Operator , Bg, End , Atom_list ) ):-! ,
%	s_operator( LL1 , LL2 , Operator ) ,
%	s_atom_list( LL2 , LL3 , Atom_list ) ,
%	expect( tk( rpar_tk, _ , End ) , LL3 , LL0 ).
% exclama
% add exclamation mark
s_expr([ tk(exclamation_tk, Bg, _), tk(lpar_tk, _, _) | LL1], LL0, cmp("exclamation", [ cmp(Operator, Atom_list) ] ) ) :-        !,
   s_operator(LL1, LL2, Operator), s_atom_list(LL2, LL3, Atom_list), expect(tk(rpar_tk, _, End), LL3, LL0),
   assert( express_position( "exclamation" , Bg, End ) ) .

% to arrange for a  real var after the left parenthesis
% xxxxxxx
% Sx = string::concat( "predicate_outvar" , Str ),

s_expr([ tk( lpar_tk , Bg , _) | LL1 ], LL0, cmp("data_atom",  [ real(Str) | Atom_list])) :-
   LL1 = [ Head | LL2 ],  Head = tk(number_tk(Str), _, _),    !,
   s_atom_list( LL2, LL3, Atom_list ) ,  expect( tk( rpar_tk , _, End ), LL3, LL0 ) ,
   assert( express_position("data_atom", Bg, End) ).

%  s_expr( [ tk( lpar_tk, Bg , _ ) | LL1 ] , LL0 , par_atom_list( Operator , Bg, End, Atom_list ) ):-! ,
s_expr([ tk(lpar_tk, Bg, _) | LL1 ], LL0, cmp( Operator, Atom_list ) ) :-        !,
    s_operator( LL1, LL2, Operator ) ,   s_atom_list( LL2, LL3, Atom_list ) ,
    expect( tk(rpar_tk, _, End ), LL3, LL0 ),    assert( express_position(Operator, Bg, End) ).

s_expr(LL, _, _) :-        syntax_error("expr", LL),        fail.
%-----
% i,o,o
s_atom_list( LL1, LL0, [ Atom | Atom_list ] ) :-  s_atom( LL1, LL2, Atom),        !,
   s_atom_list( LL2, LL0, Atom_list ).

s_atom_list( LL, LL, [] ).

clauses
    expect(Tok, [Tok | L], L).
%---
% syntax_errors_nd( Type_str, Toq  ):-     tok_error( Type_str , Toq  ).
%----
    syntax_error(Type_mess, [Toq | _Tok_lis]) :-
        % Sx = toString( Token ),
        Toq = tk(Tk, P1, P2),
        % retractall( tok_error( Type_mess , Tk , P1 , P2 ) ) ,
        retractall(tok_error(Type_mess, _, _, _)),
        assert(tok_error(Type_mess, Tk, P1, P2)),
        % C3 = string::concat( Type_mess, " " , Sx ) ,
        !.
    syntax_error(_, _).
%---
    empty_errors() :-
        !,
        retractall(tok_error(_, _, _, _)).

%---
% % Line_count
    calculate_line_position(Pos, Line_count, Line_pos) :-
        Adhoc = 2,
        str_line(Line_count, CountPos, _LinePart),
        CountPos < Pos,
        Line_pos = Pos - CountPos + Adhoc,
        !.
    calculate_line_position(_Pos, 0, 0) :-
        !.

% calculate_line_position( P1 ,  Line , Line_pos ) :- ! .
%----
% assert( str_line( Cou , CountPos , BgStr )  ) ,
%display_errors():-  stdio::write( "Temp lines : \n" ),
%  str_line( Cou , CountPos , LinePart ) ,
%  stdio::write( "line-" , toString( Cou ) , " " , toString( CountPos ) , " -",  LinePart  , "-\n" ) , fail , ! .
    find_error_position(Pos1, Line_count, Line_pos) :-
        tok_error(_Type_str, _Toq, Pos1, _P2),
        calculate_line_position(Pos1, Line_count, Line_pos),
        !.
% bigstr::goto_sci_edit_pos( Pos1 ), ! .

    find_error_position(0, 0, 0) :-
        !.

%go_to_error_position():-
% tok_error( _Type_str , _Toq  , Pos1, _P2 ) ,
% bigstr::goto_sci_edit_pos( Pos1 ), ! .
%go_to_error_position():-!.
    display_errors() :-
        stdio::write("Parsing errors : \n"),
        tok_error(Type_str, Toq, P1, _P2),
        % Toq = tk( _ , P1 , P2 ) ,
        calculate_line_position(P1, Line_count, Line_pos),
        stdio::write("Error: ", Type_str, " ", toString(Toq), " Line: ", toString(Line_count), " position ", toString(Line_pos), "\n"),
        fail,
        !.

    display_errors() :-
        !.

    read_til_end(Source1, Source2, Pos) :-
        Pos = string::search(Source1, "\n"),
        string::tryFront(Source1, Pos, _, Rs),
        Source2 = Rs,
        !.

    read_til_end(Source1, Source2, Pos) :-
        Pos = string::search(Source1, "\r"),
        string::tryFront(Source1, Pos, _, Rs),
        Source2 = Rs,
        !.

%----
    is_a_space(' ').
    is_a_space('\t').
    is_a_space('\n').

    is_a_space_i(32).
    is_a_space_i(9).
    is_a_space_i(10).
    is_a_space_i(13).

    is_comment_start(37).
    is_comment_start(59).

    is_a_space_s(" ").
    is_a_space_s("\t").
    is_a_space_s("\n").

    is_alfa(V) :-
        V >= 97,
        V <= 122,
        !.
    is_alfa(V) :-
        V >= 65,
        V <= 90,
        !.

    is_not_a_space_i(Chv) :-
        is_a_space_i(Chv),
        !,
        fail.
    is_not_a_space_i(_) :-
        !.

    get_frontChar_i(Str, Cha_i, Str2) :-
        string::frontChar(Str, Cha, Str2),
        Cha_i = string::getCharValue(Cha),
        !.
%---
    get_frontChar_2i(Str, Cha_i, Cha_2i, Str3) :-
        string::frontChar(Str, Cha, Str2),
        string::frontChar(Str2, Cha2, Str3),
        Cha_i = string::getCharValue(Cha),
        Cha_2i = string::getCharValue(Cha2),
        !.

% frontChar("ABCD", Char, String),
    is_alfa_string(Str) :-
        Str = "",
        !.
    is_alfa_string(Str) :-
        get_frontChar_i(Str, Cha_i, Str2),
        is_alfa(Cha_i),
        !,
        % string::frontChar( Str , Cha , Str2 ),
        % Va = string::getCharValue( Cha ), is_alfa( Va ), !,
        is_alfa_string(Str2).

% add manually 11:24 8-9-2024
    get_fronttoken(Source, Bg, Res2, 2) :-
        string5x::fronttoken(Source, Fronttoken, Rest),        Fronttoken = "\"",        !,
        string5x::searchstring(Rest, "\"", Pos),        P2 = Pos - 1,
        string5x::frontstr(P2, Rest, Bg, Res),        string5x::frontstr(1, Res, _, Res2).

    get_fronttoken(Source, Bg, Res2, 2) :-
        string5x::fronttoken(Source, Fronttoken, Rest),        Fronttoken = "'",        !,
        string5x::searchstring(Rest, "'", Pos),        P2 = Pos - 1,
        string5x::frontstr(P2, Rest, Bg, Res),        string5x::frontstr(1, Res, _, Res2).

% string with 2 dubbel colon signs ,, 58  is  minus char
get_fronttoken(Source, Bg3, Rest5, 2 ) :-
        string5x::fronttoken(Source, Fronttoken, Rest),  Lx = string::length(Fronttoken),    Lx > 1,
        get_frontChar_2i(Rest, Cha_v, Cha_v2, Rs2),  Cha_v = 58,  Cha_v2 = 58,
        get_frontChar_i( Rs2, Cha_v3, _Rs3 ) ,   is_not_a_space_i( Cha_v3 ) ,
        string5x::fronttoken( Rs2, Fronttoken3, Rest4 ),  Lx3 = string::length(Fronttoken3),      Lx3 > 1,
        get_frontChar_2i(Rest4, Cha_v4, Cha_v42,  Rs4),   Cha_v4 = 45,   Cha_v42 = 45,
        get_frontChar_i(Rs4, Cha_v5, _Rs5),        is_not_a_space_i(Cha_v5),
        string5x::fronttoken(Rs4, Fronttoken5, Rest5),  Lx5 = string::length(Fronttoken5),  Lx5 > 1,    !,
        Bg3 = string::concat(Fronttoken, "_", Fronttoken3, "_", Fronttoken5).
% maybe we must use / maintain de dubbel colon hier

% string with 1 dubbel colon signs ,, 58  is  minus char
get_fronttoken( Source, Bg3, Rest4, 1) :-
   string5x::fronttoken( Source, Fronttoken, Rest), Lx = string::length( Fronttoken ) ,  Lx > 1,
   get_frontChar_2i(Rest, Cha_v, Cha_v2,  Rs2),  Cha_v = 58, Cha_v2 = 58,   get_frontChar_i(Rs2, Cha_v3, _Rs3),    is_not_a_space_i(Cha_v3),
   string5x::fronttoken(Rs2, Fronttoken3, Rest4),    Lx3 = string::length(Fronttoken3),  Lx3 > 1,    !,
   Bg3 = string::concat(Fronttoken, "_", Fronttoken3).


% string with 2 minus signs , and Exclam at end   is  minus char
get_fronttoken(Source, Bg3, Rest6, 0) :-
        string5x::fronttoken(Source, Fronttoken, Rest),  Lx = string::length(Fronttoken),    Lx > 1,
        get_frontChar_i(Rest, Cha_v, Rs2),     Cha_v = 45,  get_frontChar_i(Rs2, Cha_v2, Rs3),   is_not_a_space_i(Cha_v2),
        string5x::fronttoken(Rs2, Fronttoken3, Rest4),   Lx3 = string::length(Fronttoken3),     Lx3 > 1,
        get_frontChar_i(Rest4, Cha_v4, Rs4), Cha_v4 = 45,   get_frontChar_i(Rs4, Cha_v5, Rs5),    is_not_a_space_i(Cha_v5),
        string5x::fronttoken(Rs4, Fronttoken5, Rest5),    Lx5 = string::length(Fronttoken5),     Lx5 > 1,
        get_frontChar_i( Rest5, Cha_xx, Rest6 ),  Cha_xx = 33,         !,
        Bg3 = string::concat(Fronttoken, "_", Fronttoken3, "_", Fronttoken5, "!" ).


% string with 2 minus signs ,, 45  is  minus char
get_fronttoken(Source, Bg3, Rest5, 0) :-
        string5x::fronttoken(Source, Fronttoken, Rest),  Lx = string::length(Fronttoken),    Lx > 1,
        get_frontChar_i(Rest, Cha_v, Rs2),     Cha_v = 45,  get_frontChar_i(Rs2, Cha_v2, Rs3),   is_not_a_space_i(Cha_v2),
        string5x::fronttoken(Rs2, Fronttoken3, Rest4),   Lx3 = string::length(Fronttoken3),     Lx3 > 1,
        get_frontChar_i(Rest4, Cha_v4, Rs4), Cha_v4 = 45,   get_frontChar_i(Rs4, Cha_v5, Rs5),    is_not_a_space_i(Cha_v5),
        string5x::fronttoken(Rs4, Fronttoken5, Rest5),    Lx5 = string::length(Fronttoken5),     Lx5 > 1,        !,
        Bg3 = string::concat(Fronttoken, "_", Fronttoken3, "_", Fronttoken5).

% string with 1 minus sign ANd exclam at the end   ,, 45  minus char
get_fronttoken(Source, Bg3, Rest5, 0) :-
    string5x::fronttoken(Source, Fronttoken, Rest), Lx = string::length(Fronttoken),  Lx > 1,
    get_frontChar_i(Rest, Cha_v, Rs2),  Cha_v = 45,   get_frontChar_i(Rs2, Cha_v2, _Rs3),    is_not_a_space_i(Cha_v2),
    string5x::fronttoken(Rs2, Fronttoken3, Rest4),    Lx3 = string::length(Fronttoken3),  Lx3 > 1,
    get_frontChar_i( Rest4, Cha_xx, Rest5 ),  Cha_xx = 33, !,
    Bg3 = string::concat(Fronttoken, "_", Fronttoken3, "!" ).


% string with 1 minus sign  ,, 45  minus char
get_fronttoken(Source, Bg3, Rest4, 0) :-
        string5x::fronttoken(Source, Fronttoken, Rest), Lx = string::length(Fronttoken),  Lx > 1,
        get_frontChar_i(Rest, Cha_v, Rs2),  Cha_v = 45,   get_frontChar_i(Rs2, Cha_v2, _Rs3),    is_not_a_space_i(Cha_v2),
        string5x::fronttoken(Rs2, Fronttoken3, Rest4),    Lx3 = string::length(Fronttoken3),  Lx3 > 1,    !,
        Bg3 = string::concat(Fronttoken, "_", Fronttoken3).



get_fronttoken( Source, Bg3, Rest2, 0) :-
   string5x::fronttoken( Source, Fronttoken, Rest),  Fronttoken = "@", get_frontChar_i(Rest, Cha_v, _Rs2),
   is_not_a_space_i( Cha_v ), string5x::fronttoken(Rest, Fronttoken2, Rest2), !, string5x::concat("@", Fronttoken2, Bg3).

% variabele met apostrof  , komt blijkbaar voor in de stdlib doc file
get_fronttoken( Source, Bg3, Rest3, 1 ) :-
   string5x::fronttoken( Source, Fronttoken, Rest),  Fronttoken = "$", get_frontChar_i(Rest, Cha_v, _Rs2),
   is_not_a_space_i( Cha_v ),
   string5x::fronttoken( Rest, Fronttoken2, Rest2 ) ,
   string5x::fronttoken( Rest2, Fronttoken3, Rest3 ),  Fronttoken3 = "'",   !,
   string5x::concat( "$" , Fronttoken2 , Bg3 ).

get_fronttoken( Source, Bg3, Rest2, 0) :-
   string5x::fronttoken( Source, Fronttoken, Rest),  Fronttoken = "$", get_frontChar_i(Rest, Cha_v, _Rs2),
   is_not_a_space_i( Cha_v ), string5x::fronttoken(Rest, Fronttoken2, Rest2), !, string5x::concat("$", Fronttoken2, Bg3).

get_fronttoken(Source, Bg3, Rest2, 0) :-
  string5x::fronttoken(Source, Fronttoken, Rest),      Fronttoken = "&",     get_frontChar_i(Rest, Cha_v, _Rs2),
  is_not_a_space_i(Cha_v),    string5x::fronttoken(Rest, Fronttoken2, Rest2),      !,
  string5x::concat("&", Fronttoken2, Bg3).

% string zoals lalala!
get_fronttoken(Source, Sx, Rest3, 0) :-
    string5x::fronttoken(Source, Fronttoken, Rest),  is_alfa_string(Fronttoken),  get_frontChar_i(Rest, Cha_v, _Rs2),
    is_not_a_space_i(Cha_v),  string5x::fronttoken(Rest, Fronttoken2, Rest3),  Fronttoken2 = "!",
    Sx = string::concat(Fronttoken, Fronttoken2),      !.

get_fronttoken(Source, Sx, Rest3, 0) :-
    string5x::fronttoken(Source, Fronttoken, Rest),  is_alfa_string(Fronttoken),  get_frontChar_i(Rest, Cha_v, _Rs2),
    is_not_a_space_i(Cha_v),  string5x::fronttoken(Rest, Fronttoken2, Rest3),  Fronttoken2 = "=",
    Sx = string::concat(Fronttoken, Fronttoken2),      !.


% xyxyxy Dont allow   let*  replace let* with let_star
% 21-12-2025
get_fronttoken(Source, Sx, Rest3, -4) :-
    string5x::fronttoken(Source, Fronttoken, Rest),    is_alfa_string(Fronttoken),    Fronttoken = "let",
    get_frontChar_i(Rest, Cha_v, _Rs2),    is_not_a_space_i(Cha_v),    string5x::fronttoken(Rest, Fronttoken2, Rest3),
    Fronttoken2 = "*",    Sx = string::concat(Fronttoken, "_star"),    !.

%get_fronttoken( Source, Sx , Rest3 ):-
%  string5x::fronttoken( Source , Fronttoken , Rest ),
%  is_alfa_string( Fronttoken ) ,
%  get_frontChar_i( Rest, Cha_v , _Rs2 ),  is_not_a_space_i( Cha_v ),
%  string5x::fronttoken( Rest, Fronttoken2, Rest3 ),  Fronttoken2 = "*",
%  Sx = string::concat( Fronttoken , "_star" ),
%  !.
get_fronttoken(Source, "==", Rest3, 0) :-
   string5x::fronttoken(Source, Fronttoken, Rest),  Fronttoken = "=",   get_frontChar_i(Rest, Cha_v, _Rs2),
        is_not_a_space_i(Cha_v),    string5x::fronttoken(Rest, Fronttoken2, Rest3),   Fronttoken2 = "=",        !.

get_fronttoken(Source, ">=", Rest3, 0) :-
        string5x::fronttoken(Source, Fronttoken, Rest),    Fronttoken = ">",    get_frontChar_i(Rest, Cha_v, _Rs2),
        is_not_a_space_i(Cha_v),    string5x::fronttoken(Rest, Fronttoken2, Rest3),    Fronttoken2 = "=",        !.
get_fronttoken(Source, "<=", Rest3, 0) :-
        string5x::fronttoken(Source, Fronttoken, Rest),   Fronttoken = "<",    get_frontChar_i(Rest, Cha_v, _Rs2),
        is_not_a_space_i(Cha_v),    string5x::fronttoken(Rest, Fronttoken2, Rest3),  Fronttoken2 = "=",        !.

get_fronttoken(Source, "->", Rest3, 0) :-
        string5x::fronttoken(Source, Fronttoken, Rest),    Fronttoken = "-",    get_frontChar_i(Rest, Cha_v, _Rs2),
        is_not_a_space_i(Cha_v),    string5x::fronttoken(Rest, Fronttoken2, Rest3),     Fronttoken2 = ">",     !.

get_fronttoken(Source, ":>", Rest3, 0) :-
        string5x::fronttoken(Source, Fronttoken, Rest),   Fronttoken = ":",   get_frontChar_i(Rest, Cha_v, _Rs2),
        is_not_a_space_i(Cha_v),    string5x::fronttoken(Rest, Fronttoken2, Rest3),     Fronttoken2 = ">",     !.

%get_fronttoken( Source , Qw , Rest4 ):-
%     string5x::fronttoken( Source , Fronttoken , Rest ) ,
%	 is_alfa_string( Fronttoken  ),
%	  string5x::fronttoken( Rest , Fronttoken2 , Rest3 ) ,  Fronttoken2 = "-" ,
%	   string5x::fronttoken( Rest3 , Fronttoken4 , Rest4 ),
%	    is_alfa_string( Fronttoken4  ),
%		Qw = string::concat( Fronttoken, "-", Fronttoken4 ),	 ! .
get_fronttoken(Source, "komma", Rest , 4 ) :-
  string5x::fronttoken( Source, Fronttoken, Rest),     Fronttoken = "," ,    !.

% default i think
get_fronttoken(Source, Fronttoken, Rest, 0) :-   string5x::fronttoken(Source, Fronttoken, Rest),     !.

get_fronttoken( Source, Bg3, Res2, 0 ) :-
   string5x::searchstring(Source, " ", Pos) ,  Pos > 1, P2 = Pos - 1 ,  string5x::frontstr( P2, Source, Bg3, Res ) ,
   string5x::frontstr( 1, Res, _, Res2 ) ,     !.

get_fronttoken(Source, Fronttoken, Rest, 0) :-        string5x::fronttoken(Source, Fronttoken, Rest),        !.

% arrange for the Quote here
scan( Starting_Position, Source, [ tk(Token, Location_Of_Token, Xend_Position) | Tail ] ) :-
   skip_spaces( Source, New_Source, 0 , Number_Of_Spaces ) ,
   Location_Of_Token = Starting_Position + Number_Of_Spaces ,
   get_fronttoken( New_Source, Fronttoken, Rest, Xtra_len ) ,      !,
   Lower_Case_Fronttoken = Fronttoken,
   string5x::str_len( Fronttoken, Length_Of_Fronttoken ) ,
   string_tokenq( Lower_Case_Fronttoken, Location_Of_Token, Length_Of_Fronttoken, Token ) ,
   Xend_Position = Location_Of_Token + Length_Of_Fronttoken + Xtra_len ,
  scan( Xend_Position, Rest, Tail ).
scan(_, _, [] ).
%-----
        % string::frontChar(Source, Char, Source1), Va = string::getCharValue(Char),
        % Zq = string::charToString( Char ),

 skip_spaces(Source, New_Source, NSpaces, Resu_spaces) :-
        get_frontChar_i(Source, Va, Source1),    is_a_space_i(Va),        !,
        NSpaces2 = NSpaces + 1,
        skip_spaces(Source1, New_Source, NSpaces2, Resu_spaces).

 skip_spaces(Source, New_Source, NSpaces, Resu_spaces) :-
        get_frontChar_i(Source, Va, Source1),        is_comment_start(Va),
        read_til_end(Source1, Source2, Count),        !,
        NSpaces2 = NSpaces + 1 + Count,
        skip_spaces(Source2, New_Source, NSpaces2, Resu_spaces).

 skip_spaces(Source, Source, Resu, Resu) :-        !.

%--
 operator_to_name( ":", "colon" ) :-        !.
 operator_to_name( "*", "multiplication" ) :-        !.
 operator_to_name( "==", "check_is_equal" ) :-        !.

 operator_to_name( "if", "condition_if" ) :-        !.
 operator_to_name( "and", "and" ) :-        !.
%  operator_to_name( "" , "" ) :- ! .
 operator_to_name( "<", "smaller_than" ) :-        !.
 operator_to_name( ">", "bigger_than" ) :-        !.
 operator_to_name( "<=", "smaller_than_eq" ) :-        !.
 operator_to_name( ">=", "biggger_than_eq" ) :-        !.
 operator_to_name( "+", "plus" ) :-        !.
 operator_to_name( "-", "minus" ) :-        !.
 operator_to_name( "/", "division" ) :-        !.

%---------------------------------------------------
% Only let uncommented hier the ones that are being used in the
% chosen grammar-file
%  string_tokenq(":", 	colon ) :- !.
%  string_tokenq("=", 	equalsign ) :- !.

%26-12-2025
% this for variabel declarations
operator_to_atom( "=", equal_tk ) :-        !.
operator_to_atom( "==",  dbl_equal_tk ) :-        !.
operator_to_atom( "if",  conditional_tk ) :-        !.
operator_to_atom( "and", conjuction_tk ) :-        !.
operator_to_atom( "<",  smallerthan_tk ) :-        !.
operator_to_atom( "+",  plus_tk ) :-        !.
operator_to_atom( "*",  multiplication_tk ) :-        !.
% operator_to_atom( "!", _, _, exclamation_tk ) :-        !.
operator_to_atom( "/", division_tk ) :-        !.
operator_to_atom( "-", minus_tk ) :-        !.


%----
string_tokenq( "=" , _, _, equal_tk ) :-        !.
string_tokenq( Operat, _, _, name_tk( Op_name ) ) :-  operator_to_name( Operat, Op_name ),  !.
string_tokenq( "==", _, _, dbl_equal_tk ) :-        !.

string_tokenq( "if", _, _, conditional_tk ) :-        !.
string_tokenq( "and", _, _, conjuction_tk ) :-        !.
string_tokenq( "<", _, _, smallerthan_tk ) :-        !.
string_tokenq( "+", _, _, plus_tk ) :-        !.
string_tokenq( "*", _, _, multiplication_tk ) :-        !.
string_tokenq( "!", _, _, exclamation_tk ) :-        !.
string_tokenq( "/", _, _, division_tk ) :-        !.
string_tokenq( "-", _, _, minus_tk ) :-        !.
string_tokenq( "(", _, _, lpar_tk ) :-        !.
string_tokenq( ")", _, _, rpar_tk ) :-        !.
string_tokenq( Str, _, _, number_tk(Rea) ) :-    conversion5x::str_real(Str, Rea),       !.
string_tokenq( Str, _, _, variabel_tk(Str) ) :-  string5x::frontstr(1, Str, Bg, _Res),  Bg = "$",    !.
string_tokenq( Str, _, _, name_tk(Str) ) :-     !.


% OPTIONAL
%  string_tokenq(";", 	semicolon ) :- !.
%  string_tokenq("?", 	interrogation ) :- !.
%  string_tokenq("\"",	quote ) :- !.
        % string_tokenq("simple_deduction_strength_formula", 	simple_deduction_strength_formula ) :- !.
    %string_tokenq("conditional_probability_consistency", 	conditional_probability_consistency ) :- !.


% DEZE toevoegen zodra je een grammar gebruikt  waarin deze voorkomen
clauses
tokenize( Expr, Tokens ) :-    scan( 0, Expr, Tokens ).

% parse( Tokens, Term ) :-   	s_expr( Tokens, Unused_Tokens, Term ),  	Rest_Tokens = [].
%----
parse_report(Tokens, Term, Rest_Tokens) :-        s_expr(Tokens, Rest_Tokens, Term).

%-----
% 09:41 24-5-2025
    %conversion5x::term_str( expr , Termx , Res2 ),
% parse_rest( Rest_tokens, _Count ):- Rest_Tokens = [] , ! .
% parse_rest( List_tokens, Count ):-
%  parse_report( List_tokens, Termx, Rest_Tokens ), assert( term_parsed( Count, Termx ) ),
%  Res2 = toString( Termx ), write_out( [ Res2 , "\n" ] ) , Count2 = Count + 1,
%  parse_rest( Rest_Tokens, Count2 ).
%----
    reverse_slist([], Varslist2, Varslist2) :-
        !.
    reverse_slist([Varx | Vars_str_list], Varslist, Varslist2) :-
        reverse_slist(Vars_str_list, [Varx | Varslist], Varslist2).

    is_member(Varx, [Varx | _Varslist]) :-
        !.
    is_member(Varx, [_ | Varslist]) :-
        is_member(Varx, Varslist),
        !.

    is_not_member(Varx, Varslist) :-
        is_member(Varx, Varslist),
        !,
        fail.
    is_not_member(_Varx, _Varslist) :-
        !.

    filter_which_are_not_in_list([], _Varslist, [], []) :-
        !.
    filter_which_are_not_in_list([Varx | Vars_str_list], Varslist, [Varx | Varslist2], Varslist3) :-
        is_not_member(Varx, Varslist),
        !,
        filter_which_are_not_in_list(Vars_str_list, Varslist, Varslist2, Varslist3).
% TODO has probably to be reversed somewhere
    filter_which_are_not_in_list([Varx | Vars_str_list], Varslist, Varslist2, [Varx | Varslist3]) :-
        filter_which_are_not_in_list(Vars_str_list, Varslist, Varslist2, Varslist3).

    add_when_not_in_list([], Varslist, Varslist2) :-
        reverse_slist(Varslist, [], Varslist2),
        !.
    add_when_not_in_list([Varx | Vars_str_list], Varslist, Varslist2) :-
        is_not_member(Varx, Varslist),
        !,
        add_when_not_in_list(Vars_str_list, [Varx | Varslist], Varslist2).
% TODO has probably to be reversed somewhere
    add_when_not_in_list([_Varx | Vars_str_list], Varslist, Varslist2) :-
        add_when_not_in_list(Vars_str_list, Varslist, Varslist2).

%---
    position_of_last_occurance(Str, Tag, Cou, Rpos) :-
        string5x::searchstring(Str, Tag, P),
        string5x::frontstr(P, Str, _, Rest2),
        !,
        Cou2 = Cou + P,
        position_of_last_occurance(Rest2, Tag, Cou2, Rpos).
    position_of_last_occurance(_Str, _Tag, Cou, Cou) :-
        !.

%--
    string_replace_tag_in_pos(A, P, Rmvtag, Rep, Aq) :-
        P > 0,
        % searchstring( A , Rmvtag ,  P),
        P2 = P - 1,
        string5x::str_len(Rmvtag, Le),
        Le > 0,
        string5x::frontstr(P2, A, Sta, Rest),
        string5x::frontstr(Le, REst, _, Rest2),
        !,
        string5x::concat(Sta, Rep, Z1),
        string5x::concat(Z1, Rest2, Aq).
    string_replace_tag_in_pos(A, _, _Rmvtag, _, A) :-
        !.

%---
    string_replace_tag(A, Rmvtag, Rep, Ares) :-
        string5x::searchstring(A, Rmvtag, P),
        P2 = P - 1,
        string5x::str_len(Rmvtag, Le),
        Le > 0,
        string5x::frontstr(P2, A, Sta, Rest),
        string5x::frontstr(Le, REst, _, Rest2),
        !,
        string5x::concat(Sta, Rep, Z1),
        string5x::concat(Z1, Rest2, Aq),
        string_replace_tag(Aq, Rmvtag, Rep, Ares).
    string_replace_tag(A, _Rmvtag, _, A) :-
        !.

% turn off
    change_adhoc(Res, Res) :-
        !.
    change_adhoc(Res4, Res7) :-
        string_replace_tag(Res4, ",\"", ",", Res5),
        string_replace_tag(Res5, "\"),\n", "),\n", Res6),
        string_replace_tag(Res6, "\")])", ")])", Res7),
        !.

% transpilex(22,"conditional( transx(- 6 , [numf(4),numf(5)] -) , transx(- 21 , [numf(8),variabel(\"$Cs\"),numf(20)] -) , 0 ) ")])
% after
%assert_var( Sx ):- var_concerned( Sx ), !.
%assert_var( Sx ):- assert( var_concerned( Sx ) ) , !.
%assert_vars( _Nx, [] ):- !.
%assert_vars( Nx, [ variabel( Sx, _, _ ) | Rs ] ):- !, assert_var( Sx ), assert_vars( Nx, Rs ).
%assert_vars( _Nx, [ _ | Rs ] ):- !,  assert_vars( _Nx, Rs ).
% assert_vars( Numsf_vars )
%---
%assert_variabels_concerned( Nx ):-    is_transp( is_debug , Nx , _Operat , Subnums , Numsf_vars, _ ), !,
%  assert_variabels_concerned_list( Subnums ),  assert_vars( Nx, Numsf_vars ).
%assert_variabels_concerned_list( [] ):- !.
%assert_variabels_concerned_list( [ Num | Nums_sub ] ):- !, assert_variabels_concerned( Num ),
% assert_variabels_concerned_list( Nums_sub ).
%--
    which_list_for_second(0, Varsn_lis2, _Varsn_lis_todo, Varsn_lis2) :-
        !.
    which_list_for_second(_Count_replace, _Varsn_lis2, Varsn_lis_todo, Varsn_lis_todo) :-
        !.

    first_up(S2, C4) :-
        string5x::str_len(S2, Le),
        Le > 0,
        !,
        string5x::frontstr(1, S2, Bg, Res),
        conversion5x::upper_lower(Bg2, Bg),
        string5x::concat(Bg2, Res, C4),
        !.
    first_up(S2, S2) :-
        !.

    clean_var(H, S3) :-
        !,
        string_replace_tag(H, "$", "", S2),
        first_up(S2, S3).

    clean_var_list([], []) :-
        !.
    clean_var_list([H | Ter], [S3 | Ter2]) :-
        !,
        clean_var(H, S3),
        clean_var_list(Ter, Ter2).

    concat_list([], Res, Res) :-
        !.
    concat_list([H | Lis], Hs, Res) :-
        !,
        string5x::concat(Hs, H, Hs2),
        concat_list(Lis, Hs2, Res).

    funct_constr_name_or_std_pred(namex(Fu, _, _), Fu) :-
        !.
    funct_constr_name_or_std_pred(Operat, Sq) :-
        Sq = toString(Operat),
        !.

%---
% todo imp
    trim_string(Bg0, Bg3) :-
        !,
        trim_tags(Bg0, ")", Bg2),
        trim_tags(Bg2, " ", Bg3).
% trim_tags( string, string, string ) - ( i,i,o)

    trim_tags(Fie0, Tag, Fie) :-
        string5x::concat(Bg, Tag, Fie0),
        !,
        trim_tags(Bg, Tag, Fie).
    trim_tags(Fie0, Tag, Fie) :-
        string5x::concat(Tag, End, Fie0),
        !,
        trim_tags(End, Tag, Fie).
    trim_tags(Fie, _, Fie) :-
        !.

%---
% last_file
%-----
%---
    komma_separator_if_more_resting(Rest, " , ") :-
        Le = list::length(Rest),
        Le > 0,
        !.
    komma_separator_if_more_resting(_Rest, "") :-
        !.

    debug_commment_str(_, "") :-
        !.

    str_after(Varstr, Big_str, Before, Rest) :-
        string5x::str_len(Varstr, Lz),
        string5x::searchstring(Big_str, Varstr, Fp),
        Fpx = Fp - 1,
        string5x::frontstr(Fpx, Big_str, Before, _Rest0),
        Fp2 = Fp + Lz - 1,
        string5x::frontstr(Fp2, Big_str, _Sta, Rest),
        !.

    str_before(Varstr, Jso_big_str, Sta) :-
% str_len( Varstr , Lz ),
        string5x::searchstring(Jso_big_str, Varstr, Fp),
        Fp2 = Fp - 1,
        string5x::frontstr(Fp2, Jso_big_str, Sta, _Rest),
        !.

    length_of_list([_ | Lis0x], C, Le) :-
        C2 = C + 1,
        length_of_list(Lis0x, C2, Le).
    length_of_list([], Le, Le) :-
        !.

    increment_count(C2) :-
        retract(count(C)),
        !,
        C2 = C + 1,
        assert(count(C2)).

%---
    write_nspaces(N) :-
        N > 0,
        !,
        write_out(["  "]),
        N2 = N - 1,
        write_nspaces(N2).
    write_nspaces(_N) :-
        !.

    write_level(Level) :-
        conversion5x::str_int(Sx, Level),
        write_out(["\n-lev-", Sx, "- "]),
        !.
%----

%has_no_sub_nesting2( [] , Res, Res ):-!.
%has_no_sub_nesting2( [ metta_sub(  par_atom_list( _Operatorx , _, _, Ato_lis ) ) | _Lis ] , Cou,  Res ):- !, Cou2 = Cou + 1,
%    has_no_sub_nesting2(  Ato_lis  , Cou2, Res ), !.
%has_no_sub_nesting2( [ _H | Lis ] , Cou, Res ):- !,   has_no_sub_nesting2( Lis  , Cou, Res ),!.
% to  examp
% [cmp( "func2" , [ list( var( "H" ) , var( "T" ) ) , var( "W" ) , var( "Q" ) ] ),
%     cmp(",",[cmp("=",[var("A"),cmp("+",[var("W"),var("H")])]),cmp("func2",[var("T"),var("A"),var("Q")])])])
%----
%write_out_metta2( _ , [] ) :- !.
%write_out_metta2( Level, [ metta_sub( S_expr ) | Lis ] ):- !,   Level2 = Level + 1, write_out( ["("] ),
%   write_out_metta( Level2, S_expr ), write_out( [")"] ), write_out_metta2( Level, Lis ).
%write_out_metta2( Level, [ H | Lis ] ):- !,   Sx = toString( H ),
%   write_level( Level ),   write_out( [Sx , "\n"] ) , write_out_metta2( Level, Lis ).
%-----
%write_out_metta( Level, par_atom_list( Operatorx , _, _, Atom_list ) ):-
%  Sx = toString( Operatorx ),  write_level( Level ),
%  string5x::concat( "par_atox ", Sx, C1 ),  write_out( [ C1 ] ),
%  write_out_metta2( Level, Atom_list ).
%write_out_metta( Level, exclama_atom_list( Operatorx , _, _, Atom_list ) ):-
%  Sx = toString( Operatorx ),  write_level( Level ),
%  string5x::concat( "exclamax ", Sx, C1 ),  write_out([ C1 ]),  write_out_metta2( Level, Atom_list ).
%----
    increment(C2) :-
        retract(tel(C)),
        !,
        C2 = C + 1,
        assert(tel(C2)).

% MOVE THIS predicates section HIGHER IN THE FILE
predicates
% sequential_pred_names_and_vars
    sequential_pred_names_and_vars : (integer, integer, integer, string*, string* [out], string*, string* [out], integer*, integer* [out]) determ.
    vars_slist_remove_pred_out_vars : (integer, string, string*, string* [out], string* [out]).
% vars_slist_clean_pred_out_vars_names : ( string* , string* ) procedure( i, o ).
% transpile_subcl_metta_tpro_subc
% transpile_subcl_metta_tpro_subc : (  atom_list, sterml ) determ(   i, o ).
    transpile_subcl_metta_tpro_subc : (integer, integer, integer, integer, sterm, sterm [out], integer [out], string [out], integer [out]) determ.
    transpile_subcl_metta_tpro_subc2 : (integer, integer, integer, integer, string*, sterml, sterml [out]) determ.
    parse_output_var : (string, string [out]) determ.
    is_not_present_in_list : (string, string*) determ.
    transpile_output_variabels2 : (sterml, sterml [out]) determ.
    transpile_output_variabels : (sterm, sterm [out]) determ.
%---
% inventarise_subcl_metta_tpro_subc
    inventarise_subcl_metta_tpro_subc : (integer, integer, integer, integer, sterm) determ.
    inventarise_subcl_metta_tpro_subc2 : (integer, integer, integer, integer, sterml) determ.
    transpile_simpel_term : (integer, integer, integer, sterm, sterm [out]) determ.
    pre_transpile : (sterm, sterm [out]).
    transpile_default_via_invent : (integer, integer, integer, integer, string*, sterm, sterm [out]) determ.
    nested_pred : (string, integer, integer, integer [out], string [out], string* [out], string* [out], integer [out], integer* [out]) determ.
% enhance_main_level : () procedure().
% get_main_level : ( integer ) procedure( o ).
%transpile_subcl_metta_tpro_s : ( operator, atom_list , sterm ) determ( i, i , o ).
% non_nested_atomlist_tsubpro_list : (  atom_list, sterml ) determ(   i, o ).
    listi_lstr2_remove_head : (integer*, string*, integer [out], string [out], integer* [out], string* [out]).
    max_deep_variabels_nest_connected : (integer, integer*, string*, string*, string* [out], integer) determ.
    it_doesnt_have_add_atom_or_remove_atom_as_previous : (integer, string) determ.
    append_outvar_to_list : (string, string, string*, string* [out]).
    construct_implement_clause_body : (integer, integer, string, string*, integer, integer*, string*, integer, string*, sterml, sterml [out],
        string [out]) determ.
%-----
    construct_implement_clause : (integer, integer, integer, string, string*, integer, string, string*, integer*, string*, sterm [out],
        string* [out], integer [out]) determ.
    construct_subclauses : (integer, string*, integer*, string*) determ.
    remove_real_int_vals : (integer, string*, string* [out], string* [out]) determ.
    get_head_term_from_list : (sterml, sterm [out]).
    term_add_output_variabel : (sterm, string, sterm [out]).
    set_nth_merge : (integer, string*, integer, string, string*, string* [out]) determ.
    add_vars_in_list_in_positions : (integer*, string*, string*, string* [out]) determ.
    untag_pred_out_vars : (string*, string* [out]) determ.
    remove_pred_out_vars_varlist : (sterml, sterml [out], sterml [out]) determ.
    find_pred_out_var_or_assign : (sterml, sterm [out]).
    find_pred_out_var : (sterml, sterm [out]) determ.
    find_output_var : (string*, string, string [out], string* [out]).
    make_clause_body_line_end : (integer, string, string, string*, sterm [out], string [out]) determ.
% stdio_write_bodylist : ( core::charCount ,  sterml  ) determ( i, i ).
%---
    stdio_write_clause : (string, sterm).
    bodylist_to_string_format : (core::charCount, string, sterml, string [out]) determ.
    comma_or_dot : (sterml, string [out]).
    prolog_list_clause_to_string_format : (sterm, string, string [out]) determ.
    extra_new_line_for_type_of_predicate : (string, string [out]).
    copy_data_clause : (integer, sterm, sterm [out]) determ.
    head_prolog_term_string : (sterml, string [out]) determ.
    set_outvar_in_list : (integer, string*, string* [out], string [out]).
    list_split_outvars : (string*, string* [out], string* [out]) determ.
    outvars_at_the_end : (string*, string* [out]) determ.
    which_depth_to_use : (integer, integer, integer [out]).
    current_transpile_model_get : (integer [out], integer [out], integer [out], integer [out], integer [out]).
    transpile_head_term : (sterm, sterm [out], string [out], string* [out], integer [out]).
    const_name : (string [out], integer [out]) nondeterm.
    warning_if_it_is_already_declared : (integer, string) determ.
    arity_of_head_vars : (sterml, integer [out]).
    verify_predicates_exist : (sterm).
    verify_predicate_exist : (string, integer).
    verify_predicates_exist_body : (sterml) determ.
    validate_verify_the_program : ().
    string_predicate : (string [out], string [out], integer [out]) nondeterm.
    write_string_predicates : (outPutStream).

clauses
% clause_memory( Comment , TERMX_trans )
    std_metta_pred("division", 3, cmp(":-", [cmp("division", [var("X"), var("W"), var("Q")]), cmp("=", [var("Q"), cmp("/", [var("X"), var("W")])])])).
    std_metta_pred("multiplication", 3,
            cmp(":-", [cmp("multiplication", [var("X"), var("W"), var("Q")]), cmp("=", [var("Q"), cmp("*", [var("X"), var("W")])])])).

    std_metta_pred("plus", 3, cmp(":-", [cmp("plus", [var("X"), var("W"), var("Q")]), cmp("=", [var("Q"), cmp("+", [var("X"), var("W")])])])).
    std_metta_pred("minus", 3, cmp(":-", [cmp("minus", [var("X"), var("W"), var("Q")]), cmp("=", [var("Q"), cmp("-", [var("X"), var("W")])])])).

    std_metta_pred("db_dummy", 1, cmp(":-", [cmp("db_dummy", [atom("True")]), atom("!")])).

% std_metta_pred( "match variant 01", cmp(":-",[cmp("match",[var("_"),var("_"),var("Invx1"),var("Invx1")]),atom("!")]) ).
%std_metta_pred( "match variant 01", cmp(":-",[cmp("match",[var("_"),var("_"),var("Invx1"),var("Outvx1")]),cmp(",",[cmp("=",[var("Outvx1"),var("Invx1")]),atom("!")])]) ).
%std_metta_pred( "match variant 02", cmp(":-",[cmp("match_all",[var("_"),var("_"),var("Invx1"),var("Outvx1")]),cmp(",",[cmp("=",[var("Outvx1"),var("Invx1")]),atom("!")])]) ).
    std_metta_pred("check_is_equal custom ", 3,
            cmp(":-",
                [
                    cmp("check_is_equal", [var("Var1"), var("Var2"), str("True")]),
                    cmp(",", [cmp("=", [var("Var1"), nill]), cmp(",", [cmp("=", [var("Var2"), str("empty_atom")]), atom("!")])])
                ])).
    std_metta_pred("check_is_equal", 3,
            cmp(":-", [cmp("check_is_equal", [var("Var1"), var("Var2"), str("True")]), cmp(",", [cmp("=", [var("Var2"), var("Var1")]), atom("!")])])).
    std_metta_pred("check_is_equal catch", 3, cmp(":-", [cmp("check_is_equal", [var("_"), var("_"), str("False")]), atom("!")])).

%std_metta_pred( "condition_if",  cmp(":-",[cmp("condition_if",[var("Var1"),var("Var2"),var("Outvx2"),var("Outvar_e3")]),cmp(",",[cmp("=",[var("Var2"),var("Var1")]),cmp(",",[atom("!"),cmp("=",[var("Outvar_e3"),var("Outvx2")])])])]) ).
%std_metta_pred( "condition_if catch ",  cmp(":-",[cmp("condition_if",[var("_"),var("_"),var("_"),str("False")]),atom("!")]) ).
    std_metta_pred("condition_if", 4,
            cmp(":-",
                [
                    cmp("condition_if", [var("Var1"), var("Var2"), var("_"), var("Outvar_e3")]),
                    cmp(",", [cmp("=", [var("Var1"), str("True")]), cmp(",", [atom("!"), cmp("=", [var("Outvar_e3"), var("Var2")])])])
                ])).
    std_metta_pred("condition_if", 4,
            cmp(":-",
                [
                    cmp("condition_if", [var("_"), var("_"), var("Outvx2"), var("Outvar_e3")]),
                    cmp(",", [atom("!"), cmp("=", [var("Outvar_e3"), var("Outvx2")])])
                ])).
    std_metta_pred("condition_if", 4, cmp(":-", [cmp("condition_if", [var("_"), var("_"), var("_"), str("False")]), atom("!")])).

    std_metta_pred("car_atom", 2, cmp(":-", [cmp("car_atom", [list(var("Head"), var("_")), var("Head")]), atom("!")])).
    std_metta_pred("cdr_atom", 2, cmp(":-", [cmp("cdr_atom", [list(var("_"), var("Lis")), var("Lis")]), atom("!")])).
%std_metta_pred( "cons_atom",  cmp(":-",[cmp("cons_atom",[var("Head"),str("empty_atom"),list(var("Head"),nill)]),atom("!")]) ).
%std_metta_pred( "cons_atom",  cmp(":-",[cmp("cons_atom",[var("Head"),str("empty_atom"),list(var("Head"))]),atom("!")]) ).
%std_metta_pred( "cons_atom",  cmp(":-",[cmp("cons_atom",[var("Head"),var("Lis"),list(var("Head"),var("Lis"))]),atom("!")]) ).
% cons_atom( Head , "empty_atom" , [ Head ]  ):- ! .

    std_metta_pred("get_all_db_result", 3,
            cmp(":-",
                [
                    cmp("get_all_db_result", [var("Db_area"), var("Lis"), var("Outvx3")]),
                    cmp(",",
                        [
                            cmp("retract", [cmp("db_result", [var("Db_area"), var("El")])]),
                            cmp(",", [atom("!"), cmp("get_all_db_result", [var("Db_area"), list(var("El"), var("Lis")), var("Outvx3")])])
                        ])
                ])).
    std_metta_pred("get_all_db_result", 3, cmp(":-", [cmp("get_all_db_result", [var("Db_area"), var("Lisx"), var("Lisx")]), atom("!")])).

% this one must be  created via metta implement
% std_metta_pred( "score_cell", cmp(":-",[cmp("score_cell",[var("Head"),var("Head_new")]),cmp("=",[var("Head_new"),cmp("+",[var("Head"),int(1)])])]) ).
%  must eventually be changed to groupe_let in case that let or let star is before it
% std_metta_pred( "groupe", cmp(":-",[cmp("groupe",[var("_"),var("_"),var("Head_new"),var("Tail_new"),list(var("Head_new"),var("Tail_new"))]),atom("!")]) ).
% std_metta_pred( "let_star",  cmp(":-",[cmp("let_star",[var("_"),var("Outvx2"),var("Outvx2")]),atom("!")]) ).
    remove_dollar(H, Rs) :-
        string::tryFront(H, 1, Bg, Rs),
        Bg = "$",
        !.
    remove_dollar(H, H) :-
        !.
% H2 = string::replaceFirst( H, "$", "" ) , !.

    first_char_upper(H2, H3) :-
        string::tryFront(H2, 1, Bg, Rs),
        Bg2 = string::toUpperCase(Bg),
        H3 = string::concat(Bg2, Rs),
        !.
    first_char_upper(H2, H2) :-
        !.

    get_head_term_from_list([Head_term | _Rest], Head_term) :-
        !.

    get_head_term_from_list(_Bodylist_trans_piled, cmp("nohead", [])) :-
        !.

% for test
% remove_real_int_vals(  Varlist_pred_z  , Varlist_pred_z , [] ):- !.
% examples
%par_atom_list( equal(1,2),0,115,
% [metta_sub( par_atom_list(namex("get_cell_state",4,18),3,32,[variabel("$row",19,23),variabel("$column",24,31)])),
%   metta_sub( par_atom_list(namex("match",37,42),36,111,[ namex("&",43,44),namex("self",44,48),
%      metta_sub(par_atom_list(namex("sudoku_puzzle_state",50,69),49,96,[
%       variabel("$row",70,74),variabel("$column",75,82),variabel("$quad",83,88),variabel("$state",89,95)])),
%        variabel("$state",100,106)]))])
% to  examp
% [cmp( "func2" , [ list( var( "H" ) , var( "T" ) ) , var( "W" ) , var( "Q" ) ] ),
%     cmp(",",[cmp("=",[var("A"),cmp("+",[var("W"),var("H")])]),cmp("func2",[var("T"),var("A"),var("Q")])])])
%----
%non_nested_atomlist_tsubpro_list( [] , [] ) :- !.
%non_nested_atomlist_tsubpro_list( [ variabel( Strn, _, _ ) | Atom_List2 ] , [ var( Strn ) | Cmp_list ] ) :-  !,
%  non_nested_atomlist_tsubpro_list( Atom_List2  ,  Cmp_list  ).
% non_nested_atomlist_tsubpro_list( Atom_List2, Cmp_list ), ! .
%-----
% transpile_subcl_metta_tpro_s( Operat , Atom_List2 , Pro_sterm ),
% eerste versie
%transpile_subcl_metta_tpro_s( namex( Strname , _ , _ ) , Atom_List2 , cmp( Strname, Cmp_list)  ):-
%  non_nested_atomlist_tsubpro_list( Atom_List2, Cmp_list ), ! .
%transpile_subcl_metta_tpro_s( _Operat , _Atom_List2 , cmp( "default_nonparsed", [] ) ) :- !.
%---
%transpile_subcl_metta_tpro_subc(  [] , [] ) :- !.
%transpile_subcl_metta_tpro_subc(   [ metta_sub( par_atom_list( Operat, _, _, Atom_List2 ) ) | Lis ] , [ cmp( Strname, Atom_List3)  | Result_list ] ):-
%     has_no_sub_nesting2(  Atom_List2  , 0, Res ), Res = 0 ,
%      Operat = namex( Strname , _ , _ ),      non_nested_atomlist_tsubpro_list( Atom_List2, Atom_List3 ), !,
%	 transpile_subcl_metta_tpro_subc( Lis, Result_list ).
%transpile_subcl_metta_tpro_subc(   [ metta_sub( par_atom_list( Operat, Bg, End, Atom_List2 ) ) | Lis ] , Result_list  ):-
%	transpile_subcl_metta_tpro_subc(   Atom_List2, Atom_List3 ),       Operat = namex( Strname , _ , _ ), !,
%	transpile_subcl_metta_tpro_subc( [ cmp( Strname, Atom_List3)  | Lis ], Result_list ).
%transpile_subcl_metta_tpro_subc(   [ H | Lis ] , [ H | Result_list ] ):-
%	transpile_subcl_metta_tpro_subc(  Lis, Result_list ).
%   cmp("=",[cmp("sub",[cmp("get_cell_state",[
%  var("$row"),var("$column")])]),
    %  cmp("sub",[
    %   cmp("match",[atom("&"),atom("self"),
    %   cmp("sub",[cmp("sudoku_puzzle_state",[var("$row"),var("$column"),var("$quad"),var("$state")])]),var("$state")])])])
%cmp("sub_txt",[
%  cmp("sub_tst",[
%    cmp("get_cell_state",[var("$row"),var("$column")])]),
%     cmp("sub_tst",[
%      cmp("match",[atom("&"),atom("self"),
%      cmp("sub",[cmp("sudoku_puzzle_state",[var("$row"),var("$column"),var("$quad"),var("$state")])]),var("$state")])])])
%amount_of_sub_nesting2( [] , Res, Res ):- ! .
%amount_of_sub_nesting2( [ cmp( Operat,   Ato_lis  ) | Lis ] , Cou,  Res ):-     Operat = "sub" ,  ! , Cou2 = Cou + 1 ,
    %   amount_of_sub_nesting2(  Ato_lis  , Cou2, Cou3 ),    amount_of_sub_nesting2(  Lis  , Cou3, Res ),    ! .
%amount_of_sub_nesting2( [ cmp( _Operat,   Ato_lis  ) | Lis ] , Cou,  Res ):-      ! , Cou2 = Cou + 0 ,
    %   amount_of_sub_nesting2(  Ato_lis  , Cou2, Cou3 ),    amount_of_sub_nesting2(  Lis  , Cou3, Res ),    ! .
%amount_of_sub_nesting2( [ _H | Lis ] , Cou, Res ):- ! ,   amount_of_sub_nesting2( Lis  , Cou, Res ) , ! .
% inventarise_subcl_metta_tpro_subc2( _, [] ) :- !.
%                       transpiled  argument                                   transpiled( Count, Sx )
% inventarise_subcl_metta_tpro_subc2(  Level, [ cmp( Operat,  Atom_List2 )  | Lis ]  ):-
    %    Operat = "sub" ,
    %   amount_of_sub_nesting2(  Atom_List2  , 0 , Res ) ,  Res = 0 , ! ,
    %  try_get_first_predname( Atom_List2, Pname ),	  assert( predicate_nesting( 0, Level, Pname ) ),
    % inventarise_subcl_metta_tpro_subc2( Level, Lis ).
    %inventarise_subcl_metta_tpro_subc2( Level,  [ cmp( Operat,  Atom_List2 ) | Lis ]  ):-
    %Operat = "sub" ,
    %try_get_first_predname( Atom_List2, Pname ),	assert( predicate_nesting( 1, Level, Pname ) ),
    %inventarise_subcl_metta_tpro_subc2( Level,  Atom_List2 ) , !,
    %inventarise_subcl_metta_tpro_subc2( Level,  Lis  ).
    %inventarise_subcl_metta_tpro_subc2( Level,  [ H | Lis ]  ):- ! ,
    %inventarise_subcl_metta_tpro_subc2( Level, Lis  ).
%------------------------
    %inventarise_subcl_metta_tpro_subc( Level, cmp( Operat ,  Atom_List2 )   ):-
    %   Operat = "sub",     amount_of_sub_nesting2(  Atom_List2  , 0, Res ), Res = 0 ,
    %  try_get_first_predname( Atom_List2, Pname ),
%	 assert( predicate_nesting( 0, Level, Pname ) ),	 ! .
% inventarise_subcl_metta_tpro_subc( Level, cmp( Operat,  Atom_List )   ):-
    %  assert( predicate_nesting( 1, Level, Operat ) ),
    % inventarise_subcl_metta_tpro_subc2(  Level,  Atom_List  ).
% cmp("=",[cmp("sub",[cmp("get_cell_state",[var("$row"),var("$column")])]),cmp("sub",[cmp("match",[atom("&"),atom("self"),cmp("sub",[cmp("sudoku_puzzle_state",[var("$row"),var("$column"),var("$quad"),var("$state")])]),var("$state")])])])
% i,o
%        atom(string Value);
    %       int(integer Value);
    %      int64(integer64 Value);
    %     real(real Value);
    %    str(string Value);
    get_vars([], _, [], [], [], []) :-
        !.
% Cou , Lis_vars , _Lis_nums , Lis_nonvars, _Lis_nums_nonvars

    get_vars([var(Nm) | Lis], Cou, [Nm | Lisvars], [Cou | Lisnums], Lis_nonvars, Lis_nonnums) :-
        !,
        Cou2 = Cou + 1,
        get_vars(Lis, Cou2, Lisvars, Lisnums, Lis_nonvars, Lis_nonnums).

    get_vars([atom(Nm) | Lis], Cou, [Xs | Lisvars], [Cou | Lisnums], Lis_nonvars, Lis_nonnums) :-
        !,
        Cou2 = Cou + 1,
        Xs = string::concat("atomvar#", Nm),
        get_vars(Lis, Cou2, Lisvars, Lisnums, Lis_nonvars, Lis_nonnums).
    get_vars([real(Rea) | Lis], Cou, [Xs | Lisvars], [Cou | Lisnums], Lis_nonvars, Lis_nonnums) :-
        !,
        Cou2 = Cou + 1,
        Xs = string::concat("realvar#", toString(Rea)),
        get_vars(Lis, Cou2, Lisvars, Lisnums, Lis_nonvars, Lis_nonnums).

    get_vars([int(Rea) | Lis], Cou, [Xs | Lisvars], [Cou | Lisnums], Lis_nonvars, Lis_nonnums) :-
        !,
        Cou2 = Cou + 1,
        Xs = string::concat("intvar#", toString(Rea)),
        get_vars(Lis, Cou2, Lisvars, Lisnums, Lis_nonvars, Lis_nonnums).

    get_vars([str(Sx) | Lis], Cou, [Xs | Lisvars], [Cou | Lisnums], Lis_nonvars, Lis_nonnums) :-
        !,
        Cou2 = Cou + 1,
        Xs = string::concat("strvar#", Sx),
        get_vars(Lis, Cou2, Lisvars, Lisnums, Lis_nonvars, Lis_nonnums).

    get_vars([H | Lis], Cou, Lisvars, Lisnums, [H | Lis_nonvars], [Cou | Lis_nonnums]) :-
        !,
        Cou2 = Cou + 1,
        get_vars(Lis, Cou2, Lisvars, Lisnums, Lis_nonvars, Lis_nonnums).

% get_vars( [ atom( Nm ) | Lis ], Hl , Lis_vars ):- !, Xs = string::concat( "atomvar#", Nm ),  get_vars(  Lis , [ Xs | Hl ], Lis_vars ).
% get_vars( [ real( Rea ) | Lis ], Hl , Lis_vars ):- !, Xs = string::concat( "realvar#", toString(Rea) ),  get_vars(  Lis , [ Xs | Hl ], Lis_vars ).
% get_vars( [ int( Inte ) | Lis ], Hl , Lis_vars ):- !, Xs = string::concat( "intvar#", toString(Inte) ),  get_vars(  Lis , [ Xs | Hl ], Lis_vars ).
% get_vars( [ str( Inte ) | Lis ], Hl , Lis_vars ):- !, Xs = string::concat( "strvar#", toString(Inte) ),  get_vars(  Lis , [ Xs | Hl ], Lis_vars ).
% get_vars( [ _ | Lis ], Hl , Lis_vars ):- !,  get_vars(  Lis ,  Hl , Lis_vars ).
% debug_tag( Tg1, Tg1 ):-!.
    debug_tag(_, "") :-
        !.

    get_indent("  ") :-
        !.

% doesnt work
%newline_for_pred( _Func , "" ):- !.
%newline_for_pred( "retract" , " rt " ):- !.
%newline_for_pred( "retractall" , " rta " ):- !.
%newline_for_pred( "assertz" , " assz " ):- !.
%newline_for_pred( _Func , " nl \n" ):- !.
%newline_for_pred( Func , " rt " ):- previous_pred( Pred ) , Pred = "retract" , retract( previous_pred( _ ) ),!,  assert( previous_pred( Func ) )  .
%newline_for_pred( Func , " rta " ):- previous_pred( Pred ) , Pred = "retractall" , retract( previous_pred( _ ) ), ! ,  assert( previous_pred( Func ) )  .
%newline_for_pred( Func , " assz " ):- previous_pred( Pred ) , Pred = "Ã¤ssertz" ,  retract( previous_pred( _ ) ), ! ,  assert( previous_pred( Func ) )  .
%newline_for_pred( Func , Varlist,  " rt " ):- Varlist = [ Head | _ ] , Head = cmp( F2, _ ) , Func = "retract" , ! .
%newline_for_pred( Func , Varlist,  " rta " ):- Varlist = [ Head | _ ] , Head = cmp( F2, _ ) , Func = "retractall" , ! .
%newline_for_pred( Func , Varlist,  " assz " ):- Varlist = [ Head | _ ] , Head = cmp( F2, _ ) , Func = "assertz" , ! .
%newline_for_pred( Func , Varlist,  " rt \n" ):-  Func = "retract" , ! .
%newline_for_pred( Func , Varlist,  " rta \n" ):-  Func = "retractall" , ! .
%newline_for_pred( Func , Varlist,  " assz \n " ):-  Func = "assertz" , ! .
%newline_for_pred( _Func , _ , " xt2 \n" ):- !.
%newline_for_pred( Func , Varlist,  " \n  " ):-  Func = "retract" , ! .
%newline_for_pred( Func , Varlist,  " \n  " ):-  Func = "retractall" , ! .
%newline_for_pred( Func , Varlist,  " \n  " ):-  Func = "assertz" , ! .
    newline_for_pred(_Func, _, S) :-
        get_indent(Sx),
        S = string::concat(" \n", Sx),
        !.

%newline_for_pred( _Func , _ , "\n" ):- !.
%newline_for_pred( Func , " rta " ):- previous_pred( Pred ) , Pred = "retractall" , retract( previous_pred( _ ) ), ! ,  assert( previous_pred( Func ) )  .
%newline_for_pred( Func , " assz " ):- previous_pred( Pred ) , Pred = "Ã¤ssertz" ,  retract( previous_pred( _ ) ), ! ,  assert( previous_pred( Func ) )  .
    % stdio::write( " prv-", Pred, " cur-" , Func, "\n" ),
%newline_for_pred( Func , "\n" ):- Func = "retract" , retractall( previous_pred( _ ) ),    assert( previous_pred( Func ) ) , !.
%newline_for_pred( Func , "\n" ):- Func = "retractall" , retractall( previous_pred( _ ) ),    assert( previous_pred( Func ) ) , !.
%newline_for_pred( Func , "\n" ):- Func = "assertz" , retractall( previous_pred( _ ) ),    assert( previous_pred( Func ) ) , !.
% newline_for_pred( Func , "\n" ):-  assert( previous_pred( Func ) ) , !.
    cmp_element_to_metta_string(var(Nm), Nm2, "") :-
        Nm2 = string::concat(" $", Nm, " "),
        !.

    cmp_element_to_metta_string(cmp(Func, Varslist), Qw, "xppqqaa1") :-
        cmp_elements_to_metta_string(Varslist, "", Sx),
%       Indent = "",
%  Qw = string::concat(  Dbotag1 , Indent, "\n" , Sx , Dbotag2  ) , !.
% uuyytt
%  Qw = string::concat(  Dbotag1 , "\n" , Indent, Sx , Dbotag2  ) , ! .
% remove nl
        Qw = string::concat(" (", Func, " ", Sx, ") "),
        !.

    cmp_element_to_metta_string(El, Nm3, "") :-
        Nm2 = toString(El),
        Nm3 = string::concat(" ", Nm2, " "),
        !.

%----
    element_to_string(var(Nm), Nm2, "") :-
        Le = string::length(Nm),
        Le > 8,
        string::tryFront(Nm, 8, Bg, Nm2),
        Bg = "Realvar#",
        !.

    element_to_string(var(Nm), Nm3, "") :-
        Le = string::length(Nm),
        Le > 8,
        string::tryFront(Nm, 8, Bg, Nm2),
        Bg = "Atomvar#",
        Nm3 = string::concat("\"", Nm2, "\""),
        !.

    element_to_string(var(Nm), Nm, "") :-
        !.
    element_to_string(nill, "[]", "") :-
        !.

    element_to_string(atom(Nm), Nm, "") :-
        Nm = "!",
        !.
    element_to_string(atom(Nm), Nm, "") :-
        Nm = "fail",
        !.

    element_to_string(atom(Nm), Nm2, "") :-
        Nm2 = string::concat("\"", Nm, "\""),
        !.
    element_to_string(str(Nm), Nm2, "") :-
        Nm2 = string::concat("\"", Nm, "\""),
        !.

% 08:33 14-7-2025
    element_to_string(real(Nm), Nm2, "") :-
        Nm2 = toString(Nm),
        !.
    element_to_string(int(Nm), Nm2, "") :-
        Nm2 = toString(Nm),
        !.

    % Out = string::concat( Hs , "\n" , Space , toString( cmp( Funcname, Lisx ) ) , ".\n" )  , ! .
%cmp(":-",[cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),var("Lis"),var("Lis_res")]),
% cmp(",",[
%   cmp("retract",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")])]),
%  cmp(",",[atom("!"),
%   cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),
%    list(cmp("cell_candidatex",[var("B"),var("Num")]),var("Lis")),
%	 var("Lis_res")])
%	 ])])])
    element_to_string(list(Headl, Restl), Qw, "") :-
        var_list_to_prolog_vars_string([Headl], "", Sx1),
        var_list_to_prolog_vars_string([Restl], "", Sx2),
        Qw = string::concat("[ ", Sx1, " | ", Sx2, " ] "),
        !.

%element_to_string( cmp( Func, Varslist ) , Qw ):-  Func = ",",
%  Varslist = [ Head | Rest ],  Head = atom( Aval ) , Aval = "!" ,
%  var_list_to_prolog_vars_string( Rest , "" , Sx ) ,
%  Qw = string::concat( " pp,!, \n ( " , Sx , " )" ) , !.
    element_to_string(cmp(Func, Varslist), Qw, "xt1") :-
        Func = ",",
        var_list_to_prolog_vars_string(Varslist, "", Sx),
        debug_tag("aau- ", Dbotag1),
        debug_tag("-uaa ", Dbotag2),
        Indent = "",
%  Qw = string::concat(  Dbotag1 , Indent, "\n" , Sx , Dbotag2  ) , !.
% uuyytt
%  Qw = string::concat(  Dbotag1 , "\n" , Indent, Sx , Dbotag2  ) , ! .
% remove nl
        Qw = string::concat(Dbotag1, Indent, Sx, Dbotag2, ""),
        !.
%  Qw = string::concat(  Dbotag1 , Indent, Sx , Dbotag2 , "\n"  ) , ! .

% 10:31 17-7-2025
% cmp(":-",[cmp("check_is_equal",[var("Var1"),var("Var2"),str("True")]),cmp(",",[cmp("=",[var("Var1"),nill]),cmp(",",[cmp("=",[var("Var2"),str("empty_atom")]),atom("!")])])])
    element_to_string(cmp(Func, Varslist), Qw, Nls) :-
        Func = "=",
        Varslist = [Var1, Var2],
        Var1 = var(Vs1),
        Var2 = nill,
        !,
        debug_tag(" ppzzi- ", Dbotag1),
        Indent = "",
        newline_for_pred(Func, Varslist, Nls),
        Qw = string::concatList([Dbotag1, Indent, Vs1, " = []"]).

    element_to_string(cmp(Func, Varslist), Qw, Nls) :-
        Func = "=",
        Varslist = [Var1, Var2],
        Var1 = var(Vs1),
        Var2 = str(Vs2),
        !,
        debug_tag(" ppzzi- ", Dbotag1),
        Indent = "",
        newline_for_pred(Func, Varslist, Nls),
        Qw = string::concatList([Dbotag1, Indent, Vs1, " = \"", Vs2, "\""]).

% TODO make this more general to have less code
% 10:31 17-7-2025
% % %  cmp(",",[cmp("=",[var("Var2"),var("Var1")]),cmp(",",[atom("!"),cmp("=",[var("Outvar_e3"),var("Outvx2")])])])
    element_to_string(cmp(Func, Varslist), Qw, Nls) :-
        Func = "=",
        Varslist = [Var1, Var2],
        Var1 = var(Vs1),
        Var2 = var(Vs2),
        % var_list_to_prolog_vars_string( Varslist , "" , Sx ),
        debug_tag(" ppzzi- ", Dbotag1),
        Indent = "",
        % lastxxx
        newline_for_pred(Func, Varslist, Nls),
        % Tmps = string::concat( Nls, Indent ) ,
%  Tmps =  Indent  ,
%  Qw = string::concatList( [ Dbotag1 , Indent , Vs1 , " = " , Vs2 , " ,kma-doubt" ] ) , ! .
        Qw = string::concatList([Dbotag1, Indent, Vs1, " = ", Vs2]),
        !.

% 14:37 30-7-2025
    element_to_string(cmp(Func, Varslist), Qw, Nls) :-
        Func = "=",
        Varslist = [Var1, Var2],
        Var1 = var(Vs1),
        Var2 = atom(Vs2),
        debug_tag(" ppzzj- ", Dbotag1),
        Indent = "",
        newline_for_pred(Func, Varslist, Nls),
        Qw = string::concatList([Dbotag1, Indent, Vs1, " = \"", Vs2, "\""]),
        !.

% todo   indent spaces
    element_to_string(cmp(Func, Varslist), Qw, Nls) :-
        var_list_to_prolog_vars_string(Varslist, "", Sx),
        debug_tag(" pp- ", Dbotag1),
        Indent = "",
        % lastxxx
        newline_for_pred(Func, Varslist, Nls),
        % Tmps = string::concat( Nls, Indent ) ,
%  Tmps =  Indent  ,
        Qw = string::concatList([Dbotag1, Indent, Func, "( ", Sx, " )"]),
        !.

    element_to_string(_, "", "") :-
        !.
%-----

    cmp_elements_to_metta_string([], Crea_str2, Crea_str2) :-
        !.
    cmp_elements_to_metta_string([Element], Crea_str, Crea_str2) :-
        cmp_element_to_metta_string(Element, Sx, _),
        !,
        Crea_str2 = string::concat(Crea_str, Sx),
        !.

    cmp_elements_to_metta_string([Element | Lis], Crea_str, Vars_Str) :-
        cmp_element_to_metta_string(Element, Sx, Nls),
        !,
        Crea_str2 = string::concat(Crea_str, Sx, " , ", Nls),
        cmp_elements_to_metta_string(Lis, Crea_str2, Vars_Str).

%----
    var_list_to_prolog_vars_string([], Crea_str2, Crea_str2) :-
        !.
    var_list_to_prolog_vars_string([Element], Crea_str, Crea_str2) :-
        element_to_string(Element, Sx, _),
        !,
        Crea_str2 = string::concat(Crea_str, Sx),
        !.

    var_list_to_prolog_vars_string([Element | Lis], Crea_str, Vars_Str) :-
        element_to_string(Element, Sx, Nls),
        !,
        Crea_str2 = string::concat(Crea_str, Sx, " , ", Nls),
        var_list_to_prolog_vars_string(Lis, Crea_str2, Vars_Str).

%----
% [cmp("sub",[
%  cmp("match",[
%    atom("&self"),
    %cmp("sub",[
%	  cmp("tempnum",[var("$num")])]),atom("True")])
%  ])]
%asser_match_db_predicate2( _Tp, [] ):- !.
%asser_match_db_predicate2( Tp_match , [ cmp( Op, Lis ) | Rs ] ):- Op = "sub" ,
% Lis = [ Head | Rest ] , Head = cmp( Db_pred, _Varslist ), !, assert( is_match_db_pred( Tp_match, Db_pred ) ), ! .
%asser_match_db_predicate2( Tp_match, [ _ | Rs ] ):-  !,  asser_match_db_predicate2( Tp_match, Rs  ).
%---
% dbuu succes1 cmp("sub",[
%cmp("condition_if",[
% cmp("sub",[
%  cmp("check_is_equal",[
%   cmp("sub",[cmp("exist_as_false_candidate",[var("$levelx"),var("$r"),var("$c"),var("$num")])]),atom("True")])]),atom("empty"),var("$num")])])
% TODO FINDALL  is_match_db_pred
% and PUT them in the database clause, AND Reject them in the other bodys
% REMOVE
%asser_match_db_predicate2( Mlevel , Level0, Level, Tp, Db_pred , Db_pred_result ):-
% Db_pred_result =  cmp( _Db_predname_res , Db_pred_res_vars_list ) , Db_pred_res_vars_list = [ El1 ] ,
% Db_pred =  cmp( Db_predname , _Db_pred_vars_list ) ,
% stdio::write( "dbuu succes1 ", toString( Db_pred_result ) ,"\n" ) ,
% assert( is_match_db_pred( Mlevel , Level0, Level,  Tp, Db_predname , Db_pred,  El1 ) ) , ! .
%asser_match_db_predicate2( Mlevel , Level0, Level, Tp, Db_pred , Db_pred_result ):-
% Db_pred =  cmp( Db_predname , _Db_pred_vars_list ) ,
% stdio::write( "dbuu succes2 ", toString( Db_pred_result ) ,"\n" ) ,
% assert( is_match_db_pred( Mlevel , Level0, Level, Tp, Db_predname , Db_pred , Db_pred_result ) ) , ! .
% cmp("equal",[cmp("sub",[cmp("init_get_candidates",[])]),cmp("sub",[cmp("match",[atom("&self"),cmp("sub",[cmp("sudoku_number",[var("$num"),var("$a"),var("$b")])]),var("$num")])])])
%cmp("equal",[
% cmp("sub",[cmp("init_get_candidates",[])]),
%  cmp("sub",[
%    cmp("match",[ atom("&self"),
%	  cmp("sub",[cmp("sudoku_number",[var("$num"),var("$a"),var("$b")])]),
%	  var("$num") 	  ]) 	])  ])
    try_peal_off_outer(El3_db_result0, El1) :-
        El3_db_result0 = cmp(_Db_predname_res, Db_pred_res_vars_list),
        Db_pred_res_vars_list = [El1],
        !.
    try_peal_off_outer(El3_db_result, El3_db_result) :-
        !.
%---
    judge_db_output_term(El3_db_result, is_db_result_execute) :-
        El3_db_result = cmp(_Db_predname_res, Db_pred_res_vars_list),
        amount_of_sub_nesting2(Db_pred_res_vars_list, 0, Res),
        Res > 0,
        !.

    judge_db_output_term(El3_db_result, is_db_result_term) :-
        El3_db_result = cmp(_Db_predname_res, _Db_pred_res_vars_list),
        !.

    judge_db_output_term(El3_db_result, is_db_result_variabel) :-
        El3_db_result = var(Vname),
        !.
        % dollarx

    judge_db_output_term(_El3_db_result, 0) :-
        !.
%---
% dbuu succes1
%cmp("sub",[
% cmp("condition_if",[cmp("sub",[cmp("check_is_equal",[cmp("sub",[cmp("exist_as_false_candidate",[var("$levelx"),var("$r"),var("$c"),var("$num")])]),atom("True")])]),atom("empty"),var("$num")])])
%			 Cmp1_el_sub = cmp( Op_komma , Lis_komma ) ,
%  stdio::write( "dbuu-detect ", toString( El2_db ) , "\n" ) ,   fail,
%		 asser_match_db_predicate2( "match" , Db_pred, El3_db_result ) , ! .
% detect cmp("sub",[
        %   cmp("komma",[
        %       cmp("sub",[
        %           cmp("cell_candidate",[  var("$r"),var("$c"),var("$num")])
        %     ]),    cmp("sub" , [ cmp( "not_exist_as_false_candidate" , [ var( "$levelx"),var("$r"),var("$c"),var("$num")])])])])

    asser_match_db_predicate(Cou, Mlevel, Level0, Level, [cmp(Op, Lis) | Rs]) :-
        % enum_predicate( Cou, "match", Match ),
        Op = "match",
        Lis = [_El1_db_area, El2_db, El3_db_result0],
        try_peal_off_outer(El3_db_result0, El3_db_result),
        judge_db_output_term(El3_db_result, Judgement),
        _El1_db_area = atom(_El1_db_areax),
        El2_db = cmp(_Opx, Lis_db_pred),
        Lis_db_pred = [Pred_komma],
        Pred_komma = cmp(Op_komma, Lis_komma),
        Op_komma = "komma",
        Lis_komma = [Komma_el1, Komma_el2],
        Komma_el1 = cmp(Op_el1_sub, Lis_db_predx1),
        Lis_db_predx1 = [Db_pred1],
        Db_pred1 = cmp(Db_predname1, _Db_pred_vars_list1),
        Komma_el2 = cmp(Op_el2_sub, Lis_db_predx2),
        Lis_db_predx2 = [Db_pred2],
        Db_pred2 = cmp(Db_predname2, _Db_pred_vars_list2),
        !,
        % enum_predicate( Cou, Db_predname1, Db_predname1x ),
        % enum_predicate( Cou, Db_predname2, Db_predname2x ),
        Db_predname1x = string::concat(Db_predname1, toString(Cou)),
        %Db_predname2x = string::concat( Db_predname2, toString( Cou ) ),
        % Match = string::concat( "match", toString( Cou ) ),
        assert(
            is_match_db_pred(query1, _El1_db_areax, Mlevel, Level0, Level, Cou, Db_predname1, Op, Db_predname1x,
                cmp(Db_predname1x, _Db_pred_vars_list1), El3_db_result, Judgement)),
        assert(
            is_match_db_pred(query2, _El1_db_areax, Mlevel, Level0, Level, Cou, Db_predname2, Op, Db_predname2,
                cmp(Db_predname2, _Db_pred_vars_list2), El3_db_result, Judgement)).

% Complete copy with 1 word changed match = match_all
    asser_match_db_predicate(Cou, Mlevel, Level0, Level, [cmp(Op, Lis) | Rs]) :-
        % enum_predicate( Cou, "match_all", Match_all ),
        Op = "match_all",
        Lis = [_El1_db_area, El2_db, El3_db_result0],
        try_peal_off_outer(El3_db_result0, El3_db_result),
        judge_db_output_term(El3_db_result, Judgement),
        _El1_db_area = atom(_El1_db_areax),
        El2_db = cmp(_Opx, Lis_db_pred),
        Lis_db_pred = [Pred_komma],
        Pred_komma = cmp(Op_komma, Lis_komma),
        Op_komma = "komma",
        Lis_komma = [Komma_el1, Komma_el2],
        Komma_el1 = cmp(Op_el1_sub, Lis_db_predx1),
        Lis_db_predx1 = [Db_pred1],
        Db_pred1 = cmp(Db_predname1, _Db_pred_vars_list1),
        Komma_el2 = cmp(Op_el2_sub, Lis_db_predx2),
        Lis_db_predx2 = [Db_pred2],
        Db_pred2 = cmp(Db_predname2, _Db_pred_vars_list2),
        !,
        %enum_predicate( Cou, Db_predname1, Db_predname1x ),
        %enum_predicate( Cou, Db_predname2, Db_predname2x ),
        Db_predname1x = string::concat(Db_predname1, toString(Cou)),
        %Db_predname2x = string::concat( Db_predname2, toString( Cou ) ),
        % Match_all = string::concat( "match_all", toString( Cou ) ),
        assert(
            is_match_db_pred(query1, _El1_db_areax, Mlevel, Level0, Level, Cou, Db_predname1, Op, Db_predname1x,
                cmp(Db_predname1x, _Db_pred_vars_list1), El3_db_result, Judgement)),
        assert(
            is_match_db_pred(query2, _El1_db_areax, Mlevel, Level0, Level, Cou, Db_predname2, Op, Db_predname2,
                cmp(Db_predname2, _Db_pred_vars_list2), El3_db_result, Judgement)).

    asser_match_db_predicate(Cou, Mlevel, Level0, Level, [cmp(Op, Lis) | Rs]) :-
        % enum_predicate( Cou, "match_all", Match_all ),
        Op = "match_all",
        Lis = [_El1_db_area, El2_db, El3_db_result0],
        try_peal_off_outer(El3_db_result0, El3_db_result),
        judge_db_output_term(El3_db_result, Judgement),
        _El1_db_area = atom(_El1_db_areax),
        El2_db = cmp(_Opx, Lis_db_pred),
        Lis_db_pred = [Db_pred1],
        Db_pred1 = cmp(Db_predname1, _Db_pred_vars_list1),
        !,
        Db_predname1x = string::concat(Db_predname1, toString(Cou)),
        %		   enum_predicate( Cou, Db_predname1, Db_predname1x ),
        % enum_predicate( Cou, "match_all", Match_all ),
        % Match_all = string::concat( "match_all", toString( Cou ) ),
        assert(
            is_match_db_pred(query1, _El1_db_areax, Mlevel, Level0, Level, Cou, Db_predname1, Op, Db_predname1x,
                cmp(Db_predname1x, _Db_pred_vars_list1), El3_db_result, Judgement)).

    asser_match_db_predicate(Cou, Mlevel, Level0, Level, [cmp(Op, Lis) | Rs]) :-
        % enum_predicate( Cou, "match", Match ),
        Op = "match",
        Lis = [_El1_db_area, El2_db, El3_db_result0],
        try_peal_off_outer(El3_db_result0, El3_db_result),
        judge_db_output_term(El3_db_result, Judgement),
        _El1_db_area = atom(_El1_db_areax),
        El2_db = cmp(_Opx, Lis_db_pred),
        Lis_db_pred = [Db_pred1],
        Db_pred1 = cmp(Db_predname1, _Db_pred_vars_list1),
        !,
        Db_predname1x = string::concat(Db_predname1, toString(Cou)),
        % enum_predicate( Cou, Db_predname1, Db_predname1x ),
        % enum_predicate( Cou, "match", Match ),
        % Match = string::concat( "match", toString( Cou ) ),
        assert(
            is_match_db_pred(query1, _El1_db_areax, Mlevel, Level0, Level, Cou, Db_predname1, Op, Db_predname1x,
                cmp(Db_predname1x, _Db_pred_vars_list1), El3_db_result, Judgement)).

%  asser_match_db_predicate2( "match_all", Lis ) , ! .
% asser_match_db_predicate( [ cmp( Op, Lis ) | Rs ] ):- Op = "match" ,   asser_match_db_predicate2( "match", Lis ) , ! .
    asser_match_db_predicate(_, _, _, _, _Atom_List2) :-
        !.

% bij match all something like this
%uicici cmp(":-",[cmp("tempnum_db",[var("Lis0"),var("Lis_res")]),cmp(",",[cmp("retract",[cmp("tempnum",[var("Num")])]),cmp(",",[atom("!"),cmp("tempnum_db",[list(var("Num"),var("Lis0")),var("Lis_res")])])])])
%uicici cmp(":-",[cmp("tempnum_db",[var("Lis_res"),var("Lis_res")]),atom("!")])
%----
% cmp("sub",[cmp("cell_candidatex",[var("$b"),var("$num")])] )
% cmp("cell_candidatex",[var("$b"),var("$num")])
%---
%is_match_db_pred(501,0,0,0,"match","cell_candidate",
% cmp("cell_candidate",[var("$r"),var("$c"),var("$num")]),
%  cmp("condition_if",[cmp("sub",[cmp("check_is_equal",[cmp("sub",[cmp("exist_as_false_candidate",[var("$levelx"),var("$r"),var("$c"),var("$num")])]),atom("True")])]),atom("empty"),var("$num")])).
%is_match_db_pred(502,0,0,0,"match","not_exist_as_false_candidate",
% cmp("not_exist_as_false_candidate",[var("$levelx"),var("$r"),var("$c"),var("$num")]),
%  cmp("condition_if",[cmp("sub",[cmp("check_is_equal",[cmp("sub",[cmp("exist_as_false_candidate",[var("$levelx"),var("$r"),var("$c"),var("$num")])]),atom("True")])]),atom("empty"),var("$num")])).
% went wrong
% Doubt here
    hide_fail_if_is_inside_db_pred(_Callingpred, _Type_s, Pname, _Varslist3, _Sx, Pname) :-
        is_match_db_pred(_, _, _, _, _, _Cou, _Predx, _Type_match, Pname, _, _Pred_result, _Judge),
        !,
        % Type_match = "match" , !,
        stdio::write(" FAILS matchxx db ", Pname, "\n"),
        fail.

% fail_create_db_predicate
% hide_fail_if_is_inside_db_pred
% TODO here
%hide_fail_if_is_inside_db_pred( Pname,  _Varslist3, _Sx,   Pname ):-
% is_match_db_pred( _, _, _ , _ , _ , _Cou, _Predx , _Type_match , _Pname , _, Pred_db_result , _Judge ) ,
% Pred_db_result = cmp( Pname_pred , _ ) ,
% stdio::write( " Check if FAILS matchxx db ", Pname , " - " , Pname_pred , "\n" ) ,
% Pname = "komma6",  Pname_pred = "alternative_field" ,
% stdio::write( " Check if FAILS matchxx db 99zz " , Pname , " - " , Pname_pred , "\n" ) , !, fail.
% *********------
% This one is not nessecary because it is already built in the DB predicate
%hide_fail_if_is_inside_db_pred( _Callingpred , _Type_s, Pname,  _Varslist3, _Sx,   Pname ):-
%  is_match_db_pred( _, _, _ , _ , _ , _Cou, _Predx , _Type_match , _Pnamex , _, Pred_db_result , Judge ) , Judge = is_db_result_term ,
%  Pred_db_result = cmp( Pname_pred , _ ) ,
%  clause_memory( _ , _ , _ , Pname_pred , _  ) , !.
% try without
% IS NOT Nessecary because it is  applied inside the DB predicate
    % IF IT is declared earlier as a predicate then dont fail
    hide_fail_if_is_inside_db_pred(Callingpred, _Type_s, Pname, _Varslist3, _Sx, Pname) :-
%  _Type_s = "nested_pred_memory",
        _Type_s = "predicate_nesting_ordered",
        % it works like this for   find_first_field_which_had_alternatives  but its luck
        % lucky gues
        is_match_db_pred(_, _, _, _, _, _Cou, _Predx, _Type_match, _Pnamex, _, Pred_db_result, Judge),
        Judge = is_db_result_term,
        Pred_db_result = cmp(Pname_pred, _),
        % fail,
        % TEMP
        stdio::write(" Check if FAILS matchxx0w db ", Callingpred, " - ", _Type_s, " ", Pname, " - ", _Pnamex, " - ", Pname_pred, " - ", _Predx, "\n"),
        %                                            komma6 -     had_alternatives - alternative_field - had_alternatives
        % Mlevel , Level0 , Level0
        predicate_nesting_ordered(_, _, Level0, Level1, Level, Pname, _, _, _, _, _),
        stdio::write("\n Exist nested pred Check if FAILS matchxx0w db \n"),
        % tchxx0w db                                  komma6 - had_alternatives - alternative_field - had_alternatives
        % predicate_nesting_ordered(2,1,1,1,2,"komma6",[],[],2,2,2).
        % predicate_nesting_ordered(5,2,1,1,2,"alternative_field",["$lev","$r","$c","$last_cand"],[1,2,3,4],0,0,4).
        predicate_nesting_ordered(_, _, Level0, Level1, Level, Pname_pred, _, _, _, _, _),
        !,
        stdio::write("\n Exist nested pred2 Check if FAILS matchxx0w db \n"),
% nested_pred_memory(_ , _ , _ , _ , _ , Pname_pred , _ , _Constructed_name , _ , _ , _ , _) ,
        % Pname_pred = Pname , ! ,
        stdio::write(" \n Succeed FAILS matchyy db0w ", Pname, " - ", _Pnamex, " - ", Pname_pred, " - ", _Predx, "\n"),
        fail.

% TODO if we have for example a nested If predicate with output variabel, then we have to set that result in create_db_arg
% TODO Doubt here
    hide_fail_if_is_inside_db_pred(_Callingpred, _Type_s, Pname, _Varslist3, _Sx, Pname) :-
        is_match_db_pred(_, _, _, _, _, _Cou, _Predx, _Type_match, _Pname, _, Pred_db_result, _Judge),
        Pred_db_result = cmp(Pname_pred, _),
        stdio::write(" Check if FAILS matchxx db ", Pname, " - ", Pname_pred, "\n"),
        % tmp  Pname_pred = "condition_if"
        % Pname_pred = Pname , ! ,
        % constr_clauses(5,2,1,1,2,"condition_if","condition_if_check_is_equal",["check_is_equal_exist_as_false_candidate"],[6],[2,3],["condition_if","check_is_equal"]).
        % constr_clauses(_,_,_,_,_, Pname_pred , Constructed_name , _ , _ , _ , _ ) ,
% nested_pred_memory(_ , _ , _ , _ , _ , _Cou, _Predx , Pname_pred , _ , _Constructed_name , _ , _ , _ , _) ,
        nested_pred_memory(_, _, _, _, _, Pname_pred, _, _Constructed_name, _, _, _, _),
        Pname_pred = Pname,
        !,
        stdio::write(" FAILS matchyy db ", Pname, "\n"),
        fail.
    hide_fail_if_is_inside_db_pred(_Callingpred, _Type_s, Pname, _Varslist3, _Sx, Pname) :-
        !.

%uicici cmp(":-",[cmp("sudoku_number_create_db",[var("Num"),var("A"),var("B")]),cmp(",",[cmp("retractall",[cmp("sudoku_number_x",[var("_"),var("_"),var("_")])]),cmp(",",[cmp("sudoku_number",[var("Num"),var("A"),var("B")]),cmp(",",[cmp("asserta",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")])]),cmp(",",[atom("fail"),atom("!")])])])])])
%uicici cmp(":-",[cmp("sudoku_number_create_db",[var("_"),var("_"),var("_")]),atom("!")])
%uicici cmp(":-",[cmp("sudoku_number_db",[
% var("Num"),var("A"),var("B"),var("Lis"),var("Lis_res")]),
%  cmp(",",[cmp("retract",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")])    ]),
%   cmp(",",[atom("!"),
%    cmp("sudoku_number_db",[ var("Num"),var("A"),var("B"),
%	 list(cmp("cell_candidatex",[var("B"),var("Num")]),	  var("Lis")),
%  var("Lis_res")]  )]  )]  )
% ])
%uicici cmp(":-",[cmp("sudoku_number_db",[var("_"),var("_"),var("_"),var("Lis_res"),var("Lis_res")]),atom("!")])
%cmp(":-",[cmp("sudoku_number_db",[
%  var("Num"),var("A"),var("B"),var("Lis"),var("Lis_res")]),
%  cmp(",",[cmp("retract",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")]) ]),
%  cmp(",",[atom("!"),
%  cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),
%  list(cmp("cell_candidatex",[var("B"),var("Num")]),  var("Lis")),
%  var("Lis_res")]  )]  )]  )
%  ])
%    Var_var_listyy = list::append( Var_var_list, [ atom( "!" ) , cmp( Db_predname_res , Db_pred_res_vars_list2 ) ] ) ,
%  cmp( "," , Var_var_listyy )
% nill_list
%add_db_input_list( Varlist_pred0, _Varnames_head , Sub_pred, Varlist_pred ):-   is_match_db_pred( "match_all" , Sub_pred , _Pred_result ) , !,
%	 Varlist_pred = list::append( Varlist_pred0, [ "nill_list" ] ).
    db_var_list(_Varlist_pred0, Varnames_head, Nested_name, Varlist_pred) :-
%  Sub_pred = "match_all" ,
        nested_pred_memory(_, _, _, _, _, "match_all", _, Nested_name, _Pred_varlist, _, _, _Subnames),
        Varlist_pred = Varnames_head,
        !.
        % is_match_db_pred( "match_all" , Sub_pred , _Pred_result ) , !,
%	 Varlist_pred = list::append( Varnames_head , [ "nill_list" ] ).

    db_var_list(Varlist_pred, _Varnames_head, _Sub_pred, Varlist_pred) :-
        !.

%---
% not needed anymore
% add_db_input_list( _Varlist_pred0, Varnames_head , Sub_pred, Varlist_pred ):-   is_match_db_pred( _,_,_,"match_all" , Sub_pred , _, _Pred_result ) , !,
%	 Varlist_pred = list::append( Varnames_head , [ "nill_list" ] ).
    add_db_input_list(Varlist_pred, _Varnames_head, _Sub_pred, Varlist_pred) :-
        !.
        % Varlist_pred = list::append( Varlist_pred0, [ "add_inp_lis[]" ] ),

%uicici cmp(":-",[cmp("sudoku_number_create_db",[var("Num"),var("A"),var("B")]),cmp(",",[cmp("retractall",[cmp("sudoku_number_x",[var("_"),var("_"),var("_")])]),cmp(",",[cmp("sudoku_number",[var("Num"),var("A"),var("B")]),cmp(",",[cmp("asserta",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")])]),cmp(",",[atom("fail"),atom("!")])])])])])
%uicici cmp(":-",[cmp("sudoku_number_create_db",[var("_"),var("_"),var("_")]),atom("!")])
% uicici cmp(":-",[cmp("sudoku_number_create_db",[]),atom("!")])
% nested_pred_memory(3,2,1,1,2,"condition_if",2,"condition_if_check_is_equal",["atomvar#empty","$num","atomvar#True"],[2,3],2,["condition_if","check_is_equal"]).
% nested_pred_memory(3,2,1,1,2, Db_predname_res ,2,"condition_if_check_is_equal",["atomvar#empty","$num","atomvar#True"],[2,3],2,["condition_if","check_is_equal"]).
%*  Var_var_listyy = list::append( Var_var_list, [ list( cmp( Db_predname_res , Db_pred_res_vars_list2 ), var("Lis") ) , var("Lis_res") ] ) ,
% here the sort of head is tag67 here the if which is inside the match
% 12:26 15-8-2025
%create_db_predicate( Varnames_head, Pname,  Varslist3, Sx,   Pname2, "Outv_db" , "[]", cmp( Pname3 , Var_var_list_c_db ) , "match_all" ):-
%  is_match_db_pred( _,_,_,Type_match , Pname , Pred_result ) ,
%  Type_match = "match_all",  Pname2 = string::concat( Pname, "_db" ) , Pname_sub = string::concat( Pname, "_x" ) ,
%  Pred_result =  cmp( Db_predname_res , Db_pred_res_vars_list ) ,
%  vars_list_to_vars_list( Db_pred_res_vars_list , Db_pred_res_vars_list2 ) ,
%  vars_slist_to_vars_list( Varslist3 , Var_var_list ) ,
%    vars_slist_to_vars_list(  Varnames_head, Var_var_list_head ) ,
%    Var_var_listpp = list::append( Var_var_list_head , [ var( "Lis" ) , var( "Lis_res" ) ] ) ,
%    Var_var_listqq = list::append( Var_var_list_head , [ var( "Lis_res" ) , var( "Lis_res" ) ] ) ,
%  Var_var_listyy = list::append( Var_var_list_head, [ list( cmp( Db_predname_res , Db_pred_res_vars_list2 ), var("Lis") ) , var("Lis_res") ] ) ,
%  TERMX_create = cmp( ":-", [  cmp( Pname2 , Var_var_listpp ),  cmp( "," , [ cmp( "retract", [ cmp( Pname_sub , Var_var_list )
%  ] )  ,  cmp( "," , [ atom( "!" ) , cmp( Pname2 , Var_var_listyy ) ] )  ]  )   ] ),
%  Pname3 = string::concat( Pname, "_db_create" ) ,
%  vars_slist_to_vars_list( ["Outv_db"] , Var_var_list_c_db ) ,
%  vars_slist_to_free_slist( Varslist3 , Varslist3_free ) , vars_slist_to_vars_list( Varslist3_free , Var_var_list_free ) ,
%  Termx_crea_db1 =
%  cmp(":-",[cmp(Pname3,[atom("True")]),cmp(",",[cmp(",",[cmp( Pname,Var_var_list),cmp(",",[cmp("asserta",[
%   cmp( Pname_sub , Var_var_list )]),cmp(",",[atom("fail"),atom("!")])])])])]) ,
%  assert( clause_memory( is_database_construct_pred, "db create db-1 " , Pname3, Termx_crea_db1 ) ),
%   Termx_crea_db2 = cmp(":-",[cmp(Pname3,[atom("True")]),atom("!")]),
%  assert( clause_memory( is_database_construct_pred, "db create db-1-2 " , Pname3, Termx_crea_db2 ) ),
%  assert( clause_memory( is_database_construct_pred, "db create dbpred-1 " , Pname2, TERMX_create ) ),
%  Catch_clause = cmp( ":-", [ cmp( Pname2 , Var_var_listqq ), atom( "!" ) ] ) ,
%  assert( clause_memory( is_database_construct_pred , "db create dbpred catch-1 " , Pname2 , Catch_clause ) ) ,
%  !.
% 12:26 15-8-2025
%create_db_predicate( Varnames_head, Pname,  Varslist3, Sx,   Pname2 , "Outv_db" , "[]" , cmp( Pname3 , Var_var_list_c_db ) , "match_all" ):-
%  is_match_db_pred( _,_,_,Type_match , Pname , Pred_result ) ,
%  Type_match = "match_all",  Pname2 = string::concat( Pname, "_db" ) , Pname_sub = string::concat( Pname, "_x" ) ,
%	   Pred_result = var( Varname ) ,
%	     remove_dollar( Varname, Varn%ame2 ), first_char_upper( Varname2, Varname3 ) , ! ,
%  vars_slist_to_vars_list( Varslist3 , Var_var_list ) ,
%      vars_slist_to_vars_list(  Varnames_head, Var_var_list_head ) ,
%	  Var_var_listpp = list::append( Var_var_list_head , [ var( "Lis" ) , var( "Lis_res" ) ] ) ,
%      Var_var_listqq = list::append( Var_var_list_head , [ var( "Lis_res" ) , var( "Lis_res" ) ] ) ,
%      Var_var_listyy = list::append( Var_var_list_head , [ list( var( Varname3 ) , var("Lis") ) , var("Lis_res") ] ) ,
%  TERMX_create = cmp( ":-", [  cmp( Pname2 , Var_var_listpp ),  cmp( "," , [ cmp( "retract", [ cmp( Pname_sub , Var_var_list )
%  ] )  ,  cmp( "," , [ atom( "!" ) , cmp( Pname2 , Var_var_listyy ) ] )  ]  )   ] ),
%    Pname3 = string::concat( Pname, "_db_create" ) ,
%	vars_slist_to_vars_list( ["Outv_db"] , Var_var_list_c_db ) ,
%  vars_slist_to_free_slist( Varslist3 , Varslist3_free ) , vars_slist_to_vars_list( Varslist3_free , Var_var_list_free ) ,
%  Termx_crea_db1 =
%   cmp(":-",[cmp(Pname3,[atom("True")]),cmp(",",[cmp("retractall",[cmp(Pname_sub, Var_var_list_free )]),cmp(",",[cmp( Pname,Var_var_list),cmp(",",[cmp("asserta",[
%   cmp( Pname_sub , Var_var_list )]),cmp(",",[atom("fail"),atom("!")])])])])]) ,
%  assert( clause_memory( is_database_construct_pred, "db create db-2 " , Pname3 , Termx_crea_db1 ) ),
%   Termx_crea_db2 = cmp(":-",[cmp(Pname3,[atom("True")]),atom("!")]),
%  assert( clause_memory( is_database_construct_pred , "db create db-2-2 " , Pname3, Termx_crea_db2 ) ),
%  assert( clause_memory( is_database_construct_pred , "db create dbpred-2 " , Pname2 , TERMX_create ) ),
%     Catch_clause = cmp( ":-", [ cmp( Pname2 , Var_var_listqq ), atom( "!" ) ] ) ,
%   asserta( clause_memory( is_database_construct_pred , "db create dbpred catch-2-2 " , Pname2 , Catch_clause ) ) ,
%  !.
% TODO : create the copy DB retract and assert to _x  , and insert that predicate before itself in the body
% without output parametre
% todo
%create_db_predicate( Pname,  Varslist3, Sx,   Pname2 ):-  is_match_db_pred( _Type_match , Pname , Pred_result ) ,
%  Type_match = "match_all",
%  Pred_result =  var( _ ) ,  Pname2 = string::concat( Pname, "_db" ) ,
%  vars_slist_to_vars_list( Varslist3 , Var_var_list ) ,
%  Var_var_listx = list::append( Var_var_list, [ atom("True") ] ) ,
%  TERMX_create = cmp( ":-", [ cmp( Pname2 , Var_var_listx ), cmp( ",", [ cmp( Pname , Var_var_list ) ] ) ] ) ,
%  assert( clause_memory( "db create std " , TERMX_create ) ) ,
%  !.
    % Db_predname_res = "condition_if" ,
    % amount_of
    %  not check on condition if   , but check if it is a nested construct , probably with amount_of
    % TODO
    % see tag69  of how to arrange the variabels
%  Sub_pred = Db_predname_res,
    % invent the nested  vars   and add them here
%   metta_vars_list_to_prolog_vars_list( Db_pred_res_vars_list , Db_pred_res_vars_list2 ) ,
%   Subnest_name_tmp = string::concat( Subnest_name, "_tmpxq" ) ,
    %  Db_result_body =  cmp( Subnest_name_tmp , Db_pred_res_vars_list2 ) ,
%	nested_pred_memory( _ , Mlevelx ,_,_, Subpnum, Sub_pred , _ ,  Subnest_name  , Varlist_pred , Subnums2, _, Sub_preds2 ) ,
    is_a_std_predicate("is_not_equal") :-
        !.
    is_not_a_std_predicate(Pname) :-
        is_a_std_predicate(Pname),
        !,
        fail.
    is_not_a_std_predicate(_Pname) :-
        !.

    the_predicate_can_fail(Pname2) :-
        is_match_db_pred(query2, _, _, _, _, _Cou, _Predx, _Type_match, Pname2, _Pred2, _Db_result2, _Judge),
        is_not_a_std_predicate(Pname2),
        !.

%---
    append_comparison_if_it_is_a_procedure_result_pred(Pname2, Lis_body_komma01, _Resu_true_lis1, Lis_body_komma02, Lis_body_komma0) :-
        the_predicate_can_fail(Pname2),
        Lis_body_komma0 = list::append(Lis_body_komma01, Lis_body_komma02),
        !.

    append_comparison_if_it_is_a_procedure_result_pred(Pname2, Lis_body_komma01, Resu_true_lis1, Lis_body_komma02, Lis_body_komma0) :-
        Lis_01 = list::append(Lis_body_komma01, Resu_true_lis1),
        Lis_body_komma0 = list::append(Lis_01, Lis_body_komma02),
        !.

%----
    append_for_match_all(Type_match, Lis_body_komma0, Lis2, Lis_body_komma) :-
        Type_match = "match_all",
        !,
        Lis_body_komma = list::append(Lis_body_komma0, Lis2),
        !.
    append_for_match_all(_Type_match, Lis_body_komma, _Lis2, Lis_body_komma) :-
        !.

%---
    assert_catch_for_match_all(Pred_count, Type_match, Arity0, Pname_qdb, _, TERMX_catch_all_results, _Term_catch_false) :-
        Type_match = "match_all",
        !,
        asserta(clause_memory(Pred_count, is_database_construct_pred, Arity0, "db create std-qa match_all catch", Pname_qdb, TERMX_catch_all_results)).

    assert_catch_for_match_all(Pred_count, Type_match, Arity0, Pname_qdb, Predx2, _TERMX_catch_all_results, Term_catch_false) :-
        Type_match = "match",
        _Pos = string::search(Predx2, "_fail_false"),
        !,
        asserta(clause_memory(Pred_count, is_database_construct_pred, Arity0, "db create std-qa match_all catch", Pname_qdb, Term_catch_false)).

    assert_catch_for_match_all(_, _, _, _, _, _, _) :-
        !.

% 11:58 21-9-2025
    create_db_arg(Db_result, var("Outvx5"), Db_result_body, is_db_result_execute) :-
        Db_result = cmp(Db_predname_res, Db_pred_res_vars_list),
        Sub_pred = Db_predname_res,
%  amount_of_sub_nesting2(  Db_pred_res_vars_list  , 0 , Res ) , Res > 0 ,
        clause_memory(_, _, _, _, Db_predname_res, _),
        % it is declared earlier
        % predicate_nesting_ordered(5,2,1,1,2,"trace_remove_apply_cell",["$lev","$r","$c"],[1,2,3],0,0,3).
        predicate_nesting_ordered(_, _, _, _, _, Db_predname_res, Varlist_pred, _, _, _, _),
% 13:02 21-9-2025
        % IF the predicate is declared as =  predicate it should be executed
        Subnest_name = Db_predname_res,
        % nested_pred_memory( _ , Mlevelx ,_,_, Subpnum, Sub_pred , _ ,  Subnest_name  , Varlist_pred , _Subnums2, _, _Sub_preds2 ) ,
        Cou2 = 5,
        !,
%    max_deep_variabels_nest_connected( Mlevelx, [ Subpnum ] , [ Sub_pred ], [] , Vars_list_deep , 50 ) ,
%	 Varslist3_px0 = list::append(  Varlist_pred , Vars_list_deep  ) ,
        Varslist3_px0 = Varlist_pred,
        outvars_at_the_end(Varslist3_px0, Varslist3_p),
        current_transpile_model_get(_, _, _, Subclauses_body_call_type, _),
        remove_real_int_vals(Subclauses_body_call_type, Varslist3_p, Varslist3, _),
        vars_slist_remove_pred_out_vars(Subclauses_body_call_type, "", Varslist3, Varslist_z02, _),
        set_outvar_in_list(Cou2, Varslist_z02, Varslist_z02x, Sx),
        untag_pred_out_vars(Varslist_z02x, Varslist_z03),
        Varslist_z = list::removeDuplicates(Varslist_z03),
        vars_slist_to_vars_list(Varslist_z, Var_var_list),
        Db_result_body = cmp(Subnest_name, Var_var_list),
        !.

    create_db_arg(Db_result, var("Outvx3"), Db_result_body, is_db_result_execute) :-
        Db_result = cmp(Db_predname_res, Db_pred_res_vars_list),
        Sub_pred = Db_predname_res,
% last to solve add atom
%   Db_predname_res <> "add_atom" ,
        amount_of_sub_nesting2(Db_pred_res_vars_list, 0, Res),
        Res > 0,
        nested_pred_memory(_, Mlevelx, _, _, Subpnum, Sub_pred, _, Subnest_name, Varlist_pred, _Subnums2, _, _Sub_preds2),
% 13:27 17-8-2025
        Cou2 = 3,
        !,
        max_deep_variabels_nest_connected(Mlevelx, [Subpnum], [Sub_pred], [], Vars_list_deep, 50),
        Varslist3_px0 = list::append(Varlist_pred, Vars_list_deep),
        outvars_at_the_end(Varslist3_px0, Varslist3_p),
        current_transpile_model_get(_, _, _, Subclauses_body_call_type, _),
        remove_real_int_vals(Subclauses_body_call_type, Varslist3_p, Varslist3, _),
        vars_slist_remove_pred_out_vars(Subclauses_body_call_type, "", Varslist3, Varslist_z02, _),
        set_outvar_in_list(Cou2, Varslist_z02, Varslist_z02x, Sx),
        untag_pred_out_vars(Varslist_z02x, Varslist_z03),
        Varslist_z = list::removeDuplicates(Varslist_z03),
        vars_slist_to_vars_list(Varslist_z, Var_var_list),
        Db_result_body = cmp(Subnest_name, Var_var_list),
        !.

% TODo TODO TODO see tag69 to create the clause line
%  Db_arg_head = cmp( Db_predname_res , Db_pred_res_vars_list2 ), ! .
    create_db_arg(Db_result, Db_arg_head, cmp("dum_db_pred1", []), is_db_result_term) :-
        Db_result = cmp(Db_predname_res, Db_pred_res_vars_list),
        metta_vars_list_to_prolog_vars_list(Db_pred_res_vars_list, Db_pred_res_vars_list2),
        Db_arg_head = cmp(Db_predname_res, Db_pred_res_vars_list2),
        !.

    create_db_arg(Db_result, Db_arg_head, cmp("dum_db_pred2", []), is_db_result_variabel) :-
        Db_result = var(Vname),
        % dollarx
        remove_dollar(Vname, Vname2),
        first_char_upper(Vname2, Vname3),
        !,
        Db_arg_head = var(Vname3),
        !.

%--
    add_non_present_variabels(Varslist3_00, Varnames_head0, Varslist3) :-
        % "$
        vars_slist_to_vars_slist(Varslist3_00, Varslist3_0),
        vars_slist_to_vars_slist(Varnames_head0, Varnames_head),
        Diff = list::difference(Varnames_head, Varslist3_0),
        Varslist3 = list::append(Varslist3_0, Diff),
        !.
        %Elem* List1,
        %Elem* List2)

    add_non_present_variabels(Varslist3_0, _Varnames_head, Varslist3_0) :-
        !.

%  Var_var_listpp = list::append( Var_var_list, [  ] ) ,
%------
    %Db_result =  cmp( Db_predname_res , Db_pred_res_vars_list ) ,
    %metta_vars_list_to_prolog_vars_list( Db_pred_res_vars_list , Db_pred_res_vars_list2 ) ,
%  Var_var_listpp = list::append( Var_var_list, [ cmp( Db_predname_res , Db_pred_res_vars_list2 ) ] ) ,
%  TERMX_create = cmp( ":-", [  cmp( Pname2 , Var_var_listpp ),  cmp( ",", [ cmp( Pname , Var_var_list ) , atom( "!" ) ] ) 	]  ),
    % division
    % create_db_arg( Db_result2 , _Db_arg2 ) ,
    % Var_var_listzz = list::append( Var_var_list_2, [ Db_arg2 ] ) ,
    % vars_slist_to_vars_list( Varslist3_2 , Var_var_list_2 ) ,
    % division
%   Resu_true = cmp("=",[var("Q"),cmp("/",[var("X"),var("W")])])
    % Asser_result = cmp(":-",[cmp(",",[cmp("assert",[cmp("prog_result",[var("Var1")])]),atom("!")])]),
    %cmp(":-",[cmp(",",[cmp("assert",[cmp("prog_result",[var("Var1")])]),cmp(",",[atom("fail"),atom("!")])])])
%	 Asser_result1 = cmp("assert",[cmp("prog_result",[var("Var1")])]) ,
%   Lis_body_komma = list::append( Lis_body_komma0 , [ Asser_result1 , Asser_result2 ] ),
% cmp(":-",[cmp("cell_candidate_db",[var("R"),var("C"),var("Num"),var("Outvx3")]),cmp(",",
    % [cmp("get_all_db_result",[var("Outvx3")]),atom("!")]  )])
% asserta( clause_memory( is_database_construct_pred , "db create std match_all catch" , Pname_qdb , TERMX_catch_all_results ) ),
% Only allow it to be created 1 time, 2 times will happen when the same db predicate occurs twice in the body
% this occurs with example 22
% should not be done like this
    create_db_predicate(Pred_count, _Varnames_head, Pname1, _Varslist3, _VL, _Outvar_create, Pname_qdb, "Outv_not_db", "notx",
            cmp("db_dummy", [var("Outv_not_db")]), cmp("db_du2mmy", [var("Outv_not_2db")]), _Type_match) :-
        % vars_slist_to_vars_list( _VL , VL2 ) ,
        Pname_qdb = string::concat(Pname1, "_db", toString(Pred_count)),
% 14:45 23-8-2025
        clause_memory(Pred_count, _, _, _, Pname_qdb, _TERMX_create),
        !,
        fail.

% TODO more neat  plus   db_result in the clause
    create_db_predicate(Pred_count, _Varnames_head, Pname1, Varslist3_0, _VL0, _Outvar_create, Pname_qdb, "Outv_not_db", "notx",
            cmp("db_dummy", [var("Outv_not_db")]), Clause_line, Type_match) :-
% &self is inside Varslist3_0
        is_match_db_pred(query1, Dbarea, _, _, _, _Cou1, _Predx1, Type_match, Pname1, _, Db_result1, _Judge),
        is_match_db_pred(query2, _Dbar, _, _, _, _Cou2, Predx2, Type_match, Pname2, Pred2, _Db_result2, _),
        Pred2 = cmp(_, Varslist3_2_x0),
        remove_pred_out_vars_varlist(Varslist3_2_x0, Varslist3_2, _),
        find_pred_out_var_or_assign(Varslist3_2_x0, Outvar_xx),
        Pname_qdb = string::concat(Pname1, "_db", toString(Pred_count)),
        Lex = list::length(_VL0),
        Lex2 = Lex - 1,
        FirstPart = list::take(Lex2, _VL0),
        LastPart = list::drop(Lex2, _VL0),
        add_non_present_variabels(FirstPart, _Varnames_head, FirstPart2),
%		stdio::write( "aa1",toString(FirstPart), toString(_Varnames_head), toString(FirstPart2) , "\n" ),
        VLxx = list::append(FirstPart2, LastPart),
        % add_non_present_variabels( _VL0, _Varnames_head, _VL ),
        vars_slist_to_vars_list(VLxx, VL2),
        Clause_line = cmp(Pname_qdb, VL2),
% For example -18  we must add variabel level  from _Varnames_head     at the end  for/in clause_line
% and also in :
        metta_vars_list_to_prolog_vars_list(Varslist3_2, Varslist3_2p),
        % Varslist3_2pz = list::append( Varslist3_2p, [var("Outvar_db2")] ),
        Varslist3_2pz = list::append(Varslist3_2p, [Outvar_xx]),
        (Type_match = "match" or Type_match = "match_all"),
        create_db_arg(Db_result1, Db_arg1_head, Db_pred1_body, _Result_db_type),
        add_non_present_variabels(Varslist3_0, _Varnames_head, Varslist3),
%     stdio::write( "aa1p2", toString(Varslist3_0), toString(_Varnames_head), toString(Varslist3) , "\n" ),
        vars_slist_to_vars_list(Varslist3, Var_var_list),
        vars_slist_to_vars_list(Varslist3_0, Var_var_list_0),
        Var_var_listpp = list::append(Var_var_list, [Db_arg1_head]),
        Var_var_listqq = list::append(Var_var_list, [var("Outv_db_result")]),
        arity_of_head_vars(Var_var_listqq, ArityX),
        Resu_true = cmp("=", [Outvar_xx, atom("True")]),
        Asser_result1 = cmp("asserta", [cmp("db_result", [atom(Dbarea), Db_arg1_head])]),
        Asser_result2 = atom("fail"),
        % 11:42 30-8-2025
        Lis_body_komma01 = [cmp(_Predx1, Var_var_list_0), cmp(Pname2, Varslist3_2pz)],
        % Lis_body_komma01 = [ cmp( Pname1 , Var_var_list_0 ) ,  cmp( Pname2 , Varslist3_2pz )  ] ,
        % cmp( Pname1 , Var_var_list_0 ) ,  cmp( Pname2 , Varslist3_2pz ) , Resu_true,
        Lis_body_komma02 = [Db_pred1_body],
        append_comparison_if_it_is_a_procedure_result_pred(Pname2, Lis_body_komma01, [Resu_true], Lis_body_komma02, Lis_body_komma0),
        append_for_match_all(Type_match, Lis_body_komma0, [Asser_result1, Asser_result2], Lis_body_komma),
        TERMX_create = cmp(":-", [cmp(Pname_qdb, Var_var_listpp), cmp(",", Lis_body_komma)]),
        assert(clause_memory(Pred_count, is_database_construct_pred, ArityX, "db create std-p ", Pname_qdb, TERMX_create)),
        TERMX_catch_all_results =
            cmp(":-", [cmp(Pname_qdb, Var_var_listqq), cmp(",", [cmp("get_all_db_result", [atom(Dbarea), nill, var("Outv_db_result")]), atom("!")])]),
        Term_catch_false = cmp(":-", [cmp(Pname_qdb, Var_var_listqq), cmp(",", [cmp("=", [var("Outv_db_result"), str("False")]), atom("!")])]),
        assert_catch_for_match_all(Pred_count, Type_match, ArityX, Pname_qdb, Predx2, TERMX_catch_all_results, Term_catch_false),
        !.

%  vars_slist_to_vars_list( _VL , VL2 ) ,     Clause_line = cmp( Pname_qdb , VL2 ),
%  is_match_db_pred( query2, _ , _ , _ , Type_match , Pname2 , Pred2 , _Db_result2 ) ,   Pred2 = cmp( _ , Varslist3_2 ) ,
%  metta_vars_list_to_prolog_vars_list( Varslist3_2 , Varslist3_2p ) ,
%  Varslist3_2pz = list::append( Varslist3_2p, [var("Outvar_db2")] ),
    create_db_predicate(Pred_count, _Varnames_head, Pname1, Varslist3_0, _VL0, _Outvar_create, Pname_qdb, "Outv_not_db", "notx",
            cmp("db_dummy", [var("Outv_not_db")]), Clause_line, Type_match) :-
        is_match_db_pred(query1, Dbarea, _, _, _, _Cou1, _Predx1, Type_match, Pname1, _, Db_result1, _Judge),
%    vars_slist_to_vars_list( _VL , VL2 ) ,
        Pname_qdb = string::concat(Pname1, "_db", toString(Pred_count)),
        Lex = list::length(_VL0),
        Lex2 = Lex - 1,
        FirstPart = list::take(Lex2, _VL0),
        LastPart = list::drop(Lex2, _VL0),
        add_non_present_variabels(FirstPart, _Varnames_head, FirstPart2),
%		  stdio::write( "bb1p2", toString(FirstPart), toString(_Varnames_head), toString(FirstPart2) , "\n" ),
        VLxx = list::append(FirstPart2, LastPart),
        % add_non_present_variabels( _VL0, _Varnames_head, _VL ),
        vars_slist_to_vars_list(VLxx, VL2),
        Clause_line = cmp(Pname_qdb, VL2),
        (Type_match = "match" or Type_match = "match_all"),
        create_db_arg(Db_result1, Db_arg1_head, Db_pred1_body, _Result_db_type),
        add_non_present_variabels(Varslist3_0, _Varnames_head, Varslist3),
%    stdio::write( "bbc1q2", toString(Varslist3_0), toString(_Varnames_head), toString(Varslist3) , "\n" ),
        vars_slist_to_vars_list(Varslist3, Var_var_list),
        Var_var_listpp = list::append(Var_var_list, [Db_arg1_head]),
        Var_var_listqq = list::append(Var_var_list, [var("Outv_db_result")]),
        arity_of_head_vars(Var_var_listqq, ArityX),
        % Resu_true = cmp("=",[var("Outvar_db2"), atom("True")]) ,
        Asser_result1 = cmp("asserta", [cmp("db_result", [atom(Dbarea), Db_arg1_head])]),
        Asser_result2 = atom("fail"),
%	 Lis_body_komma0 = [ cmp( Pname1 , Var_var_list ) ,  cmp( Pname2 , Varslist3_2pz ) , Resu_true, Db_pred1_body ] ,
% 11:38 30-8-2025
        Lis_body_komma0 = [cmp(_Predx1, Var_var_list), Db_pred1_body],
        %	 Lis_body_komma0 = [ cmp( Pname1 , Var_var_list ) ,   Db_pred1_body ] ,
        append_for_match_all(Type_match, Lis_body_komma0, [Asser_result1, Asser_result2], Lis_body_komma),
        TERMX_create = cmp(":-", [cmp(Pname_qdb, Var_var_listpp), cmp(",", Lis_body_komma)]),
        assert(clause_memory(Pred_count, is_database_construct_pred, ArityX, "db create std-z ", Pname_qdb, TERMX_create)),
        TERMX_catch_all_results =
            cmp(":-", [cmp(Pname_qdb, Var_var_listqq), cmp(",", [cmp("get_all_db_result", [atom(Dbarea), nill, var("Outv_db_result")]), atom("!")])]),
        Term_catch_false = cmp(":-", [cmp(Pname_qdb, Var_var_listqq), cmp(",", [cmp("=", [var("Outv_db_result"), str("False")]), atom("!")])]),
        assert_catch_for_match_all(Pred_count, Type_match, ArityX, Pname_qdb, "", TERMX_catch_all_results, Term_catch_false),
        !.

% was not ok , we make a copy of the first and remove query2
% LAST  was not good
%create_db_predicate( _Varnames_head, Pname,  Varslist3, _VL , Sx,   Pname_qdb , "Outv_not_db" , "notx", cmp( "db_dummy" ,[var("Outv_not_db")] ) , Type_match ):-
%  is_match_db_pred( query1, _ , _ , _ , Type_match , Pname , _ , Db_result ) ,  ( Type_match = "match" or Type_match = "match_all" ) ,
%  create_db_arg( Db_result , Db_arg_head , Db_pred1_body ) ,
%  Pname_qdb = string::concat( Pname, "_db" ) ,  vars_slist_to_vars_list( Varslist3 , Var_var_list ) ,
%  Var_var_listpp = list::append( Var_var_list, [ Db_arg_head ] ) ,
%  TERMX_create = cmp( ":-", [  cmp( Pname_qdb , Var_var_listpp ),  cmp( ",", [ cmp( Pname , Var_var_list ) , Db_pred1_body ] ) 	]  ),
%  assert( clause_memory( is_database_construct_pred , "db create std " , Pname_qdb , TERMX_create ) ),  !.
%create_db_predicate( _Varnames_head, Pname,  Varslist3, Sx,   Pname2, "Outv_not_db" , "notx", cmp( "db_dummy" ,[var("Outv_not_db")] ) , Type_match ):-
%  is_match_db_pred( _ , _ , _ , _Type_match , Pname , _, Db_result ) ,  ( Type_match = "match" or Type_match = "match_all" ) ,
%  Db_result =  var( _ ) ,
%  Pname2 = string::concat( Pname, "_db" ) ,
%  vars_slist_to_vars_list( Varslist3 , Var_var_list ) ,
%  Var_var_listx = list::append( Var_var_list, [ atom("True") ] ) ,
%  TERMX_create = cmp( ":-", [ cmp( Pname2 , Var_var_listx ), cmp( ",", [ cmp( Pname , Var_var_list )  ] ) ] ) ,
%  assert( clause_memory( is_database_construct_pred , "db create std " , Pname2 , TERMX_create ) ) ,  !.
%  TERMX_create = cmp( ":-", [ cmp( Pname2 , Var_var_listx ), cmp( ",", [ cmp( Pname , Var_var_list ) , atom( "!" ) ] ) ] ) ,
% create_db_predicate( Subnest_name,  _, _,   Subnest_name ):-!.
%----
%change_db_pred_name( Pname, Pname2 ):-  is_match_db_pred( Pname ) , Pname2 = string::concat( Pname, "_db" ) ,  !.
%change_db_pred_name( Pname, Pname ):-!.
%----
%try_get_first_predname( [ cmp( Pname , Lis ) | _ ], Pname , Lis_vars ):- get_vars( Lis, [], Lis_vars ), ! .
%try_get_first_predname( _Atom_List2, "no_pred" , [] ):- !.
    try_get_first_predname([cmp(Pname, Lis) | _], Pname, Lis_vars, Lis_positions, Lis) :-
        get_vars(Lis, 1, Lis_vars, Lis_positions, Lis_nonvars, _Lis_nums_nonvars),
        !.
    try_get_first_predname(_Atom_List2, "no_pred", [], [], []) :-
        !.

%enhance_main_level():- retract( main_level( Nx ) ), Nx2 = Nx + 1, assert( main_level( Nx2 ) ), !.
%enhance_main_level():- !.
%get_main_level( Mlevel ):- main_level( Mlevel ) , ! .
%get_main_level( 0 ):- !.
%----
% current_transpile_model( Sequential_var_names , Subclauses_head_type , Subclauses_body_call_type ) ,
% keep_all, preserve_metta_vars, remove_metta_outpvar_and_created
% try this for let
% keep_all   keep_all
% LAST ------
% current_transpile_model( preserve_metta_vars , keep_all , keep_all  ):- !.
% current_transpile_model( preserve_metta_vars , 0 , keep_all  ):- !.
% this seems to work good for example 6
% last model default
% keep_all
% This One for LET
% current_transpile_model( preserve_metta_vars , 0 , keep_all  ):- !.
%current_transpile_model( 0, preserve_metta_vars , preserve_metta_output_var , keep_all  ):- !.
%current_transpile_model( 0, preserve_metta_vars , 0 , 0  ):- !.
%-------****
%asser_pred_nest( Pname, _,_,_,_ ):-	  predicate_nesting( Pname, _,_,_,_ ) , !.
%asser_pred_nest( Level0, Level, Level2 , Pname, _ , _Nestlevel ):-
%	  predicate_nesting(  Level0, Level, Level2 , Pname, _ , _ ) , !.
%asser_pred_nest( Mlevel, Level0, Level, Level2 , Pname, Varslist , Nestlevel ):-
%	 assert( predicate_nesting( Mlevel,  Level0, Level, Level2 , Pname, Varslist, Nestlevel ) ), !.
%predicate_nesting_below_primlevel3_level( Prim_level, Level, Lower_level ):-
%   predicate_nesting_ordered( Prim_level2 , _ , _ , Level , Lower_level , _Operat2 , _ , _ ) , Prim_level2 > Prim_level .
%----
%predicate_nesting_below_primlevel2_name( Prim_level, Level, Operat2, Vars ):-
% predicate_nesting_ordered( Prim_level2 , _ , _ , Level , _ , Operat2 , Vars , _ ),  Prim_level2 > Prim_level .
% TODO question if nest level decreent works
% ( i , i,i,i,o, i, o , i , o ).
    sequential_pred_names_and_vars(Max_nest, Mlevel, Level_1, Oplist, Operat2_L, Varslist, Varslist_result, Level_list, Level_list_res) :-
        Mlevel_seq = Mlevel + 1,
% !!!
        Max_nest > 0,
        % todo, we should incorporate  extra level numbers because it does not parse correct
        % maybe like this:  Pred_level  is always Level_1 + 1 ? NO
        % 23-4-2025    Pred_level
        Pred_Level_1 = Level_1 + 1,
        % Mlevel_seq is max  1 higher ? to solve the problem
        retract(predicate_nesting_ordered_tmp(_, Mlevel_seq, Level_1, Level_1, Pred_level, Operat, Vars, _Vpos, _Nestlevel, _Lin_nest, _Ariti)),
        !,
% deep
        % TODO question to retract or not
        %  predicate_nesting_ordered_tmp( _ , Mlevel_seq , Level_1, Level_1, _ ,Operat, Vars , _Nestlevel ) , !,
        Max_nest2 = Max_nest - 1,
        Varslist2 = list::append(Varslist, Vars),
        sequential_pred_names_and_vars(Max_nest2, Mlevel_seq, Level_1, [Operat | Oplist], Operat2_L, Varslist2, Varslist_result,
            [Pred_level | Level_list], Level_list_res).

    sequential_pred_names_and_vars(_Max_nest, _Mlevel, _Level_1, Oplist, Oplist2, Varslist0, Varslist, Level_list_res, Level_list_res2) :-
        Oplist2 = list::reverse(Oplist),
        Level_list_res2 = list::reverse(Level_list_res),
        current_transpile_model_get(_, Sequential_var_names, _, _, _),
        % preserve_metta_vars
        vars_slist_remove_pred_out_vars(Sequential_var_names, "", Varslist0, Varslistq, _),
        untag_pred_out_vars(Varslistq, Varslist),
%     Varslist = Varslist0,
        !.
%----

%sequential_variabels_for_predicate( Mlevel, Level_1, Varlist , Varslist3 ):- Mlevel_seq = Mlevel + 1 ,
%  predicate_nesting_ordered( _ , Mlevel_seq , Level_1, Level_1, _ , _Operat, Vars , _Nestlevel ), !,
%  Varslist2 = list::append( Vars , Varlist ),
%  sequential_variabels_for_predicate( Mlevel_seq, Level_1, Varslist2 , Varslist3 ).
% sequential_variabels_for_predicate( _Mlevel, _Level_1, Varslist2 , Varslist2 ):- !.
%asser_pred_nest( Pname, Level0, Level, Level2 , Varslist ),
%amount_of_sub_nesting2( [] , Res, Res ):- ! .
%amount_of_sub_nesting2( [ cmp( Operat,   Ato_lis  ) | Lis ] , Cou,  Res ):-     Operat = "sub" ,  ! , Cou2 = Cou + 1 ,
%    amount_of_sub_nesting2(  Ato_lis  , Cou2, Cou3 ),    amount_of_sub_nesting2(  Lis  , Cou3, Res ),    ! .
%amount_of_sub_nesting2( [ cmp( _Operat,   Ato_lis  ) | Lis ] , Cou,  Res ):-      ! , Cou2 = Cou + 0 ,
%    amount_of_sub_nesting2(  Ato_lis  , Cou2, Cou3 ),    amount_of_sub_nesting2(  Lis  , Cou3, Res ),    ! .
%amount_of_sub_nesting2( [ _H | Lis ] , Cou, Res ):- ! ,   amount_of_sub_nesting2( Lis  , Cou, Res ) , ! .
% inventarise_subcl_metta_tpro_subc2( _,  []  ) :- !.
%                       transpiled  argument                                   transpiled( Count, Sx )
% transpile_subcl_metta_tpro_subc2(   [ cmp( Operat,  Atom_List2 )  | Lis ] , [ cmp( "sub_trp", Atom_List2)  | Result_list ] ):-  Operat = "sub" ,
% inventarise_subcl_metta_tpro_subc2( Level,  [ cmp( Operat,  Atom_List2 )  | Lis ]  ):-  Operat = "sub" ,
%     amount_of_sub_nesting2(  Atom_List2  , 0 , Res ) ,	 Res = 0 , ! ,
%	 try_get_first_predname( Atom_List2, Pname ),	  assert( predicate_nesting( Level, Level, Pname ) ),
%	 	 inventarise_subcl_metta_tpro_subc2( Level, Lis  ).
% example :::  it is aserted like this
%predicate_nesting("get_cell_state",     0,0,1,["$column","$row"]).
%predicate_nesting("erer",               1,0,1,["$er"]).
%predicate_nesting("nesmathc",           2,0,1,["$jh"]).
%predicate_nesting("match",              2,1,2,["$state"]).
%predicate_nesting("jepred",             2,2,3,["$creed","$weed"]).
%predicate_nesting("lapred",             3,2,3,["$ui","$weed"]).
%predicate_nesting("extra",              4,2,3,["$fd"]).
%predicate_nesting("sudoku_puzzle_state",4,3,4,["$state","$quad","$column","$row"]).
% vars_slist_clean_pred_out_vars_names( [] , [] ):- !.
% vars_slist_clean_pred_out_vars_names( [ H | Vlx0 ] , [Svar | Vlx ]  ):-
%  parse_output_var( H , Svar ) , ! , vars_slist_clean_pred_out_vars_names(  Vlx0  , Vlx ).
% vars_slist_clean_pred_out_vars_names( [ H | Vlx0 ] , [ H | Vlx ]  ):-   ! , vars_slist_clean_pred_out_vars_names(  Vlx0  , Vlx  ).
%---
    untag_pred_out_vars([], []) :-
        !.
    untag_pred_out_vars([El | Direct_vars_str0], [Outvar_str | Direct_vars_str]) :-
        parse_output_var(El, Outvar_str),
        !,
        untag_pred_out_vars(Direct_vars_str0, Direct_vars_str).
    untag_pred_out_vars([El | Direct_vars_str0], [El | Direct_vars_str]) :-
        !,
        untag_pred_out_vars(Direct_vars_str0, Direct_vars_str).

% _names
%----
% TODO  implement remove-types
    remove_real_int_vals(_Rmtype, [], [], []) :-
        !.
    remove_real_int_vals(Rmtype, [H | Varlist_pred_z], Varlist_pred, [H | Xlis]) :-
        string::tryFront(H, 8, Bg, _),
        Bg = "realvar#",
        !,
        remove_real_int_vals(Rmtype, Varlist_pred_z, Varlist_pred, Xlis).

    remove_real_int_vals(Rmtype, [H | Varlist_pred_z], Varlist_pred, [H | Xlis]) :-
        string::tryFront(H, 8, Bg, _),
        Bg = "atomvar#",
        !,
        remove_real_int_vals(Rmtype, Varlist_pred_z, Varlist_pred, Xlis).

    remove_real_int_vals(Rmtype, [H | Varlist_pred_z], [H | Varlist_pred], Xlis) :-
        remove_real_int_vals(Rmtype, Varlist_pred_z, Varlist_pred, Xlis).

%---
% untag_pred_out_vars( Direct_vars_str0 , Direct_vars_str ) ,
% 09:49 25-6-2025   UNTAG AFTER !!!
    vars_slist_remove_pred_out_vars(_, _, [], [], []) :-
        !.

    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], [H | Vlx], Rest) :-
        Filter_type = keep_all,
% TEMP TEST
% vars_slist_remove_pred_out_vars( Filter_type , [ H | Vlx0 ] , [ H | Vlx ], Rest  ):-  Filter_type = keep_all,
        parse_output_var(H, _Svar),
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], [H | Vlx], Rest) :-
        Filter_type = preserve_metta_vars,
        parse_output_var(H, _Svar),
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], Vlx, [H | Rest]) :-
        parse_output_var(H, _Svar),
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

% 10:14 5-6-2025 last added here
% current_transpile_model_get(
% dont_allow_output_vars
    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], Vlx, [H | Rest]) :-
        Filter_type = dont_allow_output_vars,
        !,
        % parse_output_var( H , _Svar ) , ! ,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

% 12:37 20-5-2025 IF you remove this it almst works for examp6
    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], [H | Vlx], Rest) :-
        Filter_type = keep_all,
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

% temp
% 14:06 12-9-2025
%vars_slist_remove_pred_out_vars( Filter_type , Predname , [ H | Vlx0 ] , [ H | Vlx ], Rest  ):-
%   string::tryFront( Predname, 9, Bg , _Rs ), Bg = "add_atom_" ,
%  parse_output_var( H , _Svar ) , ! ,
% vars_slist_remove_pred_out_vars( Filter_type, Predname , Vlx0  , Vlx,  Rest  ).
% 14:06 12-9-2025
% Filter_type = preserve_metta_output_var,
% temp
    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], [H | Vlx], Rest) :-
        string::tryFront(Predname, 9, Bg, _Rs),
        Bg = "add_atom_",
        is_metta_output_var(H),
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], [H | Vlx], Rest) :-
        Filter_type = preserve_metta_output_var,
        is_metta_output_var(H),
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], Vlx, [H | Rest]) :-
        is_metta_output_var(H),
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

    vars_slist_remove_pred_out_vars(Filter_type, Predname, [H | Vlx0], [H | Vlx], Rest) :-
        !,
        vars_slist_remove_pred_out_vars(Filter_type, Predname, Vlx0, Vlx, Rest).

%--------------
    vars_slist_to_free_slist([], []) :-
        !.
    vars_slist_to_free_slist([H | Vars_list], ["_" | Var_var_list]) :-
        vars_slist_to_free_slist(Vars_list, Var_var_list).

%---
    vars_slist_to_vars_list([], []) :-
        !.
    vars_slist_to_vars_list([H | Vars_list], [nill | Var_var_list]) :-
        H = "nill_list",
        !,
        vars_slist_to_vars_list(Vars_list, Var_var_list).
    vars_slist_to_vars_list([H | Vars_list], [var(H3) | Var_var_list]) :-
        remove_dollar(H, H2),
        first_char_upper(H2, H3),
        !,
        vars_slist_to_vars_list(Vars_list, Var_var_list).
%---

    vars_slist_to_vars_slist([], []) :-
        !.
    vars_slist_to_vars_slist([H | Vars_list], [H3 | Var_var_list]) :-
        remove_dollar(H, H2),
        first_char_upper(H2, H3),
        !,
        vars_slist_to_vars_slist(Vars_list, Var_var_list).

%---
    vars_list_to_varnames_slist([], [], []) :-
        !.

    vars_list_to_varnames_slist([var(H) | Vars_list], [H | Var_var_list], Notlist) :-
        !,
        vars_list_to_varnames_slist(Vars_list, Var_var_list, Notlist).

    vars_list_to_varnames_slist([_ | Vars_list], Var_var_list, ["not_item" | Notlist]) :-
        !,
        vars_list_to_varnames_slist(Vars_list, Var_var_list, Notlist).

    find_pred_out_var_or_assign(Direct_vars_str0, Outvar_str) :-
        find_pred_out_var(Direct_vars_str0, Outvar_str),
        !.
    find_pred_out_var_or_assign(Direct_vars_str0, var("Outvar_db2")) :-
        !.

% "$"
    find_pred_out_var([var(El) | _Direct_vars_str0], var(Outvar_str3)) :-
        parse_output_var(El, Outvar_str),
        clean_var(Outvar_str, Outvar_str3),
        !.
    find_pred_out_var([var(El) | Direct_vars_str0], Outvar_str) :-
        !,
        find_pred_out_var(Direct_vars_str0, Outvar_str).

%---
%--- ICI todo
    remove_pred_out_vars_varlist([], [], []) :-
        !.
    remove_pred_out_vars_varlist([var(El) | Direct_vars_str0], Lis2, [var(Outvar_str) | Direct_vars_str]) :-
        parse_output_var(El, Outvar_str),
        !,
        remove_pred_out_vars_varlist(Direct_vars_str0, Lis2, Direct_vars_str).

    remove_pred_out_vars_varlist([El | Direct_vars_str0], [El | Lis2], [El | Direct_vars_str]) :-
        !,
        remove_pred_out_vars_varlist(Direct_vars_str0, Lis2, Direct_vars_str).

%----
    metta_vars_list_to_prolog_vars_list([], []) :-
        !.

    metta_vars_list_to_prolog_vars_list([var(H) | Vars_list], [var(H3) | Var_var_list]) :-
        remove_dollar(H, H2),
        first_char_upper(H2, H3),
        !,
        metta_vars_list_to_prolog_vars_list(Vars_list, Var_var_list).

    metta_vars_list_to_prolog_vars_list([Terx | Vars_list], [Terx | Var_var_list]) :-
        !,
        metta_vars_list_to_prolog_vars_list(Vars_list, Var_var_list).

%---
    % Out = string::concat( Hs , "\n" , Space , toString( cmp( Funcname, Lisx ) ) , ".\n" )  , ! .
%cmp(":-",[cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),var("Lis"),var("Lis_res")]),
% cmp(",",[
%   cmp("retract",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")])]),
%  cmp(",",[atom("!"),
%   cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),
%    list(cmp("cell_candidatex",[var("B"),var("Num")]),var("Lis")),
%	 var("Lis_res")])
%	 ])])])
/*  false version
bodylist_to_string_format( _Cou , Hs , [] , Hs ):- ! .

% amount_of
bodylist_to_string_format( Cou , Hs, [ H ] , Out ):-   H = cmp( Funcname, Lisx ) ,    Space = string::create( Cou , " " ) ,
 amount_of_sub_nesting2(  Lisx  , 0, Res ), Res = 0 ,
 var_list_to_prolog_vars_string( Lisx , "" , Vars_Str ) , ! ,
 Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , " .\n" ] ) , ! .

bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = list( Headlis, Restlis ) ,   Space = string::create( Cou , " " ) ,
   bodylist_to_string_format( Cou ,  "", [ Headlis ] , ResultHead ) ,
   bodylist_to_string_format( Cou ,  "", [ Restlis ] , ResultRest ) ,
   Res2 = string::concat( Hs, "\n [-" , ResultHead, "-|  ", ResultRest, "]" ) ,  !,
  Cou2 = Cou + 0 ,
  bodylist_to_string_format( Cou2 ,  Res2, Body_list , Result ).


bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = cmp( Funcname, Lisx ) ,
   amount_of_sub_nesting2(  Lisx  , 0, Res ), Res = 0 ,     ! , Space = string::create( Cou , " " ) ,
  var_list_to_prolog_vars_string( Lisx , "" , Vars_Str ) ,
  Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , " ,\n" ] ),
  Cou2 = Cou + 0 ,
  bodylist_to_string_format( Cou2 ,  Out, Body_list , Result ).

% 13:10 6-7-2025
bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = cmp( Funcname, Lisx ) ,
       bodylist_to_string_format( Cou , Hs, Lisx , Hsres ) ,
	   Space = string::create( Cou , " " ) ,
  Out = string::concatList( [  Hs , Space , Funcname , "( " , Hsres , " )"  , " ,\n" ] ),
  Cou2 = Cou + 0 ,
  bodylist_to_string_format( Cou2 ,  Out, Body_list , Result ).

bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = var( Vn ) ,   !,

	   Space = string::create( Cou , " " ) ,
  Out = string::concatList( [  Hs , Vn,  " , " ] ),
  Cou2 = Cou + 0 ,
  bodylist_to_string_format( Cou2 ,  Out, Body_list , Result ).
*/
%---
    %cmp(":-",[ cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),var("Lis"),var("Lis_res")]),
    % cmp(",",[
%  cmp("retract",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")])]),
%  cmp(",",[ atom("!"),
%   cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),
%    list( cmp("cell_candidatex",[var("B"),var("Num")]), var("Lis")),
%	  var("Lis_res")])])])])
    comma_or_dot(Body_list, ".") :-
        Le = list::length(Body_list),
        Le = 0,
        !.
    comma_or_dot(_Body_list, ",") :-
        !.

% 1
%bodylist_to_string_format( Cou , Hs, [ H ] , Out ):-   H = cmp( Funcname, Lisx ) , Funcname = "," ,    Space = string::create( Cou , " " ) ,
% Lisx = [ Headlis | Resli ] , Headlis = atom( Ava ), Ava = "!" ,
% var_list_to_prolog_vars_string( Resli , "" , Vars_Str ) , ! ,
% Out = string::concatList( [  Hs , Space , " ! , \n " , " " , Vars_Str , " "  , " .\n" ] ) , ! .
% zz
%bodylist_to_string_format( Cou , Hs, [ H ] , Out ):-   H = cmp( Funcname, Lisx ) ,    Space = string::create( Cou , " " ) ,
% var_list_to_prolog_vars_string( Lisx , "" , Vars_Str ) , ! ,
% Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , " .\n" ] ) , ! .
%bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = list( Headlis, Restlis ) ,   Space = string::create( Cou , " " ) ,
%   bodylist_to_string_format( Cou ,  "", [Headlis] , ResultHead ) ,
%   bodylist_to_string_format( Cou ,  "", [Restlis] , ResultRest ) ,
%   Res2 = string::concat( Hs, "\n [-" , ResultHead, "-|  ", ResultRest, "]" ) ,  !,
%  Cou2 = Cou + 0 ,
%  bodylist_to_string_format( Cou2 ,  Res2, Body_list , Result ).
% 2.
%bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = cmp( Funcname, Lisx ) ,
%  Funcname = "," ,
%  Lisx = [ Headlis | Resli ] , Headlis = atom( Ava ), Ava = "!" , ! ,
%  var_list_to_prolog_vars_string( Resli , "" , Vars_Str ) ,
%  Space = string::create( Cou , " " ) ,
%   Out = string::concatList( [  Hs , Space , " ! , \n " , " " , Vars_Str , " "  , " ,\n" ] ),
%  Cou2 = Cou + 0 ,
%  bodylist_to_string_format( Cou2 ,  Out, Body_list , Result ).
%bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = cmp( Funcname, Lisx ) , Funcname = "," ,  ! ,
%  Lis2 = list::append( Lisx, Body_list ) ,
%  bodylist_to_string_format( Cou ,  Hs, Lis2 , Result ).
% std_metta_pred( "condition_if",  cmp(":-",[cmp("condition_if",[var("Var1"),var("Var2"),var("Outvx2"),var("Outvar_e3")]),cmp(",",[cmp("=",[var("Var2"),var("Var1")]),cmp(",",[atom("!"),cmp("=",[var("Outvar_e3"),var("Outvx2")])])])]) ).
%bodylist_to_string_format( _Cou , Hs , [] , Hs ):- ! .
%bodylist_to_string_format( Cou , Hs, [ H ] , Out ):-   H = cmp( Funcname, Lisx ) ,    Space = string::create( Cou , " " ) ,
% var_list_to_prolog_vars_string( Lisx , "" , Vars_Str ) , !,
% Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , " .\n" ] ) , ! .
%bodylist_to_string_format( Cou , Hs, [ H | Body_list ] , Result ):-   H = cmp( Funcname, Lisx ) ,   ! , Space = string::create( Cou , " " ) ,
%  var_list_to_prolog_vars_string( Lisx , "" , Vars_Str ) ,
%  Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , " ,\n" ] ),
%  Cou2 = Cou + 0 ,
%  bodylist_to_string_format( Cou2 ,  Out, Body_list , Result ).
%var_list_to_prolog_vars_string( Lisx , "" , Vars_Str ) ,
%Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , " ,\n" ] ),
% stdio::write( Space , toString( H ) , "\n" ),
%  Out = string::concatList( [  Hs , Space , Dbotag , Vars_Str , " "  , Comma_dot , " \n" ] ),
%  778899 remove the space
%  Out = string::concatList( [  Hs , Space , Dbotag , Vars_Str , " "  , Comma_dot ] ),
%  Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , Comma_dot , " \n" ] ),
%  778899
    %  778899 remove the space
%  Out = string::concatList( [  Hs , Space , Funcname , "( " , Vars_Str , " )"  , Comma_dot ] ),
% std_metta_predxy
% std_metta_pred( "condition_if",  cmp(":-",[
% cmp("condition_if",[var("Var1"),var("Var2"),var("Outvx2"),var("Outvar_e3")]),
% body
%  cmp(",",[cmp("=",[var("Var2"),var("Var1")]),cmp(",",[atom("!"),cmp("=",[var("Outvar_e3"),var("Outvx2")])])])
%  ]) ).
    bodylist_to_string_format(_Cou, Hs, [], Hs) :-
        !.

    bodylist_to_string_format(Cou, Hs, [H | Body_list], Result) :-
        H = cmp(Funcname, Lisx),
        Funcname = ",",
        !,
        Space = string::create(Cou, " "),
        var_list_to_prolog_vars_string(Lisx, "", Vars_Str),
        comma_or_dot(Body_list, Comma_dot),
        debug_tag(" jtnl ", Dbotag),
        Out = string::concatList([Hs, Dbotag, Vars_Str, " ", Comma_dot]),
        Cou2 = Cou + 0,
        bodylist_to_string_format(Cou2, Out, Body_list, Result).

    bodylist_to_string_format(Cou, Hs, [H | Body_list], Result) :-
        H = cmp(Funcname, Lisx),
        !,
        Space = string::create(Cou, " "),
        var_list_to_prolog_vars_string(Lisx, "", Vars_Str),
        comma_or_dot(Body_list, Comma_dot),
        Cou2 = Cou + 0,
        Out = string::concatList([Hs, Funcname, "( ", Vars_Str, " )", Comma_dot]),
        bodylist_to_string_format(Cou2, Out, Body_list, Result).

%---
% % TERMX_create = cmp(":-", [  cmp( Pname2 , [ cmp( Db_predname_res , Var_var_listx ) ] ),
% cmp("cell_xstate",[var("R"),var("C"),var("Sta")]),var("Outx")
    head_prolog_term_string(Head_list, Head_list_Str) :-
        Head_list = [Head_term, Outvar],
        Head_term = cmp(Func_name, Varlist),
        Outvar = var(Vs),
        !,
        var_list_to_prolog_vars_string(Varlist, "", Varlist_str),
        Head_list_Str = string::concat(Func_name, "(", Varlist_str, ") , ", Vs).

% 10:02 5-7-2025
    % Outvar = var( Vs ) ,
    head_prolog_term_string(Head_list, Head_list_Str) :-
        Head_list = [Head_term],
        Head_term = cmp(Func_name, Varlist),
        !,
        var_list_to_prolog_vars_string(Varlist, "", Varlist_str),
        Head_list_Str = string::concat(Func_name, "(", Varlist_str, ")  ").

% xxyy
%head_prolog_term_string( Head_list , Head_list_Str ):- Head_list_Str0 = toString( Head_list ) ,
% Head_list_Str = string::concat( "qqp6" , Head_list_Str0 ),!.
%    var_list_to_prolog_vars_string( Head_list , "qqp5" , Head_list_Str ) , ! .
    head_prolog_term_string(Head_list, Head_list_Str) :-
%    var_list_to_prolog_vars_string( Head_list , "qqp5" , Head_list_Str ) , ! .
        var_list_to_prolog_vars_string(Head_list, "", Head_list_Str),
        !.

%----
%  cmp(":-",[
    %  cmp("match_tempnum",[
    %  var("Atomvar#self"),var("Atomvar#True"),var("Num"),var("Outvar_e2")]),
    %  cmp(",",  [cmp("tempnum",[var("Num"),var("Outvar1")]),cmp("match",[var("Atomvar#self"),var("Outvar1"),var("Atomvar#True"),var("Outvar_e2")])])])
% stdio_write_clause( Comment , cmp( Tag , Lis ) ):-
% cmp("cell_xstate",[var("R"),var("C"),var("Sta")]),var("Outx")]),
% TERMX_create = cmp(":-", [  cmp( Pname2 , [ cmp( Db_predname_res , Var_var_listx ) ] ),
%cmp(":-",[cmp("sudoku_number_db",[
%  var("Num"),var("A"),var("B"),var("Lis"),var("Lis_res")]),
%  cmp(",",[cmp("retract",[cmp("sudoku_number_x",[var("Num"),var("A"),var("B")]) ]),
%  cmp(",",[atom("!"),
%  cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),
%  list(cmp("cell_candidatex",[var("B"),var("Num")]),  var("Lis")),
%  var("Lis_res")]  )]  )]  )
%  ])
% cmp(":-",[cmp("sudoku_number_db",[var("Num"),var("A"),var("B"),var("Lis_res"),var("Lis_res")]),atom("!")])
% std_metta_pred( "division", cmp(":-",[cmp("division",[var("X"),var("W"),var("Q")]),cmp("=",[var("Q"),cmp("/",[var("X"),var("W")])])]) ).
%prolog_list_clause_to_string_format( cmp( Tag , Lis ) , Hs , StringFormat ):-   Tag = ":-" ,
%   Lis = [ cmp( Funcname , Head_list ) , cmp( Tag2  , Body_list ) ] ,     Tag2 = "," ,    head_prolog_term( Head_list , Head_list_Str ) ,
%    Str_Head = string::concatList( [ Funcname , "( " , Head_list_Str , " )" ] ),
%   bodylist_to_string_format( 2, "", Body_list , Result ) ,
%   StringFormat = string::concat( Hs, Str_Head , ":-\n" , Result ),   ! .
% match( _ , _ , Invx1 , Outvx1 ):-
%  =( Invx1 , Outvx1 ) ,
%  ! .
% cmp(":-",[cmp("match",[var("_"),var("_"),var("Invx1"),var("Outvx1")]),
% cmp(",",[cmp("=",[var("Invx1"),var("Outvx1")]),atom("!")])
% ])
    extra_new_line_for_type_of_predicate(Comment, "\n") :-
        Comment = "end_result",
        !.
    extra_new_line_for_type_of_predicate(_Comment, "") :-
        !.

%  tmp a bit adhoc
% %end_result
% cmp("sudoku_number",[real(1)])
    prolog_list_clause_to_string_format(cmp(Funcname, Lis), Hs, StringFormat) :-
        Funcname <> ":-",
        amount_of_sub_nesting2(Lis, 0, Res),
        Res = 0,
        head_prolog_term_string(Lis, Head_list_Str),
        Str_Head = string::concatList([Funcname, "( ", Head_list_Str, " )"]),
        StringFormat = string::concat(Hs, "", Str_Head, ". \n"),
        !.

% amount_of
    prolog_list_clause_to_string_format(cmp(Tag, Lis), Hs, StringFormat) :-
        Tag = "assert",
        Lis = [cmp(Funcname, Head_list)],
        head_prolog_term_string(Head_list, Head_list_Str),
        Str_Head = string::concatList([Funcname, "( ", Head_list_Str, " )"]),
        StringFormat = string::concat(Hs, "assert( ", Str_Head, ") . \n"),
        !.

    prolog_list_clause_to_string_format(cmp(Tag, Lis), Hs, StringFormat) :-
        Tag = ":-",
        Lis = [cmp(Funcname, Head_list), atom(Cut)],
        Cut = "!",
        head_prolog_term_string(Head_list, Head_list_Str),
        Str_Head = string::concatList([Funcname, "( ", Head_list_Str, " )"]),
        StringFormat = string::concat(Hs, Str_Head, ":- ! . \n"),
        !.

% tmporary adhoc
    prolog_list_clause_to_string_format(cmp(Tag, Lis), Hs, StringFormat) :-
        Tag = ":-",
        Lis = [cmp(Funcname, Head_list), cmp(Subfunct, Funct_list)],
        head_prolog_term_string(Head_list, Head_list_Str),
        Str_Head = string::concatList([Funcname, "( ", Head_list_Str, " )"]),
        Subfunct = ",",
        Funct_list = [Clau1, Clau2],
        Clau1 = cmp(Operat, Varslist),
        Operat = "=",
        Varslist = [Var1, Var2],
        Clau2 = atom(Atom_s),
        Var1 = var(Svar1),
        Var2 = var(Svar2),
        Sz = string::concatList([Svar1, " = ", Svar2, " , ", Atom_s]),
        StringFormat = string::concatList([Hs, Str_Head, ":- ", Sz, ".\n"]),
        !.
% metta_pred
% tmporary adhoc

    prolog_list_clause_to_string_format(cmp(Tag, Lis), Hs, StringFormat) :-
        Tag = ":-",
        Lis = [cmp(Funcname, Head_list), cmp(Subfunct, Funct_list)],
        head_prolog_term_string(Head_list, Head_list_Str),
        Str_Head = string::concatList([Funcname, "( ", Head_list_Str, " )"]),
        Subfunct = "=",
        Funct_list = [Resultvar, Calc_clause],
        Resultvar = var(Rvar),
        Calc_clause = cmp(Calc_operat, Varslis),
        Varslis = [Var1, Var2],
        Var1 = var(Svar1),
        Var2 = var(Svar2),
        Sz = string::concatList([Rvar, " = ", Svar1, " ", Calc_operat, " ", Svar2]),
        StringFormat = string::concatList([Hs, Str_Head, ":- ", Sz, ".\n"]),
        !.

    prolog_list_clause_to_string_format(cmp(Tag, Lis), Hs, StringFormat) :-
        Tag = ":-",
        Lis = [cmp(Funcname, Head_list), cmp(Subfunct, Funct_list)],
        head_prolog_term_string(Head_list, Head_list_Str),
        Str_Head = string::concatList([Funcname, "( ", Head_list_Str, " )"]),
        bodylist_to_string_format(2, "", [cmp(Subfunct, Funct_list)], Result),
        debug_tag(" jaja ", Dbotag),
        get_indent(Sxp),
        Sz = string::concat("\n", Sxp),
        StringFormat = string::concat(Hs, Str_Head, ":-", Sz, Dbotag, Result),
        !.

% zzyy
%std_metta_pred( "condition_if",  cmp(":-",[
% cmp("condition_if",[var("Var1"),var("Var2"),var("Outvx2"),var("Outvar_e3")]),
%  cmp(",",[cmp("=",[var("Var2"),var("Var1")]),cmp(",",[atom("!"),cmp("=",[var("Outvar_e3"),var("Outvx2")])])])
%  ]) ).
% std_metta_pred( "condition_if",  cmp(":-",[cmp("condition_if",[var("Var1"),var("Var2"),var("Outvx2"),var("Outvar_e3")]),cmp(",",[cmp("=",[var("Var2"),var("Var1")]),cmp(",",[atom("!"),cmp("=",[var("Outvar_e3"),var("Outvx2")])])])]) ).
% std_metta_pred( "check_is_equal",  cmp(":-",[cmp("check_is_equal",[var("Var1"),var("Var2"),str("True")]),cmp(",",[cmp("=",[var("Var2"),var("Var1")]),atom("!")])]) ).
%    var_list_to_prolog_vars_string( Head_list , "" , Head_list_Str ) ,
%   Str_Head = toString( cmp( Funcname , Head_list ) ),
%   Str_Head = toString( cmp( Funcname , Head_list ) ),
    %  Tag2 = "," ,
    prolog_list_clause_to_string_format(Internal_clause, Hs, StringFormat) :-
        StringFormat = string::concat(Hs, toString(Internal_clause)),
        !.
%-----------
% file::appendString(  Filename,  Str,  _IsUnicodeFile ),

    stdio_write_clause(Comment, Prolog_list_clause_implementation) :-
        prolog_list_clause_to_string_format(Prolog_list_clause_implementation, "", StringFormat),
        !,
        stdio::write(Comment, "\n"),
        stdio::write(StringFormat, "\n\n").
    stdio_write_clause(_, _) :-
        !.

    copy_data_clause(Type_of_term, TERMX_trans, TERMX_trans2) :-
        Type_of_term = is_database_pred,
        TERMX_trans = cmp(Operat, Lis),
        Op2 = string::concat(Operat, "_x"),
        TERMX_trans2 = cmp(Op2, Lis),
        !.

% turn off seems to be not needed , only retractall is not implemented
% we didnt know so we have to do it another way
%memory_to_prolog_file():-
%   clause_memory( is_database_pred, Comment , TERMX_trans ) ,
%   copy_data_clause( is_database_pred , TERMX_trans, TERMX_trans2 ),
%   assert( clause_memory_tmp( is_database_pred, Comment , TERMX_trans2 ) ), fail, ! .
%memory_to_prolog_file():-
%  file::save( "results\\transpiled_intermediate_extra.pro", memory_clauses_tmp ) ,
%  retract( clause_memory_tmp( is_database_pred, Comment , TERMX_trans ) ),
%  asserta( clause_memory( is_database_pred, Comment , TERMX_trans ) ) , fail, ! .
    enums_komma_preds(Strea) :-
%  class facts - enums_preds
        enum_pred_mem(_Cou, _Pred, _Ariti, Predn),
% temporarly we dont need arity yet  because we only enum for komma,  but probably well do it for others  which use dynamic the arity
        Strea:write("\n%", Predn, " generated\n"),
        Strea:write("\n", Predn, "( _ , Vx1 , Vx1 ):- !.   \n"),
        fail,
        !.
    enums_komma_preds(Strea) :-
        !.

%  Strea:write("\nkomma15( _ , Vx1 , Vx1 ):- !.   \n"),
%  Strea:write("\nkomma16( _ , Vx1 , Vx1 ):- !.   \n"),
%  enums_komma_preds( Strea ) ,
%  class facts - enums_preds
%enum_pred_mem : ( integer, string, string ).
    string_predicate("bigger_than", "( A , B , \"True\" ):- A > B , !.", 3).
    string_predicate("bigger_than", "( _ , _ , \"False\" ):-  !.", 3).
    string_predicate("bigger_than_equal", "( A , B , \"True\" ):- A >= B , !.", 3).
    string_predicate("bigger_than_equal", "( _ , _ , \"False\" ):-  !.", 3).

    string_predicate("smaller_than", "( A , B , \"True\" ):- A < B , !.", 3).
    string_predicate("smaller_than", "( _ , _ , \"False\" ):-  !.", 3).
    string_predicate("smaller_than_equal", "( A , B , \"True\" ):- A =< B , !.", 3).
    string_predicate("smaller_than_equal", "( _ , _ , \"False\" ):-  !.", 3).

    string_predicate("bigger_then", "( A , B , \"True\" ):- A > B , !.", 3).
    string_predicate("bigger_then", "( _ , _ , \"False\" ):-  !.", 3).
    string_predicate("bigger_then_equal", "( A , B , \"True\" ):- A >= B , !.", 3).
    string_predicate("bigger_then_equal", "( _ , _ , \"False\" ):-  !.", 3).

    string_predicate("smaller_then", "( A , B , \"True\" ):- A < B , !.", 3).
    string_predicate("smaller_then", "( _ , _ , \"False\" ):-  !.", 3).
    string_predicate("smaller_then_equal", "( A , B , \"True\" ):- A =< B , !.", 3).
    string_predicate("smaller_then_equal", "( _ , _ , \"False\" ):-  !.", 3).

    string_predicate("dum_db_pred2", "():-!.", 0).
    string_predicate("is_not_equal", "( Vx1 , Vy2 , \"True\" ):- Vx1 <> Vy2 , !.  ", 3).
    string_predicate("is_not_equal", "( _ , _ , \"False\" ):-  !.  ", 3).
    string_predicate("match", "( _ , _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.", 5).
    string_predicate("match", "( _ , \"True\" , _ , \"True\" ):-  !.", 4).
    string_predicate("match", "( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.", 4).

%  Strea:write("\nmatch_all3( _ , Outvx1 , Outvx1 ):- !.\n"),
    string_predicate("match_all", "( _ , Outvx1 , Outvx1 ):- !.", 3).
    string_predicate("match_all", "( _ , Invx1 , _ , Invx1 ):-  !.", 4).
    string_predicate("match_all", "( _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.", 4).
    string_predicate("match_all", "( _ , _ , _ , Invx1 , Outvx1 ):- Outvx1 = Invx1 , !.", 5).
        %std_pred match variant 01
%std_pred match variant 02
% ****
% komma102( _ , Vx1 , Vx1 ):- !.
        %Strea:write("\nkomma102( _ , Vx1 , Vx1 ):- !.   \n"),
        %Strea:write("\nkomma15( _ , Vx1 , Vx1 ):- !.   \n"),
        %Strea:write("\nkomma16( _ , Vx1 , Vx1 ):- !.   \n"),
%  Strea:write("\n\n"),
        % Strea:write("\n\n"),

%  string_predicate( "let_star0" , "( _ , Outvx2 , Outvx2 ):- ! . " , 3 ).
%  string_predicate( "let_star2" , "( Outvx2 , _ , Outvx2 ):- ! . " , 3 ).
%  string_predicate( "let_star3" , "( Outvx2 , _ , Outvx2 ):- ! . " , 3 ).
%  string_predicate( "let_star4" , "( Outvx2 , _ , Outvx2 ):- ! . " , 3 ).
%  string_predicate( "let_star5" , "( _ , Outvx2 , Outvx2 ):- ! . " , 3 ).
%  string_predicate( "let_star" , "( _ , Outvx2 , Outvx2 ):- ! . " , 3 ).
    string_predicate("let_star3_1", "( Outvx2 , _ , Outvx2 ):- ! . ", 3).
    string_predicate("let_star3_2", "( _ , Outvx2 , Outvx2 ):- ! . ", 3).

% groupe( Row_sta , Col_sta , Q , Q_sta , App1 , App2 , Lis3 , Lis4 , Outvar_e9 ) ,
    string_predicate("groupe5_2", "( _ , Resu , _ , _ , Resu  ):- ! .", 5).
    string_predicate("groupe5_3", "( _ , _ , Resu , _ , Resu  ):- ! .", 5).

    string_predicate("groupe6_2", "( _ , Resu , _ , _ , _ , Resu  ):- ! .", 6).
    string_predicate("groupe7_6", "( _ , _ , _ , _ , _ , Resu , Resu  ):- ! .", 7).

    string_predicate("groupe5_cat3_4", "( _ , _ , Head_new , \"empty_atom\" , [ Head_new ]  ):- ! .", 5).
    string_predicate("groupe5_cat3_4", "( _ , _ , Head_new , Tail_new , [ Head_new | Tail_new ]  ):- ! .", 5).
    string_predicate("groupe5_4", "( _ , _ , _ , Tail_new ,  Tail_new   ):- ! .  ", 5).

    string_predicate("groupe9_8", "( _ , _ , _ , _ , _ , _ , _ , Lisx , Lisx  ):- ! .", 9).

%  string_predicate( "groupe" , "( _ , _ , Head_new , \"empty_atom\" , [ Head_new ]  ):- ! ." , 5 ).
%  string_predicate( "groupe" , "( _ , _ , Head_new , Tail_new , [ Head_new | Tail_new ]  ):- ! ." , 5 ).
%  string_predicate( "groupe2" , "( _ , _ , _ , Tail_new ,  Tail_new   ):- ! .  " , 5 ) .
%  Strea:write("\ncons_atom( [] , Lis , Lis  ):- ! . \n"),
    string_predicate("cons_atom", "( \"empty_atom\" , Lis , Lis  ):- ! . ", 3).
    string_predicate("cons_atom", "( Head , \"empty_atom\" , [ Head ]  ):- ! . ", 3).
    string_predicate("cons_atom", "( Head , Lis , [ Head | Lis ]  ):- ! .  ", 3).
%  string_predicate("\n% printing predicate ") .
    string_predicate("println", "( P_arg , \"True\" ):- write( P_arg ), write(\"\\n\"). ", 2).

    write_string_predicates(Strea) :-
        string_predicate(Predname, Sxy, Arity),
        Strea:write("\n", Predname, Sxy, "\n"),
        fail,
        !.

    write_string_predicates(_Strea) :-
        !.

    memory_to_prolog_file() :-
        file::save("results\\transpiled_intermediate0.pro", memory_clauses),
        split_if_condition_clauses(),
        re_arrange_add_remove_atom_clauses(),
        sort_clause_memory_by_fname(),
        validate_verify_the_program(),
        assert_newclause_metta_std_predicates(),
        file::save("results\\transpiled_intermediate.pro", memory_clauses),
        Strea = outputStream_file::createUtf8("results\\metta_to_prolog_transpiled.pro"),
        retractall(outp_file(_)),
        assert(outp_file(Strea)),
        write_empty_db_preds(Strea),
        %
        %
        % let_star
        % ICI
        write_string_predicates(Strea),
        fail,
        !.

    memory_to_prolog_file() :-
        outp_file(Strea),
        enums_komma_preds(Strea),
%  class facts - enums_preds
%enum_pred_mem : ( integer, string, string ).
        retract(clause_memory(Pred_count, is_standard_pred, Arityx, Comment, Name_of_pred, TERMX_trans)),
        extra_new_line_for_type_of_predicate(Comment, Exnl),
        % transpile_subcl_metta_tpro_subc( _Mlevel, _Level0 , _Level,  cmp( Operat ,  Atom_List2 )  ,  cmp( Operat ,  Atom_List2 ) , is_database_pred ):-
        prolog_list_clause_to_string_format(TERMX_trans, "", StringFormat),
        Strea:write(Exnl, "\n% ", toString(Pred_count), " ", Comment, " ", Name_of_pred, " arity:", toString(Arityx), "\n", StringFormat),
        fail,
        !.
        % Strea:write( "\n" , StringFormat ) , fail , ! .

    % extra_new_line_for_type_of_predicate( Comment, Exnl ),
    % transpile_subcl_metta_tpro_subc( _Mlevel, _Level0 , _Level,  cmp( Operat ,  Atom_List2 )  ,  cmp( Operat ,  Atom_List2 ) , is_database_pred ):-
%  Strea:write( Exnl, "\n%" , Comment , " ", Name_of_pred ,  "\n" , StringFormat ) , fail , ! .
%memory_to_prolog_file():-
%  outp_file( Strea ),
%  retract( clause_memory( _Pred_count, is_database_pred, Comment , Name_of_pred, TERMX_trans ) ),
%  prolog_list_clause_to_string_format( TERMX_trans , "", StringFormat ) ,
%  Strea:write( "\n" , StringFormat ) , fail , ! .
    memory_to_prolog_file() :-
        outp_file(Strea),
        retract(clause_memory(Pred_count, Type_of_pred, Arityx, Comment, Name_of_pred, TERMX_trans)),
        extra_new_line_for_type_of_predicate(Comment, Exnl),
        % transpile_subcl_metta_tpro_subc( _Mlevel, _Level0 , _Level,  cmp( Operat ,  Atom_List2 )  ,  cmp( Operat ,  Atom_List2 ) , is_database_pred ):-
        prolog_list_clause_to_string_format(TERMX_trans, "", StringFormat),
        Strea:write(Exnl, "\n% ", toString(Pred_count), " ", Comment, " ", Name_of_pred, " arity:", toString(Arityx), "\n", StringFormat),
        fail,
        !.

%memory_to_prolog_file():-
% outp_file( Strea ),
%  fail, !.
    memory_to_prolog_file() :-
        retract(outp_file(Strea)),
        Strea:close(),
        programControl::sleep(300),
        file::copy("results\\metta_to_prolog_transpiled.pro", "example_prolog_programs\\metta_to_prolog_transpiled.pro", file::overwrite()),
        !.

    memory_to_prolog_file() :-
        !.

%stdio_write_clause( Comment , Prolog_list_clause_implementation ):-
% stdio::write( Comment , "\n" ) ,
% stdio::write( toString( Prolog_list_clause_implementation ) ) , !.
    % prolog_list_clause_to_string_format( Prolog_list_clause_implementation , StringFormat ),
%---
%predicate_nesting(0,0,1,"get_cell_state",["$column","$row"]).
%predicate_nesting(0,0,1,"nesmathc",["$jh"]).
%predicate_nesting(1,1,2,"erer",["$er"]).
%predicate_nesting(1,1,2,"match",["$state"]).
%predicate_nesting(2,2,3,"lapred",["$ui","$weed"]).
%predicate_nesting(2,2,3,"erer",["$er"]).
%predicate_nesting(2,2,3,"extra",["$fd"]).
%predicate_nesting(3,3,4,"sudoku_puzzle_state",["$state","$quad","$column","$row"]).
%variabels_below_level( _Level_1, _Vars1, ["empty_temp"] ):- !.
%    predicate_nesting( _ , _Lower_level , Level , _Operat , Vars_list , _Nestlevel1 )
% if appendlist doesnt work, we have to copy predicate_nesting to predicate_nesting_tmp
% and do it with retract
    % findall( Vars_list,  predicate_nesting( _ , _ , Level , _ , Vars_list , _ ), Vars_list_list ) ,
    % findall( Vars_list,  predicate_nesting( _ , Level , _ , _ , Vars_list , _ ), Vars_list_list ) ,
    % findall( Lower_level, predicate_nesting( _ , Lower_level , Level , _ , _ , _ ), Lower_level_list0 ),
%variabels_for_level_list( _, [], Vlist_result, Vlist_result2 ):-   Vlist_result2 = list::removeDuplicates( Vlist_result ), ! .
%variabels_for_level_list( Prim_level , [ Level | Level_list ], Vlist, Variabels ):- ! ,
%  findall( Vars_list, predicate_nesting_below_primlevel2_name( Prim_level, Level, _XOperat2, Vars_list  ), Vars_list_list ),
%  Flat_list = list::appendList( Vars_list_list ),
%  Flat_list2 = list::append( Vlist,  Flat_list ),
%  variabels_for_level_list( Prim_level , Level_list , Flat_list2 , Variabels ).
%---
%         Msgx0 = string::format( "END % % % %  ", Op , Prim_level, toString( [] ), toString(Vars1_result)   ),
%		 vpiCommonDialogs::note( " END st 0" , Msgx0 ) ,
%          Msgx0 = string::format( "START % % % %  ", Operate , Prim_level, toString( [ Level | Sublevels ] ), toString(Vars1)   ),   vpiCommonDialogs::note( " START st 0" , Msgx0 ) ,
    %          variabels_for_level_list( Prim_level, Lower_level_list, [], Variabels_lower ),
    %   Catted_list2 = list::append( Catted_list , Variabels_lower ),
%variabels_below_level( Op , Prim_level ,  [], Vars1_result,  Vars1_result2  ):-
% Vars1_result2 = list::removeDuplicates( Vars1_result ) , ! .
%variabels_below_level( Operate, Prim_level , [ Level | Sublevels ], Vars1,  Result  ):-
%      Level_list = [ Level ],
%      variabels_for_level_list( Prim_level, Level_list, [], Variabels ),
%          findall( Lower_level , predicate_nesting_below_primlevel3_level( Prim_level, Level, Lower_level ) , Lower_level_list0 ),
%          Lower_level_list = list::removeDuplicates( Lower_level_list0 ),
%     Catted_list = list::append( Vars1 , Variabels ),
%        Remaining_level_list = list::append( Sublevels , Lower_level_list ),
%        Remaining_level_list_clean = list::removeDuplicates( Remaining_level_list ),
%		Remaining_level_list_clean2 = list::removeAll( Remaining_level_list_clean, Level ),
%   variabels_below_level( Operate, Prim_level , Remaining_level_list_clean2, Catted_list,  Result  ).
%  variabels_below_level( Operate, Prim_level , Remaining_level_list_clean, Catted_list,  Result  ).
    %         Msgx = string::format( "% % % todolev rema % % ", Operate , Prim_level, Level, toString(Remaining_level_list_clean2) , toString( Catted_list )  ),
%	      vpiCommonDialogs::note( " Continu " , Msgx ) ,
% variabels_below_level( _Level_1, _Vars1, Vars_list ) .
%---
%	Msgx = string::format( "% % % % ", Operat , Prim_level, Level, toString(Vars1)  ),
%	vpiCommonDialogs::note( " variabels START" , Msgx ) ,
%variabels_for_predicate( Operat, Prim_level, Level, Vars_list ):-
%    predicate_nesting_ordered( Prim_level , _ , _ , _Level_01 , Level_1 , Operat , Vars1 , _Nestlevel1 ), Level_1 = Level , ! ,
% variabels_below_level( Operat , Prim_level, [ Level_1 ] , Vars1 , Vars_list ) .
%variabels_for_predicate( _Operat, _, _, [] ):- !.
%---
% ( i, i, i, o , o , o , o , o , o ).
    nested_pred(Operat, Prim_level, Level, Predicate_length, Nested_name, Operat_list, TVars_list, Nestlevel1, Level_listx) :-
        predicate_nesting_ordered(Prim_level, Mlevel, _, _Level_01, Level_1, Operat, Vars1, _Vpos, Nestlevel1, Lin_nest, _Ariti),
        Level_1 = Level,
%     findall( Operat2, predicate_nesting( _ , Level_1 , _ , Operat2 , _ , _ ), Operat2_L ),
        % findall( Operat2, predicate_nesting_below_primlevel2_name( Prim_level, Level_1, Operat2, _  ), Operat2_L ),
        % TODO check wether we use  Nestlevel1  OR   _Lin_nest
        % ( i , i,i,i,o, i, o , i , o ).
%	 sequential_pred_names_and_vars( Nestlevel1 , Mlevel, Level_1, [] , Operat2_L , [] ,  Vars_list , [] , Level_list ),
        sequential_pred_names_and_vars(Lin_nest, Mlevel, Level_1, [], Operat2_L, [], Vars_list, [], Level_list),
        % max_deep_variabels_nest_connected( [ Subpnum ] , [ Sub_pred ], [] , Vars_list_deep ) ,
        % vars_slist_remove_pred_out_vars( Vars_list0 , Vars_list , _ ) ,
% 08:08 24-4-2025
        % TODO TODO TODO CHECK CHECK
        % OK it seems to work with  Lin_nest
%	   Lex = list::length( Operat2_L ) , 	   Remain_nest_level = Nestlevel1 - Lex ,
        Operat_list = list::append([Operat], Operat2_L),
        Predicate_length = list::length(Operat_list),
        Nested_name = string::concatWithDelimiter(Operat_list, "_"),
        %variabels_for_predicate( Operat, Prim_level, Level,  Vars_list ),
        % sequential_variabels_for_predicate( Mlevel , Level_1 , [] ,  Vars_list ) ,
        TVars_list0 = list::append(Vars1, Vars_list),
        TVars_list = list::removeDuplicates(TVars_list0),
        Level_listx = [Level | Level_list],
        !.
        % predicate_nesting( _ , Level_02 , _Level_2 , Operat2 , _Vars2 , _Nestlevel2 ),
        % Level_1 = Level_02 , !, Nested_name = string::concat( Operat , "_" , Operat2 ).

%----
    listi_lstr2_remove_head([Headnum | Subnums], [Headpred | Sub_preds], Headnum, Headpred, Subnums, Sub_preds) :-
        !.
    listi_lstr2_remove_head(Subnums, Sub_preds, 0, "", Subnums, Sub_preds) :-
        !.

% sequential
%-----
    max_deep_variabels_nest_connected(_, [], [], Vars_list_deep, Vars_list_deep, _Maxdepth) :-
        !.

    max_deep_variabels_nest_connected(Mlev2, [Subpnum | Sub_preds_nums], [Subpr_name | Sub_preds_names], Varslist, Vars_list_deep, Maxdepth) :-
        Maxdepth >= 0,
        nested_pred_memory(_, Lev, _, _, Subpnum, Subpr_name, _, _Subnest_name, Varlist_pred, Subnums2, _, Sub_preds2),
        Lev >= Mlev2,
        !,
        listi_lstr2_remove_head(Subnums2, Sub_preds2, _, _, Subnums3, Sub_preds3),
        Subnums4 = list::append(Sub_preds_nums, Subnums3),
        Sub_preds4 = list::append(Sub_preds_names, Sub_preds3),
        Varslist2 = list::append(Varslist, Varlist_pred),
        Varslist3 = list::removeDuplicates(Varslist2),
        Maxdepth2 = Maxdepth - 1,
        max_deep_variabels_nest_connected(Mlev2, Subnums4, Sub_preds4, Varslist3, Vars_list_deep, Maxdepth2).

    max_deep_variabels_nest_connected(Mlev2, [Subpnum | Sub_preds_nums], [Subpr_name | Sub_preds_names], Varslist, Vars_list_deep, Maxdepth) :-
        Maxdepth >= 0,
        predicate_nesting_ordered(_, Lev, _, _, Subpnum, Subpr_name, Varlist_pred, _Vpos, _, _Lin_nest, _),
        Lev >= Mlev2,
        !,
        Varslist2 = list::append(Varslist, Varlist_pred),
        Varslist3 = list::removeDuplicates(Varslist2),
        % QUESTION HERE  wether  to substract 1  here
        Maxdepth2 = Maxdepth - 1,
        max_deep_variabels_nest_connected(Mlev2, Sub_preds_nums, Sub_preds_names, Varslist3, Vars_list_deep, Maxdepth2).

% for safety  but shouldnt be nessecary ,  remove later if possible
    max_deep_variabels_nest_connected(Mlev2, [_ | Sub_preds_nums], [_ | Sub_preds_names], Varslist, Vars_list_deep, Maxdepth) :-
        !,
        max_deep_variabels_nest_connected(Mlev2, Sub_preds_nums, Sub_preds_names, Varslist, Vars_list_deep, Maxdepth).
%---
%set_nth_merge( Pos ,  [] , Cou , Varstr , Hl , Newlist ) :-  Cou = Pos , Hl = [Head|Rest] , !,  Newlist = list::reverse( [Head , Varstr | Rest ] ) , ! .

    set_nth_merge(Pos, [], Cou, Varstr, Hl, Newlist) :-
        Cou = Pos,
        !,
        Newlist = list::reverse([Varstr | Hl]),
        !.

    set_nth_merge(_Pos, [], _Cou, _Varstr, Hl, Newlist) :-
        Newlist = list::reverse(Hl),
        !.

    set_nth_merge(Pos, [H | Outvars_list], Cou, Varstr, Hl, Newlist) :-
        Cou = Pos,
        !,
        Cou2 = Cou + 1,
% set_nth_merge( Pos ,  Outvars_list  , Cou2 , Varstr , [ Varstr , H  | Hl ] , Newlist ).
        set_nth_merge(Pos, Outvars_list, Cou2, Varstr, [H, Varstr | Hl], Newlist).

    set_nth_merge(Pos, [H | Outvars_list], Cou, Varstr, Hl, Newlist) :-
        !,
        Cou2 = Cou + 1,
        set_nth_merge(Pos, Outvars_list, Cou2, Varstr, [H | Hl], Newlist).

%---
    add_vars_in_list_in_positions([], [], Outvars_list, Outvars_list) :-
        !.
    add_vars_in_list_in_positions([Pos | Posits_1_based], [Varstr | Direct_vars_str], Outvars_list, Outvars_list2) :-
        %  Pos2 = Pos - 1 , Pos2 >= 0 ,  Le = list::length( Outvars_list ), Le >= Pos ,   ! ,
        %  Newlist = list::setNth( Pos2 ,  Outvars_list,  Varstr ) ,
        set_nth_merge(Pos, Outvars_list, 1, Varstr, [], Newlist),
        !,
        add_vars_in_list_in_positions(Posits_1_based, Direct_vars_str, Newlist, Outvars_list2).

    add_vars_in_list_in_positions([_ | Posits_1_based], [_ | Direct_vars_str], Outvars_list, Outvars_list2) :-
        add_vars_in_list_in_positions(Posits_1_based, Direct_vars_str, Outvars_list, Outvars_list2).

%----
    find_output_var(Direct_vars_str0, _Outvar_end, Outvar_str, []) :-
        list::memberIndex_nd(El, _Nthx, Direct_vars_str0),
        parse_output_var(El, Outvar_str),
        !.
    find_output_var(_Direct_vars_str0, Outvar_end, Outvar_end, [Outvar_end]) :-
        !.

%----
% determ( i,i,i,i,o,o).
    make_clause_body_line_end(Callingnum, Callingpred, Outvar_end, Outvars_list, Clause_line_end, New_outvar) :-
        predicate_nesting_ordered(_, _, _, _, Callingnum, Callingpred, Direct_vars_str0, Posits_1_based, _Nestlevel, _Lin_nest, Ariti),
        !,
        untag_pred_out_vars(Direct_vars_str0, Direct_vars_str),
        find_output_var(Direct_vars_str0, Outvar_end, New_outvar, New_outvar_list),
        stdio::write("\n body_line_end: ", Callingpred, "\n", toString(Direct_vars_str), "\n"),
        % add_vars_in_list_in_positions   i,i,i,o
        add_vars_in_list_in_positions(Posits_1_based, Direct_vars_str, Outvars_list, Outvars_list2),
        % Predicate_outvar
        % member
        %  Outvars_list2 = list::append( Direct_vars_str , Outvars_list ),
%   Outvars_list3 = list::append( Outvars_list2 , [ Outvar_end ] ) ,
        Outvars_list3 = list::append(Outvars_list2, New_outvar_list),
        vars_slist_to_vars_list(Outvars_list3, Out_var_var_list),
        % check todo if this format is correct
        Clause_line_end = cmp(Callingpred, Out_var_var_list).
        % nul based  newlist = list::setNth : (    positive Index,    Elem* List,    Elem Item)

% default here but should possibly never be nessecary
    make_clause_body_line_end(_Callingnum, _Callingpred, Outvar_end, Outvars_list, Clause_line_end, New_outvar) :-
        vars_slist_to_vars_list(Outvars_list, Out_var_var_list),
        Clause_line_end = cmp("=", [var(Outvar_end), cmp("+", Out_var_var_list)]),
        !,
        New_outvar = Outvar_end.
        % nul based  newlist = list::setNth : (    positive Index,    Elem* List,    Elem Item)

    is_not_present_in_list(Outvar_str, Varlist_pred) :-
        list::isMember(Outvar_str, Varlist_pred),
        !,
        fail.
    is_not_present_in_list(_Outvar_str, _Varlist_pred) :-
        !.

% % condition_if_check_is_equal["$m","predicate_outvar$xm"]-Outvx2
%---
    set_outvar_in_list(Cou2, Varlist_pred, Varlist_pred2, Outvar_str) :-
%   stdio::write( toString( Varlist_pred ) , "\n" ),
        list::memberIndex_nd(El, Nthx, Varlist_pred),
        % todo
        parse_output_var(El, Outvar_str),
        is_not_present_in_list(Outvar_str, Varlist_pred),
        Varlist_pred2 = list::setNth(Nthx, Varlist_pred, Outvar_str),
        %  stdio::write( toString( Varlist_pred2 ) , Outvar_str , "\n" ),
        !.

    set_outvar_in_list(Cou2, Varlist_pred, Varslist_z, Sx) :-
        Sx = string::concat("Outvx", toString(Cou2)),
        Varslist_z = list::append(Varlist_pred, [Sx]),
        !.
%  Sx = string::concat( "Cr_outv" , toString( Cou2 ) ),     Varslist_z = list::append(  Varlist_pred , [ Sx ] ) ,!.
%---
    list_split_outvars([], [], []) :-
        !.
    list_split_outvars([H | Varlist_pred], Varlist_pred2, [H | Outvars_list]) :-
        parse_output_var(H, _Outvar_str),
        !,
        list_split_outvars(Varlist_pred, Varlist_pred2, Outvars_list).

    list_split_outvars([H | Varlist_pred], [H | Varlist_pred2], Outvars_list) :-
        !,
        list_split_outvars(Varlist_pred, Varlist_pred2, Outvars_list).

%---
    outvars_at_the_end(Varslist3_px0, Varslist3_p) :-
        list_split_outvars(Varslist3_px0, Vars_list, Vars_list_deep_outvars),
        Vars_listz = list::removeDuplicates(Vars_list),
        Vars_list_deep_outvarsz = list::removeDuplicates(Vars_list_deep_outvars),
        Varslist3_p = list::append(Vars_listz, Vars_list_deep_outvarsz).
%	 list_split_outvars( Varlist_pred, Varlist_pred2, Outvars_pred ),
%	    Varslist2x = list::append(  Varlist_pred2 , Vars_list_deep2  ) ,
%	     Varslist2_dbl = list::append(  Varslist2x , Outvars_pred  ) ,
%		   Varslist2_dblz = list::removeDuplicates( Varslist2_dbl ) ,
%		 Varslist2_dblx = list::append(  Varslist2_dblz , Vars_list_deep_outvars  ) ,
        %       Varslist2 = Varslist2_dblx ,
%	   Varslist3_p = list::removeDuplicates( Varslist2 ) ,

% we think it works , that meabs it seems to work
    it_doesnt_have_add_atom_or_remove_atom_as_previous(Mlevelx, Sub_pred) :-
% nested_pred_memory( Mlevelx ,2,1,1,2,"remove_atom",2,"remove_atom_act_rc",["atomvar#&self","$r","$c"],[2,3],1,["remove_atom","act_rc"]).
        nested_pred_memory(Mlevelx, _, _, _, _, Remove_or_add_atom, _, _, _Varlis, _, _, _Pred_sublis),
        (Remove_or_add_atom = "remove_atom" or Remove_or_add_atom = "add_atom"),
        !,
        fail.

    it_doesnt_have_add_atom_or_remove_atom_as_previous(_Mlevelx, _Sub_pred) :-
        !.

%---
% ignore for println
    append_outvar_to_list(Sub_pred, _Sx, Outvars_list, Outvars_list) :-
        Sub_pred = "println",
        !.
    append_outvar_to_list(_Sub_pred, Sx, Outvars_list, [Sx | Outvars_list]) :-
        !.

%----
    warning_if_it_is_already_declared(Pred_count, Subnest_name) :-
        clause_memory(Pred_count2, _, _, _, Subnest_name, _),
        Diff = Pred_count - Pred_count2,
        Diff > 1,
        !,
        % vpiCommondialogs::note( "Predicate is already declared", Subnest_name ) .
        stdio::write("\nWarning: Predicate is already declared", Subnest_name, "\n").

    warning_if_it_is_already_declared(_Pred_count, _Subnest_name) :-
        !.

%---
    verify_predicates_exist(Clause_x) :-
        Clause_x = cmp(Operat1, Lis1),
        Operat1 = ":-",
        Lis1 = [El1, El2],
        El1 = cmp(Fnme_head_, _Varlis_head),
        El2 = cmp(_Komma_op, Flis_body),
        verify_predicates_exist_body(Flis_body),
        !.
%    Fu2 = cmp( Fnme_body, Varlis_body ) ,
    verify_predicates_exist(_Clause_x) :-
        !.
%---

    verify_predicate_exist("assert", 1) :-
        !.
    verify_predicate_exist("match", 3) :-
        !.
    verify_predicate_exist("asserta", 1) :-
        !.
    verify_predicate_exist("assertz", 1) :-
        !.
    verify_predicate_exist("assert", 1) :-
        !.
    verify_predicate_exist("retract", 1) :-
        !.
    verify_predicate_exist(",", 2) :-
        !.

    verify_predicate_exist("action_stack", 2) :-
        !.
    verify_predicate_exist("mem_candidates", 3) :-
        !.
    verify_predicate_exist("trace_field_assigned_reverse", 3) :-
        !.
    verify_predicate_exist("mem_candidates", 3) :-
        !.
    verify_predicate_exist("false_candidate", 4) :-
        !.

    verify_predicate_exist(Operat1, _Arityx) :-
        enum_pred_mem(_Cou, _Pred, _Arit, Predn),
        Predn = Operat1,
        !.

    % Arity = Arityx , !.
    verify_predicate_exist(Operat1, Arityx) :-
        std_metta_pred(Funcname_s, Arity, _Func_clause),
        Funcname_s = Operat1,
        Arity = Arityx,
        !.

    verify_predicate_exist(Operat1, Arityx) :-
        string_predicate(Predname, Sxy, Arity),
        Predname = Operat1,
        Arity = Arityx,
        !.
        % stdio::write( "\n" , "Predicate exists std pred : " , Operat1 , " - ", toString( Arityx ) ) .

    verify_predicate_exist(Operat1, Arityx) :-
        clause_memory(_Pred_count, _Type_of_pred, Arityx, _Comment, Operat1, _Clause_x),
        !.
        % stdio::write( "\n" , "Predicate exists: " , Operat1 , " - ", toString( Arityx ) ) .

    verify_predicate_exist(Operat1, Arityx) :-
        stdio::write("\n", "Warning: undeclared predicate: ", Operat1, " - ", toString(Arityx)).

%---
    verify_predicates_exist_body([]) :-
        !.
    verify_predicates_exist_body([H | Flis_body]) :-
        H = cmp(Operat1, Lis1),
        arity_of_head_vars(Lis1, Arityx),
        !,
        verify_predicate_exist(Operat1, Arityx),
        verify_predicates_exist_body(Flis_body).

    verify_predicates_exist_body([_H | Flis_body]) :-
        !,
        verify_predicates_exist_body(Flis_body).

%---
    validate_verify_the_program() :-
%  clause_memory( Pred_count2, _ , _ , _ , Subnest_name , _ ) ,
        clause_memory(_Pred_count, _Type_of_pred, Arity_head, _Comment, Subnest_name, Clause_x),
        verify_predicates_exist(Clause_x),
        fail,
        !.
    validate_verify_the_program() :-
        !.

%---
% uyuyuyuy
% ( i,i,i, i,i,i,i,i,o,o).
    construct_implement_clause_body(_Predcount, Callingnum, Callingpred, _Varnameshead, _Mlevel, [], [], Cou, Outvars_list0, Clause_lines,
            Clause_result, New_outvar_end) :-
        Cou2 = Cou + 1,
        Outvar_end = string::concat("Outvar_e", toString(Cou2)),
        Outvars_list = list::reverse(Outvars_list0),
% make_clause_body_line_end  ( i,i,i,i,o,o).
        make_clause_body_line_end(Callingnum, Callingpred, Outvar_end, Outvars_list, Clause_line_end, New_outvar_end),
%  vars_slist_to_vars_list( Outvars_list , Out_var_var_list ) ,
        % Clause_line_end = cmp( "=" , [ var( Outvar_end ) , cmp( "+" ,  Out_var_var_list  )  ] ) ,
        % nul based  newlist = list::setNth : (    positive Index,    Elem* List,    Elem Item)
        %  Here
        %  change this  Clause_line_add  with   _Callingnum, _Callingpred
        %   if it can be foud, if it is a predicate  , the default maybe the =   if it cant be found or if it is empty
        % and check if it has a real var with position and put it in the right argument
        Clause_lines_rev = list::reverse(Clause_lines),
        Clause_result = list::append(Clause_lines_rev, [Clause_line_end]),
        !.

%1	stdio::write( "\n AFTER SPLIT: \n", toString(Varlist_pred2), "\n", toString(Vars_list_deep), "\n",toString(Varslist2x) , "\n"),
% 13:58 30-5-2025
%2   stdio::write( "\n before APPEND: \n", toString( Varslist2x ) , "\n" ),
%3       stdio::write( "\n APPEND: \n", toString( Varslist2_dblx ) , "\n" ),
    % 13:58 30-5-2025
%4     stdio::write( "\n outv SPLITx2: \n", toString( Varslist2x ) ),
%5	 stdio::write( "\n outv SPLITx3: \n", toString( Varslist2 ) ),
%6	  stdio::write( "\n outv SPLITx4: \n", toString( Varslist3_p ) ),
%7	  stdio::write( "\n outv in list: " , Sub_pred , "\n" ,  toString( Varslist3 ) , "\n",  toString( Varslist_z0 ) ) ,
    % Varslist2x_db = list::append(  Varlist_pred2 , Vars_list_deep  ) ,
%        Varslist2x = list::removeDuplicates( Varslist2x_db ) ,
%		         Varslist2 = list::removeDuplicates( Varslist2_dbl ) ,
%     list_split_outvars( Vars_list_deep, Vars_list_deep2, Vars_list_deep_outvars ),
%	 list_split_outvars( Varlist_pred, Varlist_pred2, Outvars_pred ),
%	    Varslist2x = list::append(  Varlist_pred2 , Vars_list_deep2  ) ,
%	     Varslist2_dbl = list::append(  Varslist2x , Outvars_pred  ) ,
%		   Varslist2_dblz = list::removeDuplicates( Varslist2_dbl ) ,
%		 Varslist2_dblx = list::append(  Varslist2_dblz , Vars_list_deep_outvars  ) ,
    %       Varslist2 = Varslist2_dblx ,
%	   Varslist3_p = list::removeDuplicates( Varslist2 ) ,
%			   Sx = string::concat( "Outvar" , toString( Cou2 ) ),     Varslist_z = list::append(  Varslist3 , [ Sx ] ) ,
%					  vars_slist_remove_pred_out_vars( keep_all , Varslist_z0 , Varslist_z02 , _ ) ,
% parse_output_var( El , Outvar_str ) ,
%					    vars_slist_remove_pred_out_vars( 0, Varslist_z0 , Varslist_z , _ ) ,
    % everywhere where max_deep_variabels_nest_connected
% todo a faire  , question if this works well for all cases
    % test  IF retract works
% is tag67 :  If  Sub_pred - Subnest_name  is used  in:  where  "_db"  is concatted
% this clause here is skipped, that means , Clause_line  ans Sx should not be added
% so make it loop here without adding   Clause_line  and Sx
%    max_deep_variabels_nest_connected( [ Subpnum ] , [ Sub_pred ], [] , Vars_list_deep , 1 ) ,
% Clause_implementation--
    % match_db_variabels(
%	 db_var_list( Varslist3_px0_y, Varnames_head , Sub_pred, Varslist3_px0 ) ,
% 12:23 24-6-2025
% last modif, quest if tis works  for the rest
%    	  set_outvar_in_list( Cou2 , Varslist3 , Varslist_z0 , Sx ) ,
%		   untag_pred_out_vars( Varslist_z02q , Varslist_z02 ) ,
%       stdio::write( "bb00- ",  toString( Subclauses_body_call_type ) , "-zz-", toString( Varslist3 ),  "-", toString( Varslist_z02 ), "\n") ,
    % condition_if_check_is_equal["$m","predicate_outvar$xm"]-Outvx2
%       stdio::write( "aaxx00- ",  toString( Varslist_z02 ) , "-q-", toString( Varslist_z02x ),  "-", Sx, "\n") ,
% is tag67 :  If  Sub_pred - Subnest_name  is used  in:  where  "_db"  is concatted
% this clause here is skipped, that means , Clause_line  ans Sx should not be added
%		  vars_slist_clean_pred_out_vars_names( Varslist_z02x ,  Varslist_z03  ),
%			   stdio::write( "\n check clause line apapap: \n" , toString( Clause_line ) , "\n" ) ,
%			 stdio::write( "aaxx- ",  Subnest_name , toString( Varslist3 ) , "-", Sx, "\n") ,
% 	stdio::write( "cc00dd- \n" ),
    construct_implement_clause_body(Predcount, Callingnum, Callingpred, Varnames_head, Mlevel, [Subpnum | Sub_preds_nums],
            [Sub_pred | Sub_preds_names], Cou, Outvars_list, Clause_lines, Clause_body, Outvar_end) :-
        Cou2 = Cou + 1,
        Mlevelx = Mlevel + Cou2,
        nested_pred_memory(_, Mlevelx, _, _, Subpnum, Sub_pred, _, Subnest_name, Varlist_pred, Subnums2, _, Sub_preds2),
        % xxxx
        hide_fail_if_is_inside_db_pred(Callingpred, "nested_pred_memory", Sub_pred, Varlist_pred, "", _XSub_pred2),
        !,
        % tag69
        max_deep_variabels_nest_connected(Mlevelx, [Subpnum], [Sub_pred], [], Vars_list_deep, 50),
        Varslist3_px0 = list::append(Varlist_pred, Vars_list_deep),
        outvars_at_the_end(Varslist3_px0, Varslist3_p),
        current_transpile_model_get(_, _, _, Subclauses_body_call_type, _),
        remove_real_int_vals(Subclauses_body_call_type, Varslist3_p, Varslist3, _),
        vars_slist_remove_pred_out_vars(Subclauses_body_call_type, "", Varslist3, Varslist_z02, _),
        set_outvar_in_list(Cou2, Varslist_z02, Varslist_z02x, Sx),
        untag_pred_out_vars(Varslist_z02x, Varslist_z03),
        Varslist_z = list::removeDuplicates(Varslist_z03),
% we hope it is here  the empty  list  for let_star_groupe
        vars_slist_to_vars_list(Varslist_z, Var_var_list),
        % %	    change_db_pred_name( Pname, Pname2 ),
        Clause_line = cmp(Subnest_name, Var_var_list),
        construct_implement_clause_body(Predcount, Callingnum, Callingpred, Varnames_head, Mlevel, Sub_preds_nums, Sub_preds_names, Cou2,
            [Sx | Outvars_list], [Clause_line | Clause_lines], Clause_body, Outvar_end).

    %   Re = string::format( "try find ximp % % %",  Mlevel ,  Subpnum  ,  Sub_pred  ), stdio::write( Re, "\n" ),
    % QUESTION is if this is allowed  !!!!  Mlevel  plus test  IF retract works
    % only here Leave  the realvals from Var_var_list Varlist_pred ?
    % remove_real_int_vals( Varlist_pred_z, Varlist_pred , _ ) ,
%   stdio::write( "\n outv2 in list: " , Sub_pred , "\n" ,  toString( Varlist_pred ) , "\n",  toString( Varslist_z ) ) ,
%				  Sx = string::concat( "Outvarp2" , toString( Cou2 ) ),     Varslist_z = list::append(  Varlist_pred , [ Sx ] ) ,
    % qqqwww
    %                 vars_slist_remove_pred_out_vars( Varslist_z0 , Varslist_z , _ ) ,
    % here remove  the realvals from Var_var_list Varlist_pred ?
%			   stdio::write( "\n Xcheck clauseli apapap: \n" , toString( Clause_line ) , "\n" ) ,
    construct_implement_clause_body(Pred_count, Callingnum, Callingpred, Varnames_head, Mlevel, [Subpnum | Sub_preds_nums],
            [Sub_pred | Sub_preds_names], Cou, Outvars_list, Clause_lines, Clause_body, Outvar_end) :-
        Cou2 = Cou + 1,
        Mlevelx = Mlevel + Cou2,
        predicate_nesting_ordered(_, Mlevelx, _, _, Subpnum, Sub_pred, Varlist_pred, _Vpos, _, _Lin_nest, _),
        % TODO here
        % add_db_input_list( Varlist_pred, Varnames_head , Sub_pred, Varlist_pred2 ) ,
        % Varlist_pred = list::append( Varlist_pred0, [ "add_inp_lis[]" ] ),
        set_outvar_in_list(Cou2, Varlist_pred, Varslist_z_pred, Outvar_create),
        % set_outvar_in_list( Cou2 , Varlist_pred, Varslist_z2 , _Sx2 ) ,
        % vars_slist_to_vars_list : ( string* , sterml ) determ( i, o ).
        %   vars_slist_to_vars_list( Varslist_z2 , Var_var_list2 ) ,
        % => vars_slist_to_vars_list( Varslist_z_pred , Var_var_list_pred ) ,
        it_doesnt_have_add_atom_or_remove_atom_as_previous(Mlevelx, Sub_pred),
        create_db_predicate(Pred_count, Varnames_head, Sub_pred, Varlist_pred, Varslist_z_pred, Outvar_create, Sub_pred2, Outvar_create02, _,
            Clause_line0, Clause_Line, _XType_match_db),
        % went wrong
        % Type_match_db <> "match" ,
        % it fails  but it allows it to be created , after it fails with fail_create below
        !,
% nill is the empty list   aparantly
        % Clause_line = cmp( Sub_pred2 , Var_var_list_pred ),
%     Clause_line = cmp( Sub_pred2 , [nill] ),
        construct_implement_clause_body(Pred_count, Callingnum, Callingpred, Varnames_head, Mlevel, Sub_preds_nums, Sub_preds_names, Cou2,
            [Outvar_create, Outvar_create02 | Outvars_list], [Clause_line, Clause_line0 | Clause_lines], Clause_body, Outvar_end).

    construct_implement_clause_body(Pred_count, Callingnum, Callingpred, Varnames_head, Mlevel, [Subpnum | Sub_preds_nums],
            [Sub_pred | Sub_preds_names], Cou, Outvars_list, Clause_lines, Clause_body, Outvar_end) :-
        Cou2 = Cou + 1,
        Mlevelx = Mlevel + Cou2,
        predicate_nesting_ordered(_, Mlevelx, _, _, Subpnum, Sub_pred, Varlist_pred, _Vpos, _, _Lin_nest, _),
        set_outvar_in_list(Cou2, Varlist_pred, Varslist_z, Outvar_create),
        vars_slist_to_vars_list(Varslist_z, Var_var_list),
        stdio::write(" Check if FAILS 00 ", Sub_pred, "\n"),
        hide_fail_if_is_inside_db_pred(Callingpred, "predicate_nesting_ordered", Sub_pred, Varlist_pred, Outvar_create, Sub_pred2),
        !,
        Clause_line = cmp(Sub_pred2, Var_var_list),
        append_outvar_to_list(Sub_pred, Outvar_create, Outvars_list, Outvars_list_n),
%   construct_implement_clause_body( Pred_count, Callingnum, Callingpred , Varnames_head, Mlevel , Sub_preds_nums  , Sub_preds_names  , Cou2 , [ Sx | Outvars_list ] , [ Clause_line | Clause_lines ] , Clause_body , Outvar_end ).
        construct_implement_clause_body(Pred_count, Callingnum, Callingpred, Varnames_head, Mlevel, Sub_preds_nums, Sub_preds_names, Cou2,
            Outvars_list_n, [Clause_line | Clause_lines], Clause_body, Outvar_end).

% for safety  but shouldnt be nessecary ,  remove later if possible
    construct_implement_clause_body(Pred_count, Callingnum, Callingpred, Varnames_head, Mlevel, [_Subpnum | Sub_preds_nums],
            [_Sub_pred | Sub_preds_names], Cou, Outvars_list, Clause_lines, Clause_body, Outvar_end) :-
        construct_implement_clause_body(Pred_count, Callingnum, Callingpred, Varnames_head, Mlevel, Sub_preds_nums, Sub_preds_names, Cou,
            Outvars_list, Clause_lines, Clause_body, Outvar_end).
%---

    construct_subclauses(_, _, [], []) :-
        !.
    construct_subclauses(Pred_count, Varnames_head, [Sub_num | Sub_preds_nums], [Sub_pred | Sub_preds_names]) :-
        %  max_deep_variabels_nest_connected( [ Sub_num ] , [ Sub_pred ] , [] , Vars_list_deep ) ,
        nested_pred_memory(_, Mlevel, _, _, Sub_num, Sub_pred, _, Subnest_name, Varlist_pred, Subnums2, _, Sub_preds2),
        !,
        listi_lstr2_remove_head(Subnums2, Sub_preds2, Callingnum, Callingpred, Subnums4, Sub_preds4),
        current_transpile_model_get(_, _, Subc_head_trans_type, _, _),
        construct_implement_clause(Pred_count, Subc_head_trans_type, Callingnum, Callingpred, Varnames_head, Mlevel, Subnest_name, Varlist_pred,
            Subnums4, Sub_preds4, Clause_implementation, _Vars_list3x, Arity_head),
%      stdio::write(  ),
        warning_if_it_is_already_declared(Pred_count, Subnest_name),
        assert(clause_memory(Pred_count, is_sub_clause, Arity_head, "Subclausex  : ", Subnest_name, Clause_implementation)),
%	  stdio_write_clause( "subclause imp check : \n" , Clause_implementation ),
%	  stdio::write( "\n" ) ,
        % stdio::write( "subclause imp check : " ,  toString( Clause_implementation ) ,
        % RECONS_NEWCLAUSE
        % temporary not needed  we  consult the whole prolog result file
% 	   recons_newclause_z( "construct_subclauses" , Clause_implementation ),
        % question is if it is complete yet    check in console if it is valid
        % vars_slist_to_vars_list( Vars_list3x , Var_var_list ) ,
        % Fhead = cmp( Subnest_name , Var_var_list ),
        % Vars_list4x = list::append( Vars_list_deep , [ Outvar_end ] ) ,
        % TODO  works not yet !!!!
        Subnums3 = list::append(Subnums4, Sub_preds_nums),
        Subpreds3 = list::append(Sub_preds4, Sub_preds_names),
        construct_subclauses(Pred_count, Varnames_head, Subnums3, Subpreds3).

    construct_subclauses(Pred_count, Varnames_head, [_ | Sub_preds_nums], [_ | Sub_preds_names]) :-
        !,
        construct_subclauses(Pred_count, Varnames_head, Sub_preds_nums, Sub_preds_names).

%----
% Rename this later
    arity_of_head_vars(Var_var_list, Arity) :-
        Arity = list::length(Var_var_list),
        !.
    arity_of_head_vars(_Var_var_list, 0) :-
        !.

%-----
% "$"
% todo remove    predicate out vars , probably in the heads
% Subclauses_head_type
    construct_implement_clause(Pred_count, Subc_head_transp_type, Callingnum, Callingpred, Varnames_head, Mlevel, Nested_name, Vars_list,
            Sub_preds_nums, Sub_preds_names, Clause_implementation, Vars_list3x, Arity) :-
        % listi_lstr2_remove_head( Sub_preds_nums0 , Sub_preds_names0, Sub_preds_nums , Sub_preds_names ),
        Llex = list::length(Sub_preds_names),
        which_depth_to_use(Llex, 50, Use_depth),
        % length of list here OR endless depth
%	   max_deep_variabels_nest_connected( Sub_preds_nums , Sub_preds_names , [] , Vars_list_deep , Llex ) ,
        max_deep_variabels_nest_connected(Mlevel, Sub_preds_nums, Sub_preds_names, [], Vars_list_deep, Use_depth),
        % ****
        % current_transpile_model( _ , Subclauses_head_type , _ ) ,
        % WAS standing HERE
        %  vars_slist_remove_pred_out_vars( Subclauses_head_type , Vars_list_deep0 , Vars_list_deep , _ ) ,
        % TODO  maybe set vars_slist_remove_pred_out_vars lower
%	   vars_slist_remove_pred_out_vars( keep_all , Vars_list_deep0 , Vars_list_deep , _ ) ,
        Vars_list2y_y = list::append(Vars_list, Vars_list_deep),
        db_var_list(Vars_list2y_y, Varnames_head, Nested_name, Vars_list2y),
        % ******
        vars_slist_remove_pred_out_vars(Subc_head_transp_type, Nested_name, Vars_list2y, Vars_list2q, _),
%		 stdio::write( "tempdetect", toString( Subc_head_transp_type ) , Nested_name,  toString(Vars_list2y) , toString(Vars_list2q) ,"\n"),
        untag_pred_out_vars(Vars_list2q, Vars_list2),
        Vars_list3x_z = list::removeDuplicates(Vars_list2),
        % current_transpile_model( _ , _ , Sequential_var_names ) ,
        remove_real_int_vals(Subc_head_transp_type, Vars_list3x_z, Vars_list3x_0, _),
        % TODO  maybe set vars_slist_remove_pred_out_vars lower to here?
        construct_implement_clause_body(Pred_count, Callingnum, Callingpred, Varnames_head, Mlevel, Sub_preds_nums, Sub_preds_names, 0, [], [],
            Clause_body, Outvar_end),
        % TOD NESSECARY or better solution reverse in clause body
%	   try_remove_
        Vars_list3x = list::remove(Vars_list3x_0, Outvar_end),
        % Question if   remove always succeeds
        Vars_list4x = list::append(Vars_list3x, [Outvar_end]),
        vars_slist_to_vars_list(Vars_list4x, Var_var_list),
        arity_of_head_vars(Var_var_list, Arity),
        % here remove  the realvals from Var_var_list Vars_list3x ?
        Clause_body2 = list::append(Clause_body, [atom("!")]),
        Clause_implementation = cmp(":-", [cmp(Nested_name, Var_var_list), cmp(",", Clause_body2)]),
        !.
%	   Clause_implementation = cmp( ":-",  	[    cmp( Nested_name,  Var_var_list  ) ,  cmp( "," , Clause_body ) )	   ]	   ) , ! .

% "!"
% TEMP add Cut  allways at the end of every body clause created
    %  construct_subclauses( Sub_preds_nums , Sub_preds_names )   .
    % TODO only remaining   auto construct all subclauses in the same fashion   if possible but should be possible
%	   construct_implement_clauses_sub( Sub_preds_nums , Sub_preds_names ) .
% transpile_term( cmp( Operat,  Atom_List2 ), cmp( "sub_transp0",  Atom_List2 ) ):-
%   Operat = "sub",
%   Atom_List2 = [ El | _ ],
%   nested_preds(
%   ! .
% ,var("$state")
% here you should be able to match unify  any complex term to another term
% temp to simple flat term  for the example
%cmp("=",[cmp("sub",[cmp("get_cell_state",[var("$row"),var("$column")])]),
% cmp("sub",[
%    cmp("match",
% [atom("&"),atom("self"),
%  cmp("sub",[cmp("sudoku_puzzle_state",[var("$row"),var("$column"),var("$quad"),var("$state")])]),
%   var("$state")  ]
%  )
%  ])
%  ])
    % Operat2 = "match" ,
%	 Term_list = [ _El1 , _El2 , El3, _El4  ],
    %    El3 = cmp( _Sub , [  Match_term_to_execute  ]),
    %    Match_term_to_execute = cmp( Db_pred_call , _Match_var_list ) ,
%	 Temp_str = string::concat( Db_pred_call, "_tmp_va" ),
% transpile_complex_term(   cmp( Operat,  Term_list0 ) ,  cmp( Nested_name ,  [ var("tempx_nest_pred") ] ) ):-  Operat = "sub" ,
% EXAMPLE :::
%uicici cmp(":-",[cmp("funcname",[var("A"),var("B"),var("C")]),cmp(",",[cmp("=",[var("C"),cmp("+",[var("A"),var("B")])]),atom("!")])])
%uicici cmp(":-",[cmp("func3",[var("X"),var("W"),var("Q")]),cmp(",",[cmp("funcname",[var("X"),var("W"),var("H")]),cmp("=",[var("Q"),cmp("+",[var("H"),int(2)])])])])
%uicici cmp(":-",[cmp("dox",[]),cmp(",",[cmp("func3",[int(9),int(77),var("Q")]),cmp("write",[var("Q")])])])
    %Operat = "sub" ,
    %Term_list0 = [ Elem_0 | _ ],
    % Elem_0 = cmp( Operat2, Term_list ),
    % nested_pred_memory(2,1,0,0,1,"if",3,"if_and_if",["realvar#0","$Cs"],[1,2,2],13,["if","and","if"]).
    % format
% transpile_term( _Mlevel , _Level0, _Level , cmp( Operat,  Atom_List2 ), cmp( "sub_transp0",  Atom_List2 ) ):-  Operat = "sub",  ! .
    transpile_simpel_term(_Mlevel, _Level0, _Level, cmp(Operat, Atom_List2), cmp(Funcname, Funclist)) :-
        Operat = "sub",
        get_head_term_from_list(Atom_List2, Head_term_0),
        Head_term_0 = cmp(Funcname, Funclist_0),
        % test , question is wether we should  change the variabels here, or earlier , maybe during parsing
        metta_vars_list_to_prolog_vars_list(Funclist_0, Funclist),
        !.
% head
        %    transpile_complex_term(  cmp( Operat,  Atom_List2 ) , Complex_transpiled ) , ! ,

    transpile_simpel_term(_, _, _, Terx, Terx) :-
        !.  %  on purpose catch for safety
%-------

% maybe remove this one
    transpile_default_via_invent(_Pred_count, Mlevel, Level0, Level, _Varnms_h, cmp(Operat, Term_list0), cmp(Nested_name, Var_var_list)) :-
        Msgx0 = string::format("ytyty % % % %  ", Mlevel, Level0, Level, Operat),
        stdio::write("try nested find for: ", Msgx0, "\n"),
        nested_pred_memory(_, Mlevel, Level0, Level0, _, Operat, _, Nested_name, Vars_list, _, _, _),
        vars_slist_to_vars_list(Vars_list, Var_var_list),
        !.

% asser
% last , use this to transpile   the first occuring clause to its high level  without the nestings
% last !!!!!
    transpile_default_via_invent(Pred_count, Mlevel, Level0, Level, Varnames_head, cmp(Operat, Term_list0), cmp(Nested_name, Var_var_list)) :-
        Operat = "sub",
        Term_list0 = [Elem_0 | _],
        Elem_0 = cmp(Operat2, Term_list),
%	  nested_pred_memory( _ , _ , _ , _ , _ , Operat2, _ , Nested_name , Vars_list , _ ),
% TODO add levels check
%	  nested_pred_memory( _ , Mlevel , _ , _ , _ , Operat2, _ , Nested_name , Vars_list , _ ),
%	  nested_pred_memory( _ , Mlevel , _ , Level0, Level , Operat2, _ , Nested_name , Vars_list , _ ),
%	  nested_pred_memory( _ , Mlevel , _ , _, Level , Operat2, _ , Nested_name , Vars_list , _ ),
        Msgx0 = string::format(" try find 2 ytyty % % % %  ", Mlevel, Level0, Level, Operat2),
        stdio::write(Msgx0, "\n"),
%	   nested_pred_memory(2,1,0,0,1,"if",3,"if_and_if",["realvar#0","$Cs"],[1,2,2],13,["if","and","if"]).
        % Mlevel is  alreay plus 1 here , so you have to inventarise on the same level
        % do not inventaris the whole clause
        % start earlier with clause part check, and inventarise after that the part you want to transpile
        % TEMPORARY TEST if this works  without Mlevel , set it back afterwards
        nested_pred_memory(_, Mlevel, Level0, Level0, _, Operat2, _, Nested_name, Vars_list, Sub_preds_nums0, _, Sub_preds_names0),
        % Sub_preds_nums = [] ,  	  , _ , Sub_preds_names
        %  nested_pred_memory( _ , _ , Level0 , Level0, _ , Operat2, _ , Nested_name , Vars_list , _ , _ , _ ),
        % stdio::write( " succeed " , "\n" ),
        % Continue with construct clause here
        listi_lstr2_remove_head(Sub_preds_nums0, Sub_preds_names0, Callingnum, Callingpred, Sub_preds_nums3, Sub_preds_names3),
        current_transpile_model_get(Main_term_head_type, _, _, _, _),
        % % current_transpile_model( Sequential_var_names , Subclauses_head_type , Subclauses_body_call_type ) ,
        construct_implement_clause(Pred_count, Main_term_head_type, Callingnum, Callingpred, Varnames_head, Mlevel, Nested_name, Vars_list,
            Sub_preds_nums3, Sub_preds_names3, Clause_implementation, Vars_list3x, Arity_head),
        % stdio::write( "Clause_implementation-- " ) ,
        % toString( Clause_implementation ) , "\n" )
        assert(clause_memory(Pred_count, is_clause_implementation, Arity_head, "Clause_implementation-- ", Nested_name, Clause_implementation)),
%  	   stdio_write_clause( "Clause_implementation-- " , Clause_implementation ) ,
%	    stdio::write( "\n" ) ,
        % RECONS_NEWCLAUSE
% temporay not needed we consult the prolog result
%	   recons_newclause_z( "transpile_complex_term" , Clause_implementation ) ,
        % listi_lstr2_remove_head( Sub_preds_nums0 , Sub_preds_names0, Sub_preds_nums , Sub_preds_names ),
        % max_deep_variabels_nest_connected( Sub_preds_nums , Sub_preds_names , [] , Vars_list_deep ) ,
        % Vars_list2 = list::append( Vars_list , Vars_list_deep ) ,	  Vars_list3x = list::removeDuplicates( Vars_list2 ) ,
        % pxpxpx
        % vars_slist_remove_pred_out_vars
        % current_transpile_model( _ , _ , _  , qqqq ) ,
        % vars_slist_remove_pred_out_vars( _ , Vars_list3x0 , Vars_list3x , _ ) ,
        vars_slist_to_vars_list(Vars_list3x, Var_var_list),
        construct_subclauses(Pred_count, Varnames_head, Sub_preds_nums3, Sub_preds_names3),
        !.

    transpile_default_via_invent(Pred_count, _Mlevel, _Level0, _Level, _Varnms_h, cmp(Operat, Term_list0), cmp("match_prolog", [var(Temp_str)])) :-
        Operat = "sub",
        % TODO  variabel is wrong parsed
        % We could also  unify  with a direct term here, but for the moment we choose to use multiple
        % = constructs , this would/should be easier to read and also easier to modify
        % for the moment it works, ( now we have to  transp with prolog variabels  )
        Term_list0 = [Elem_0],
        Elem_0 = cmp(Operat2, Term_list),
        Operat2 = "match",
        Term_list = [_El1, _El2, El3, _El4],
        El3 = cmp(_Sub, [Match_term_to_execute]),
        Match_term_to_execute = cmp(Db_pred_call, _Match_var_list),
        Temp_str = string::concat(Db_pred_call, "_tmp_va"),
        !.

% inventarise_subcl_metta_tpro_subc2(   [ cmp( Operat,  Atom_List2 ) | Lis ]   ):-	Operat = "sub" , !,
    %   	  try_get_first_predname( Lis, Pname ),	  assert( predicate_nesting( 0, 0, Pname ) ),
    %inventarise_subcl_metta_tpro_subc2( [ Complex_transpiled  | Lis ]  ).
    % assert( predicate_nesting( Level0, Level, Level2 , Pname, Varslist ) ),
%      inventarise_subcl_metta_tpro_subc2(  Level0, Level2, Atom_List2 ) , ! ,
%	  inventarise_subcl_metta_tpro_subc2(  Level0, Level, Lis   ).
% atom(string Value);
    %       int(integer Value);
    %      int64(integer64 Value);
    %     real(real Value);
    %    str(string Value);
% last temp
%inventarise_subcl_metta_tpro_subc2( Level0, Level,  [ var( _ ) | Lis ]  ):-   !,  Level02 = Level0 + 0,
%	inventarise_subcl_metta_tpro_subc2( Level02, Level , Lis  ).
%inventarise_subcl_metta_tpro_subc2( Level0, Level,  [ atom( _ ) | Lis ]  ):-   !,  Level02 = Level0 + 0,
%	inventarise_subcl_metta_tpro_subc2( Level02, Level , Lis  ).
%inventarise_subcl_metta_tpro_subc2( Level0, Level,  [ _Terx | Lis ]  ):-
%  stdio::write( "aa ", toString( _Terx ) , "\n" ), !,  Level02 = Level0 + 1,
%	inventarise_subcl_metta_tpro_subc2( Level02, Level , Lis  ).
    amount_of_linear_nesting([], Res, Res) :-
        !.
    amount_of_linear_nesting([cmp(Operat, Ato_lis) | Lis], Cou, Res) :-
        Operat = "sub",
        !,
        Cou2 = Cou + 1,
        amount_of_linear_nesting(Lis, Cou2, Res),
        !.
%amount_of_linear_nesting( [ cmp( _Operat,   Ato_lis  ) | Lis ] , Cou,  Res ):-      ! , Cou2 = Cou + 0 ,
%    amount_of_linear_nesting(  Ato_lis  , Cou2, Cou3 ),
%	amount_of_linear_nesting(  Lis  , Cou3, Res ),    ! .
    amount_of_linear_nesting([_H | Lis], Cou, Res) :-
        !,
        amount_of_linear_nesting(Lis, Cou, Res),
        !.

%----
    amount_of_sub_nesting2([], Res, Res) :-
        !.
    amount_of_sub_nesting2([cmp(Operat, Ato_lis) | Lis], Cou, Res) :-
        Operat = "sub",
        !,
        Cou2 = Cou + 1,
        amount_of_sub_nesting2(Ato_lis, Cou2, Cou3),
        amount_of_sub_nesting2(Lis, Cou3, Res),
        !.

    amount_of_sub_nesting2([cmp(_Operat, Ato_lis) | Lis], Cou, Res) :-
        !,
        Cou2 = Cou + 0,
        amount_of_sub_nesting2(Ato_lis, Cou2, Cou3),
        amount_of_sub_nesting2(Lis, Cou3, Res),
        !.

    amount_of_sub_nesting2([_H | Lis], Cou, Res) :-
        !,
        amount_of_sub_nesting2(Lis, Cou, Res),
        !.

%----
    arity_of_pred([cmp(Pname, Lis) | _], Res) :-
        !,
        arity_of_list2(Lis, 0, Res).
    arity_of_pred(_, 0) :-
        !.
%---
    arity_of_list2([], Res, Res) :-
        !.
    arity_of_list2([_H | Lis], Cou, Res) :-
        !,
        Cou2 = Cou + 1,
        arity_of_list2(Lis, Cou2, Res),
        !.

% to  examp
% [cmp( "func2" , [ list( var( "H" ) , var( "T" ) ) , var( "W" ) , var( "Q" ) ] ),
%     cmp(",",[cmp("=",[var("A"),cmp("+",[var("W"),var("H")])]),cmp("func2",[var("T"),var("A"),var("Q")])])])
%   cmp(":-",   [
%       cmp("func2",[list(var("H"),var("T")),var("W"),var("Q")]),
%	    cmp(",",[cmp("funcname",[var("A"),var("W"),var("H")]),cmp("func2",[var("T"),var("A"),var("Q")])])
%
%		])
% transpile_metta_sub
% transpile_subcl_metta_tpro_subc2(   [ cmp( Operat,  Atom_List2 )  | Lis ] , [ cmp( "sub_tst", [cmp("aa",Atom_List2)] )  | Result_list ] ):-
%     Atom_List3 = list::append( Atom_List2, [ var("Outvar")] ),
%      Operat = namex( Strname , _ , _ ),      non_nested_atomlist_tsubpro_list( Atom_List2, Atom_List3 ), !,
    % Operat = namex( Strname , _ , _ ), !,
% example
%   (/
%     (* (- 1 $ABs) (- $Cs (* $Bs $BCs))  )
%	  (- 1 $Bs)
%	 )
% cmp( "/" ,
%   [ cmp( "sub" , [
%     cmp( "*", [
%	   cmp( "sub" , [  cmp( "-" , [  real( 1 ) , var( "$ABs" ) ] ) ] ),
%	    cmp( "sub" , [  cmp( "-" ,  [ var( "$Cs" ), cmp( "sub",  [ cmp( "*" , [ var( "$Bs" ), var( "$BCs" ) ] )])   ])]  ) ] ) ] ),
%		 cmp( "sub" , [ cmp( "-" , [ real( 1 ) , var( "$Bs" ) ] ) ] )
%		 ]  )
% rewrite to:
%--
%division19( ABs , Bs , BCs , Cs  , Result ) :-
% multiplication17(  ABs  ,  Cs , Bs , BCs   , Vx17 )  ,
% minus18( number(1) , Bs  , Vx18 )   , ! ,
% Result is Vx17 / Vx18  .
%--
%multiplication17( ABs , Cs , Bs , BCs  , Result ) :-
% minus13( number(1) , ABs  , Vx13 )  ,
% minus16( Cs ,  Bs , BCs   , Vx16 )   , ! ,
% Result is Vx13 * Vx16  .
%minus13( number(Numvarz) , ABs  , Result ) :-
% Vn13 is Numvarz , ! ,  ABs13 is ABs , ! ,
% Result is Vn13 - ABs13  .
%minus16( Cs , Bs , BCs  , Result ) :-
% Cs16 is Cs ,  multiplication15( Bs , BCs  , Vx15 )   , ! ,
% Result is Cs16 - Vx15  .
%minus18( number(Numvarz) , Bs  , Result ) :-
% Vn18 is Numvarz , ! ,  Bs18 is Bs , ! ,
% Result is Vn18 - Bs18  .
%multiplication15( Bs , BCs  , Result ) :-
% Bs15 is Bs ,  BCs15 is BCs , ! ,
% Result is Bs15 * BCs15  .
% DEBUG CLAUSES
% (= (get_cell_state $row $column)
%  (match_sudoku_puzzle_state $row $column )
%  )
    % added
    % var("$row"),var("$column"),var("$quad"),var("$state")
%	  "sudoku_puzzle_state"
% dont / never  catch: transpile_complex_term !!!!
%----
%has_no_sub_nesting2( [] , Res, Res ):-!.
%has_no_sub_nesting2( [ metta_sub(  par_atom_list( _Operatorx , _, _, Ato_lis ) ) | _Lis ] , Cou,  Res ):- !, Cou2 = Cou + 1,
%    has_no_sub_nesting2(  Ato_lis  , Cou2, Res ), !.
%has_no_sub_nesting2( [ _H | Lis ] , Cou, Res ):- !,   has_no_sub_nesting2( Lis  , Cou, Res ),!.
%cmp( "=",[   cmp("sub",  [ cmp("get_cell_state", [ var("$row"), var("$column") ] ) ] ),
    %    cmp( "sub" ,  [ cmp( "match", [ atom( "&" ) , atom( "self" ),
%	   cmp( "sub" , [ cmp( "sudoku_puzzle_state" , [ var("$row"), var("$column"), var("$quad"), var("$state")  ] ) ] ),
%	     var("$state") ] ) ] ) ] )
%cmp("sub_trp00",[cmp("sub_trp",[
% cmp("get_cell_state",[var("$row"),var("$column")])]),
%  cmp("sub_trp2",[
%  cmp("match",
%  [atom("&"),atom("self"),
%   cmp("sub",[cmp("sudoku_puzzle_state",[var("$row"),var("$column"),var("$quad"),var("$state")])]),var("$state")]
%    )
%    ])])
%     try_get_first_predname( Atom_List2, Pname ),	  assert( predicate_nesting( Level, 0, Pname ) ),
    %   try_get_first_predname( Atom_List, Pname ),	  assert( predicate_nesting( Level, 0, Pname ) ),
%                       transpiled  argument                                   transpiled( Count, Sx )
% transpile_subcl_metta_tpro_subc2(   [ cmp( Operat,  Atom_List2 )  | Lis ] , [ cmp( "sub_trp", Atom_List2)  | Result_list ] ):-  Operat = "sub" ,
    % stdio::write("nest level ", toString( Atom_List2 ), " ", toString( Res ), "\n"),
    %transpile_subcl_metta_tpro_subc2(   Atom_List2, Atom_List3 ) , ! ,
%	transpile_subcl_metta_tpro_subc2( Atom_List3 , Result_list ).
    inventarise_subcl_metta_tpro_subc2(_, _, _Level0, _Level, []) :-
        !.

    inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level0, Level, [cmp(Operat, Atom_List2) | Lis]) :-
        Operat = "sub",
        Mlevel2 = Mlevel + 1,
        Level02 = Level0 + 1,
        Level2 = Level + 1,
        asser_match_db_predicate(Cou, Mlevel, Level0, Level, Atom_List2),
        try_get_first_predname(Atom_List2, Pname, Varslist, Posit_list, Sublis),
        arity_of_pred(Atom_List2, Arity_x),
        amount_of_linear_nesting(Sublis, 0, Linear_nest_level),
        amount_of_sub_nesting2(Atom_List2, 0, Nest_level),
%	    change_db_pred_name( Pname, Pname2 ),
%	            asser_pred_nest( Mlevel, Level0, Level, Level2 , Pname, Varslist, Nest_level ),
        assert(predicate_nesting(Mlevel, Level0, Level, Level2, Pname, Varslist, Posit_list, Nest_level, Linear_nest_level, Arity_x)),
        inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level02, Level2, Atom_List2),
        !,
        inventarise_subcl_metta_tpro_subc2(Cou, Mlevel2, Level0, Level, Lis).
% * last vers  best version is  last = 2 and below before last 2
    inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level0, Level, [cmp(Operat, Atom_List2) | Lis]) :-
        Mlevel2 = Mlevel + 1,
        Level02 = Level0 + 0,
        inventarise_subcl_metta_tpro_subc2(Cou, Mlevel2, Level0, Level, Atom_List2),
        !,
        inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level02, Level, Lis).

    inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level0, Level, [cmp(_, _) | Lis]) :-
        !,
        Level02 = Level0 + 0,
        Mlevel2 = Mlevel + 1,
        inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level02, Level, Lis).

    inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level0, Level, [_ | Lis]) :-
        Level02 = Level0 + 0,
        inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level02, Level, Lis).

%------------------------
    inventarise_subcl_metta_tpro_subc(_Cou, _Mlevel, _Level0, _Level, cmp(Operat, Atom_List2)) :-
        Operat = "sub",
        amount_of_sub_nesting2(Atom_List2, 0, Res),
        Res = 0,
        !.

% inventarise_subcl_metta_tpro_subc( Mlevel , Level0, Level, cmp( Operat,  Atom_List )     ):-
%   inventarise_subcl_metta_tpro_subc2( Mlevel , Level0 , Level,  Atom_List  ) , ! .
    inventarise_subcl_metta_tpro_subc(Cou, Mlevel, Level0, Level, cmp(Operat, Atom_List)) :-
        inventarise_subcl_metta_tpro_subc2(Cou, Mlevel, Level0, Level, [cmp(Operat, Atom_List)]),
        !.

    inventarise_subcl_metta_tpro_subc(_, _, _, _Level, _) :-
        !.
%  Res = 0 , ! ,
%---

% condition_if_check_is_equal["$m","predicate_outvar$xm"]-Outvx2
    parse_output_var(Operat, Outvar_str) :-
        Sx = "predicate_outvar$",
        Le = string::length(Sx),
% P = string::search( Operat, Sx ),    string::tryFront( Operat, Le, _Bg , Rs ) , Outvar_str = string::concat( "$mta_outv", Rs ) , ! .
        P = string::search(Operat, Sx),
        string::tryFront(Operat, Le, _Bg, Rs),
        Outvar_str = string::concat("$", Rs),
        !.

%------
    transpile_output_variabels(cmp(Operat, Atom_List2), cmp(Operat, Result_list)) :-
        !,
        transpile_output_variabels2(Atom_List2, Result_list).
%---
    transpile_output_variabels2([], []) :-
        !.
    transpile_output_variabels2([cmp(Operat, Atom_List2) | Lis], [cmp(Operat, Atom_List2) | Result_list]) :-
        Operat = "sub",
        amount_of_sub_nesting2(Atom_List2, 0, Nestlevel),
        Nestlevel = 0,
        !,
        transpile_output_variabels2(Lis, Result_list).

    transpile_output_variabels2([cmp(Operat, Atom_List2) | Lis], Result_list) :-
        Operat = "sub",
        Atom_List2 = [Head | Body],
        Head = cmp(Operat2, Atom_List3),
        parse_output_var(Operat2, Outvar_str),
        !,
        assert(is_metta_output_var(Outvar_str)),
        %
        Atom_List3 = [Head3 | Body3],
        Head3 = cmp(Operat3, Atom_List4),
        Operat3 = "sub",
        Atom_List4 = [Head4 | Body4],
        Head4 = cmp(Operat4, Atom_List5),
        Atom_List6 = list::append(Atom_List5, [var(Operat2)]),
        transpile_output_variabels2([cmp("sub", [cmp(Operat4, Atom_List6)]) | Lis], Result_list).

% transpile_output_variabels2(   [ cmp( Operat,  Atom_List2 ) | Lis ] , Result_list  ):-	Operat = "sub" ,
%    	transpile_output_variabels2(  Atom_List2, Atom_List3 ) , ! ,
%	transpile_output_variabels2(  [ cmp( "sub_trp2_vars", Atom_List3)  | Lis ] , Result_list ).
%	transpile_output_variabels2(  [ cmp( "sub", Atom_List3)  | Lis ] , Result_list ).
    transpile_output_variabels2([cmp(Operat, Atom_List2) | Lis], [cmp(Operat, Atom_List3) | Result_list]) :-
        transpile_output_variabels2(Atom_List2, Atom_List3),
        !,
        transpile_output_variabels2(Lis, Result_list).

    transpile_output_variabels2([H | Lis], [H | Result_list]) :-
        !,
        transpile_output_variabels2(Lis, Result_list).

%--------------
    transpile_subcl_metta_tpro_subc2(_Pc, _, _, _, _, [], []) :-
        !.
    transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level0, Level, Varnmsh, [cmp(Operat, Atom_List2) | Lis], [Transpiled_term | Result_list]) :-
        Operat = "sub",
        Mlevel2 = Mlevel + 1,
        Level02 = Level0 + 1,
        Level2 = Level + 1,
        amount_of_sub_nesting2(Atom_List2, 0, Nestlevel),
        Nestlevel = 0,
        !,
        transpile_simpel_term(Mlevel, Level0, Level, cmp(Operat, Atom_List2), Transpiled_term),
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel2, Level0, Level, Varnmsh, Lis, Result_list).

% add level here  to be able to transpile
    transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level0, Level, Varnmsh, [cmp(Operat, Atom_List2) | Lis], Result_list) :-
        Operat = "sub",
        Mlevel2 = Mlevel + 1,
        Level02 = Level0 + 1,
        Level2 = Level + 1,
        transpile_default_via_invent(Pred_count, Mlevel, Level0, Level, Varnmsh, cmp(Operat, Atom_List2), Complex_transpiled),
        !,
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel2, Level0, Level, Varnmsh, [Complex_transpiled | Lis], Result_list).

    transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level0, Level, Varnmsh, [cmp(Operat, Atom_List2) | Lis], Result_list) :-
        Operat = "sub",
        Mlevel2 = Mlevel + 1,
        Level02 = Level0 + 1,
        Level2 = Level + 1,
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level02, Level2, Varnmsh, Atom_List2, Atom_List3),
        !,
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel2, Level0, Level, Varnmsh, [cmp("sub_trp2", Atom_List3) | Lis], Result_list).

    transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level0, Level, Varnmsh, [cmp(Operat, Atom_List2) | Lis],
            [cmp(Operat, Atom_List3) | Result_list]) :-
        Mlevel2 = Mlevel + 1,
        Level02 = Level0 + 0,
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel2, Level0, Level, Varnmsh, Atom_List2, Atom_List3),
        !,
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level02, Level, Varnmsh, Lis, Result_list).

    transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level0, Level, Varnmsh, [H | Lis], [H | Result_list]) :-
        !,
        Level02 = Level0 + 0,
        Mlevel2 = Mlevel + 1,
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level02, Level, Varnmsh, Lis, Result_list).

%----
    term_add_output_variabel(Head_term_body_0, Outvar_s, cmp(Funcname, Varlist)) :-
        Head_term_body_0 = cmp(Funcname, Varlist_0),
        Varlist = list::append(Varlist_0, [var(Outvar_s)]),
        !.
    term_add_output_variabel(Head_term_body_0, _Va, Head_term_body_0) :-
        !.

%--
% cmp("equal",[cmp("sub",[
%   cmp("execute_sudoku",[])]),
%   cmp("sub",[
%    cmp("let_star",[
%	cmp("sub",[cmp("groupe",[cmp("sub",[cmp("get_row_state",[real(3),var("predicate_outvar$row_sta")])]),
%	cmp("sub",[cmp("println",[var("$row_sta")])]),cmp("sub",[cmp("filter_elems",[var("$row_sta"),var("predicate_outvar$sta2")])]),
%	cmp("sub",[cmp("println",[var("$sta2")])])])]),atom("True")])])])
    is_an_execute_clause(Termx) :-
        Termx = cmp(Operat, Atom_List),
        Operat = "equal",
        Atom_List = [Fhead, _Fbody],
        Fhead = cmp(Operat1, [Funct_implement_head]),
        Operat1 = "sub",
        Funct_implement_head = cmp(Func_name, Varlist_head_0),
        Le = list::length(Varlist_head_0),
% 09:40 8-9-2025
        Le = 0,
%	 Func_name = "execute_sudoku" , ! ,
        Xx = toString(Termx),
        % string::search(x
        _P = string::search(Xx, "\"groupe", string::caseInsensitive()),
        _U = string::search(Xx, "\"let", string::caseInsensitive()),
        !,
        stdio::write("\npp ", Func_name, "is_an_execute_clause\n").
%	  stdio::write( "\n", toString( Termx ) , "\n" ).

%--
    try_set_consult_model(Name) :-
        Fn = string::concatList(["transpile_models\\", Name, ".pro"]),
        file::existExactFile(Fn),
        retractall(current_transpile_model(_, _, _, _, _)),
        try
            file::consult(Fn, current_model)
        catch _ do
            fail
        end try,
        !.

%  file::
% current_transpile_model
% -----
% todo we have to make a predicates to count the length of the list, to be sure
% it works, right now we try to if  the std predicate list::length also works
% DEFAULT
% 09:48 5-9-2025
    set_transpile_model_type(Termxx) :-
        is_an_execute_clause(Termxx),
        stdio::write("is exec clause \n"),
% later we will change this ofcourse if we have to parse all sorts of code
% bigstr::current_example_get( Name ) ,
        try_set_consult_model("execute_clause"),
        stdio::write("is exec clause \n"),
        !.

    set_transpile_model_type(_Termxx) :-
% later we will change this ofcourse if we have to parse all sorts of code
        bigstr::current_example_get(Name),
        try_set_consult_model(Name),
        !.
% Fn = string::concatList([ "transpile_models\\" , Name, ".pro" ] ) ,
% file::existExactFile( Fn ) ,
% retractall( current_transpile_model( _ , _ , _ , _  , _ ) ) ,
% trap(  file::consult( Fn, current_model ),  _, fail ), ! .
%  file::
% current_transpile_model

    set_transpile_model_type(Termx) :-
        Termx = cmp(Operat, Atom_List),
        Operat = "equal",
        Atom_List = [Fhead, _Fbody],
        Fhead = cmp(Operat1, [Funct_implement_head]),
        Operat1 = "sub",
        Funct_implement_head = cmp(Func_name, Varlist_head_0),
        Le = list::length(Varlist_head_0),
        Le = 0,
        !,
        % TEMP test , allways this
% TODO TODO
        % debug_
        retractall(current_transpile_model(_, _, _, _, _)),
        debug_Note("is zero", "zero"),
%  assert( current_transpile_model( 0, preserve_metta_vars , 0 , 0  ) ).
%   dont_allow_output_vars
% question which argument
% LAST  THis is the last
%  assert( current_transpile_model( dont_allow_output_vars, 0 , 0 , 0  ) ).
%  assert( current_transpile_model( dont_allow_output_vars, 0 , 0 ,  preserve_metta_output_var  ) ).
        assert(current_transpile_model(preserve_metta_vars, 0, 0, preserve_metta_output_var, 0)).

%        current_transpile_model_get( Main_term_head_type , Sequential_var_names , Subclauses_head_type , Subclauses_body_call_type ):-
%    assert( current_transpile_model( dont_allow_output_vars, 0 , preserve_metta_output_var , preserve_metta_output_var  ) ).
    % preserve_metta_output_var
% this one should work
%set_transpile_model_type( _Varlist_head_0 ):-   !,
%  retractall( current_transpile_model( _ , _ , _ , _  ) ) ,
%
% assert( current_transpile_model( preserve_metta_output_var, 0 , 0 , 0  ) ).
    % QUESTION is when to use this one
%  assert( current_transpile_model( dont_allow_output_vars, preserve_metta_vars , preserve_metta_output_var , keep_all ) ).
%  assert( current_transpile_model( 0, 0, 0 , dont_allow_output_vars ) ).
% TEST
% this here below was the default   for the rest
    set_transpile_model_type(_Varlist_head_0) :-
        !,
        retractall(current_transpile_model(_, _, _, _, _)),
        assert(current_transpile_model(0, preserve_metta_vars, preserve_metta_output_var, keep_all, 0)).

    save_also_to_file(Pro_name) :-
        _P = string::search(Pro_name, "example", string::caseInsensitive()),
        Fn = string::concatList(["transpile_models\\", Pro_name, ".pro"]),
        file::save(Fn, current_model),
        !,
        vpiCommonDialogs::note("File saved", " in directory transpile_models ").
    save_also_to_file(_Pro_name) :-
        !.

%--
    set_model_profile(Pro_name, Names, Is_che) :-
        Names = [Na1, Na2, Na3, Na4],
        const_name(Nax1, Co_num1),
        Nax1 = Na1,
        const_name(Nax2, Co_num2),
        Nax2 = Na2,
        const_name(Nax3, Co_num3),
        Nax3 = Na3,
        const_name(Nax4, Co_num4),
        Nax4 = Na4,
        !,
        retractall(current_transpile_model(_, _, _, _, _)),
        assert(current_transpile_model(Co_num1, Co_num2, Co_num3, Co_num4, Is_che)),
        save_also_to_file(Pro_name),
        !.
        %Fn = string::concatList([ "transpile_models\\" , Pro_name, ".pro" ] ) ,
        %file::save( Fn, current_model ), ! .
    set_model_profile(_, _, _) :-
        !.
%---

    get_model_profile(Names, Is_che) :-
        % should be the get predicate
        current_transpile_model_get(Co_num1x, Co_num2x, Co_num3x, Co_num4x, Is_che),
        const_name(Nax1, Co_num1),
        Co_num1 = Co_num1x,
        const_name(Nax2, Co_num2),
        Co_num2 = Co_num2x,
        const_name(Nax3, Co_num3),
        Co_num3 = Co_num3x,
        const_name(Nax4, Co_num4),
        Co_num4 = Co_num4x,
        Names = [Nax1, Nax2, Nax3, Nax4],
        !.

%   retractall( current_transpile_model( _ , _ , _ , _ ,_ ) ) ,
%  assert( current_transpile_model( 0, preserve_metta_vars , preserve_metta_output_var , keep_all, 0  ) ).
% DEFAULT !! TODO  connect via file  for each example
    get_model_profile(["transpile_default", "preserve_metta_vars", "preserve_metta_output_var", "keep_all"], transpile_default) :-
        !.

%get_model_profile( [ "" , "" , "" , "" ] , 0 ) :- !.
    get_model_type_names(Names) :-
        Names = [ Na || const_name(Na, _) ],
        !.

    const_name("transpile_default", transpile_default).
    const_name("preserve_metta_vars", preserve_metta_vars).
    const_name("keep_all", keep_all).
    const_name("remove_metta_outpvar_and_created", remove_metta_outpvar_and_created).
    const_name("preserve_metta_output_var", preserve_metta_output_var).
    const_name("dont_allow_output_vars ", dont_allow_output_vars).

%---
    which_depth_to_use(_Llex, Deepnumber, Deepnumber) :-
        current_transpile_model_get(_, _, _, _, Use_depth),
        Use_depth = 1,
        !.
    which_depth_to_use(Llex, _Deepnumber, Llex) :-
        !.

%---
% cmp("sub",[
%  cmp("loop_sudoku_until_complete",[
%   cmp("sub",[
%    cmp("coninu",[var("$yesno")])])])]),
% ---
% trans to:  cmp("add_xnum",[
% cmp("cell_xstate",[var("R"),var("C"),var("Sta")]),var("Outx")]),
% cmp("equal",[
% ** cmp("sub",[
%  cmp("init_get_candidates",[var("$col")])]),
%  cmp("sub",[cmp("match_all",[atom("&self"),cmp("sub",[cmp("sudoku_puzzle_state",[var("$row"),var("$col"),var("$quad"),var("$state")])]),cmp("sub",[cmp("cell_candidatex",[var("$quad"),var("$state")])])])])])
%--
% []
    % vpiCommondialogs::note("nesthead", toString( Varlist_head ) ),
%	 Head_term_head_0 = cmp( Subfunct , Varlist_head ) ,
    % vpiCommondialogs::note("nesthead", toString( Head_term_head_0 ) ),
    % term_add_output_variabel( Head_term_head_0 , "Outp0" , Head_term_head ) ,
    % vpiCommondialogs::note("nesthead2", toString( Head_term_head ) ).
% cmp("sub",[
% cmp("apply_candidate",[
    % cmp("sub",[cmp("sudoku_candidate_cell",[var("$r"),var("$c")])]),
    %  var("$val")])])
    transpile_head_term(Fhead, cmp(Func_name, Head_term_head_0), Func_name, Varnames_head2, Arity0) :-
%  stdio::write( "headt1 ", toString( Fhead ) , "\n" ) ,
        Fhead = cmp(Operat1, [Funct_implement_head]),
        Operat1 = "sub",
        Funct_implement_head = cmp(Func_name, [Term_head0 | Restvars_0]),
        Term_head0 = cmp(Subnest, [Term_head2]),
        Subnest = "sub",
        Term_head2 = cmp(Subfunct, Varlist_head_0),
        vars_list_to_varnames_slist(Varlist_head_0, Varnames_head, _),
        vars_list_to_varnames_slist(Restvars_0, Restvars, _),
        Varnames_head2 = list::append(Varnames_head, Restvars),
        metta_vars_list_to_prolog_vars_list(Varlist_head_0, Varlist_head),
        metta_vars_list_to_prolog_vars_list(Restvars_0, Restlist_head),
        Lisx1 = list::append([cmp(Subfunct, Varlist_head)], Restlist_head),
        Lisx2 = list::append(Lisx1, [var("Outp0")]),
        arity_of_head_vars(Lisx2, Arity0),
%	 Head_term_head_0 = [cmp( Subfunct , Varlist_head ) ,  var("Outp0")	] ,
        Head_term_head_0 = Lisx2,
        !.

    transpile_head_term(Fhead, Head_term_head, Func_name, Varnames_head, Arity2) :-
%  stdio::write( "headt2 ", toString( Fhead ) , "\n" ) ,
        Fhead = cmp(Operat1, [Funct_implement_head]),
        Operat1 = "sub",
        Funct_implement_head = cmp(Func_name, Varlist_head_0),
        vars_list_to_varnames_slist(Varlist_head_0, Varnames_head, _),
% * xx
        metta_vars_list_to_prolog_vars_list(Varlist_head_0, Varlist_head),
        arity_of_head_vars(Varlist_head, Arity0),
        Arity2 = Arity0 + 1,
        Head_term_head_0 = cmp(Func_name, Varlist_head),
        term_add_output_variabel(Head_term_head_0, "Outp0", Head_term_head),
        !.

% temp default
    transpile_head_term(Fhead, Fhead, "catchfunctname", [], 0) :-
        !.

%---
    current_transpile_model_get(Main_term_head_type, Sequential_var_names, Subclauses_head_type, Subclauses_body_call_type, Maxdepth) :-
        current_transpile_model(Main_term_head_type, Sequential_var_names, Subclauses_head_type, Subclauses_body_call_type, Maxdepth),
        !.
    current_transpile_model_get(transpile_default, preserve_metta_vars, preserve_metta_output_var, keep_all, transpile_default) :-
        !.

% [ "is_zero", "preserve_metta_vars" , "preserve_metta_output_var" , "keep_all", "is_zero" ] , 0
%------------------------
%  cmp("assert",[cmp("sudoku_puzzle_state",[int(1),int(1),int(1),int(5)])])
% Temporarly WITHOUT assert , interpreters seems not to handle the assert yet
%  cmp("sudoku_number",[int(1)])
% data clauses
%transpile_subcl_metta_tpro_subc( _Mlevel, _Level0 , _Level,  cmp( Operat ,  Atom_List2 )  ,  cmp( "assert" , [ cmp( Operat ,  Atom_List2 ) ] )  ):-
%     Operat <> "sub",	 amount_of_sub_nesting2(  Atom_List2  , 0, Res ), Res = 0 , 	 ! .
%
% init_get_candidates( Outp0 )
%is_database_pred = 402.
%is_complex_pred = 403.
%is_undefined_pred = 403.
%cmp("equal",[cmp("sub",[cmp("count_list",[atom("empty_atom")])]),real(0)])
% Adhoc ( maybe ) or not adhoc
%cmp("equal",[cmp("sub",[cmp("count_list",[atom("empty_atom")])]),real(0)])
% 10:04 21-7-2025
% adhoc countlist catch clause implemented , probably not neces anymore
% is_database_pred is false here i think todo
    transpile_subcl_metta_tpro_subc(_Count, _Mlevel, _Level0, _Level, cmp(Operat, Atom_List2), Trans_clause, is_clause_implementation, Func2, 2) :-
        Operat = "equal",
        % cmp("count_list",[atom("empty_atom")])
        Atom_List2 = [El1, El2],
        El1 = cmp(Op2, El1_list),
        El1_list = [El1_list_el1],
        El1_list_el1 = cmp(Func2, El1_list_el1_lis),
        El1_list_el1_lis = [El1_list_el1_lis_el1],
        El1_list_el1_lis_el1 = atom(Atom_name),
        Atom_name = "empty_atom",
        El2 = real(Realnum),
        !,
        Trans_clause = cmp(":-", [cmp(Func2, [nill, int(0)]), atom("!")]).

% adhoc containslist catch clause implemented , probably not neces anymore
    transpile_subcl_metta_tpro_subc(_Count, _Mlevel, _Level0, _Level, cmp(Operat, Atom_List2), Trans_clause, is_clause_implementation, Func2, 3) :-
        Operat = "equal",
        % cmp("count_list",[atom("empty_atom")])
        Atom_List2 = [El1, El2],
        El1 = cmp(Op2, El1_list),
        El1_list = [El1_list_el1],
        El1_list_el1 = cmp(Func2, El1_list_el1_lis),
        El1_list_el1_lis = [El1_list_el1_lis_el1, El1_list_el1_lis_el2],
        El1_list_el1_lis_el1 = atom(Atom_name),
        Atom_name = "empty_atom",
        El1_list_el1_lis_el2 = var(Varname),
        El2 = atom(Atname),
        !,
        % contains_symbol( [] , _ , "False" ):- !.
%   vpiCommondialogs::note("succeed parse containslist" , Func2 ) ,
        Trans_clause = cmp(":-", [cmp(Func2, [nill, var("_"), atom("False")]), atom("!")]).

% cmp("equal",[ cmp("sub",[cmp("contains_symbol",[  atom("empty_atom") , var("$sym") ] ) ] ) ,
%  atom("False")     ]   )
%   Operat <> "sub",	 amount_of_sub_nesting2(  Atom_List2  , 0, Res ), Res = 0 , 	 ! .
% doubt
    transpile_subcl_metta_tpro_subc(_Count, _Mlevel, _Level0, _Level, cmp(Operat, Atom_List2), cmp(Operat, Atom_List2), is_database_pred, Operat,
            Arity) :-
        arity_of_head_vars(Atom_List2, Arity),
        Operat <> "sub",
        amount_of_sub_nesting2(Atom_List2, 0, Res),
        Res = 0,
        !.

    transpile_subcl_metta_tpro_subc(_Count, _Mlevel, _Level0, _Level, cmp(Operat, Atom_List2), cmp("sub_trp0", Atom_List2), is_database_pred,
            "sub_trp0", Arity) :-
        Operat = "sub",
        arity_of_head_vars(Atom_List2, Arity),
        amount_of_sub_nesting2(Atom_List2, 0, Res),
        Res = 0,
        !.
        % RECONS_NEWCLAUSE
%	  recons_newclause( Clause_implementation ) ,

    %current_transpile_model( 0, preserve_metta_vars , preserve_metta_output_var , keep_all  ):- !.
%current_transpile_model( 0, preserve_metta_vars , 0 , 0  ):- !.
% "predicate_outvar"
% Head_term_head
%  transpile_subcl_metta_tpro_subc( Mlevel , Level0 , Level , cmp( Operat,  Atom_List )  ,  cmp( ":-" , [ cmp( Func_name , Varlist_head ) ,  Head_term_body  ] )  ):-
    transpile_subcl_metta_tpro_subc(Pred_count, Mlevel, Level0, Level, cmp(Operat, Atom_List), cmp(":-", [Head_term_head, Head_term_body]),
            is_complex_pred, Funcname_head, ArityX) :-
        Operat = "equal",
        Atom_List = [Fhead, Fbody],
        % here todo  get variabels  in the head  for max nest1 because in the head we assume that it is never deeper
        % set_transpile_model_type( Varlist_head_0 ),
        % ok todo , in Varlist_head_0  there can  exist a  term , ( auto parsed inside a "sub" )
        %  , so / and, it must be placed  in prolog format inside: Varlist_head , that means, it should end up there
        %  vars_list_to_vars_list must be placed inside the new  prdicate
        % and later check if  term_add_output_variabel also works for that
        % Then,, also,, in the routines that write-out the prolog versions , make sure that it is correctly
        % written in the head
        transpile_head_term(Fhead, Head_term_head, Funcname_head, Var_names_head, ArityX),
%	 	 arity_of_head_vars( Var_names_head , Arity ) ,
        %Fhead = cmp( Operat1 , [ Funct_implement_head ] ) ,	 Operat1 = "sub" ,
        %Funct_implement_head = cmp( Func_name , Varlist_head_0 ) ,
        % vars_list_to_vars_list( Varlist_head_0 , Varlist_head ) ,
        % Head_term_head_0 = cmp( Func_name , Varlist_head ) ,	  term_add_output_variabel( Head_term_head_0 , "Outp0" , Head_term_head ) ,
        Fbody = cmp(Operat_body, Bodylist),
        Operat_body = "sub",
        inventarise_metta_sub(Pred_count, Mlevel, Level0, Level, cmp(Operat_body, Bodylist)),
        % inventarise_metta_sub( Mlevel , Level0 , Level,  cmp( Operat,  Atom_List )   ),
        % varnames must be given to  this subc2 i think
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level0, Level, Var_names_head, [cmp(Operat_body, Bodylist)], Bodylist_trans_piled),
        % it should basically be always be 1 term
        get_head_term_from_list(Bodylist_trans_piled, Head_term_body_0),
        term_add_output_variabel(Head_term_body_0, "Outp0", Head_term_body),
        !.

%  transpile_subcl_metta_tpro_subc( Mlevel , Level0 , Level , cmp( Operat,  Atom_List )  ,  cmp( "func_implement" , [] )  ):-
%    Operat = "equal" , Atom_List = [ El1 , El2 ] ,  ! .
%   transpile_subcl_metta_tpro_subc2(  Mlevel , Level0 , Level ,  Atom_List , Atom_List2 ).
%  cmp("assert",[cmp("sudoku_puzzle_state",[int(1),int(1),int(1),int(5)])])
    transpile_subcl_metta_tpro_subc(Pred_count, Mlevel, Level0, Level, cmp(Operat, Atom_List), cmp("sub_trp00", [cmp(Operat, Atom_List2)]),
            is_undefined_pred, "sub_trp00", 0) :-
        vars_list_to_varnames_slist(Atom_List, Varnames_head, _),
        transpile_subcl_metta_tpro_subc2(Pred_count, Mlevel, Level0, Level, Varnames_head, Atom_List, Atom_List2).

%transpile_metta( Is_perform, par_atom_list( Opx , Bg, End, Atom_list ), par_atom_list( Opx , Bg, End, Atom_list ) ):-
%  has_no_sub_nesting2( Atom_list , 0, Res ), Res = 0, !.
%transpile_metta( Is_perform, par_atom_list( Opx , Bg, End, Atom_list ), par_atom_list( Opx , Bg, End, Atom_List2 ) ):-
%  transpile_metta2( 0, 0, 0, Is_perform , 0, Atom_list , Atom_List2 ) .
end implement pie
