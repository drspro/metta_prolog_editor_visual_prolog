% Copyright PDC

namespace pfc\pie

implement stdPredicate
    open pie, std

constants
    pieDlgTitle = "Prolog Inference Engine".

domains
    invokerTable = redBlackTree::tree{string Name, core::tuple{integer Arity, invokePredicate Pred}}.

class facts
    invokerTable : invokerTable := initInvokerTable().

class predicates
    initInvokerTable : () -> invokerTable InvokerTable.
clauses
    initInvokerTable() = invokerTable :-
        invokerTable := redBlackTree::empty(),
        foreach std(Name, Arity, Invoker, _) do
            invokerTable := redBlackTree::insert(invokerTable, Name, core::tuple(Arity, Invoker))
        end foreach.

clauses
    isStdFunction("format", Arity) = format_1n_r :-
        Arity > 0.

clauses
    isStdPredicate(Name, CallArity) = Invoker :-
        core::tuple(PredArity, Invoker) = redBlackTree::lookUp_nd(invokerTable, Name),
        arityMatch(CallArity, PredArity),
        !.

class predicates
    arityMatch : (integer CallArity, integer PredArity) determ.
clauses
    arityMatch(A, A) :-
        !.
    arityMatch(_, -1) :-
        !.
    arityMatch(A, Minus) :-
        Minus < -1,
        A >= -Minus - 1,
        !.

clauses
    all() = list::removeDuplicates([ P || std(P, _, _, _) ]).

clauses
    getPrototype(N) = P :-
        std(N, _, _, P),
        !.

    getPrototype(N) = _ :-
        exception::raise_errorf("No prototype for '%'", N).

class predicates
    std : (string Name, integer Arity, invokePredicate Invoker, string Prototype) nondeterm (i,o,o,o) multi (o,o,o,o).
clauses
    std("arg", 3, arg_3, "arg(N, Term, A)").
    std("char_int", 2, char_int_2, "char_int(Char, Int)").
    std("concat", 3, concat_3, "concat(A, B, AB)").
    std("consult", 1, consult_1, "consult(Filename)").
    std("date", 3, date_3, "date(Year, Month, Day)").
    std("directories", 2, directories_2, "directories(Dir, SubDir)").
    std("enquote_str_2", 2, enquote_str_2, "enquote_str(String, Enquoted)").
    std("eof", 0, eof_0, "eof").
    std("fail", 0, fail_0, "fail").
    std("filedetails", 9, filedetails_9, "filedetails(Filename, Attr, Hour, Min, Sec, Year, Month, Day, Size)").
    std("filenameext", 3, filenameext_3, "filenameext(Full, First, Ext)").
    std("filenamepath", 3, filenamepath_3, "filenamepath(Full, Path, File)").
    std("files", 2, files_2, "files(Dir, File)").
    std("for", 3, for_3, "for(Index, From, To)").
    std("back_for", 3, back_for_3, "back_for(Index, From, To)").
    std("fronttoken", 3, fronttoken_3, "fronttoken(All, Front, Rest)").
    std("functor", 3, functor_3, "functor(Term, Func, Arity)").
    std("halt", 0, halt_0, "halt").
    std("integer", 1, integer_1, "integer(X)").
    std("is", 2, is_2, "X is Y").
    std("list", 0, list_0, "list").
    std("list", 1, list_1, "list(Predicate)").
    std("list", 2, list_2, "list(Predicate, Arity)").
    std("nl", 0, nl_0, "nl").
    std("nonvar", 1, nonvar_1, "nonvar(X)").
    std("notrace", 0, notrace_0, "notrace").
    std("op", 3, op_3, "op(Prior, Assoc, Func)").
    std("reconsult", 1, reconsult_1, "reconsult(Filename)").
    std("repeat", 0, repeat_0, "repeat").
    std("retract", 1, retract_1, "retract(Term)").
    std("save", 1, save_1, "save(Filename)").
    std("see", 1, see_1, "see(Filename)").
    std("seeing", 1, seeing_1, "seeing(Filename)").
    std("seen", 0, seen_0, "seen").
    std("str_atom", 2, str_atom_2, "str_atom(Str, Atom)").
    std("str_int", 2, str_int_2, "str_int(Str, Int)").
    std("str_toklist", 2, str_toklist_2, "str_toklist(Str, TokList)").
    std("tell", 1, tell_1, "tell(Filename)").
    std("telling", 1, telling_1, "telling(Filename)").
    std("time", 4, time_4, "time(Hours, Minutes, Seconds, Hundreths)").
    std("told", 0, told_0, "told").
    std("trace", 0, trace_0, "trace").
    std("var", 1, var_1, "var(X)").
    std("display", -1, display_n, "display(Terms)").
    std("write", -1, write_n, "write(Terms)").
    std("=..", 2, op_univ, "Term =.. List").
    std("==", 2, op_eeq, "A == B").
    std(@"\==", 2, op_neeq, @"A \== B").
    std("=", 2, op_eq, "A == B").
    std(":=", 2, op_eq, "A == B").
    std(@"\=", 2, op_neq, @"A \= B").
    std("dlg_ask", 3, dlg_ask, "dlg_ask(Prompt, ButtonList, Result)").
    std("dlg_note", 2, dlg_note, "dlg_note(Title, Message)").
    std("dlg_str", 3, dlg_str, "dlg_str(Prompt, InitStr, Result)").
    std("dlg_term", 1, dlg_term, "dlg_term(Term)").
    std("dlg_openfilename", 4, dlg_openfilename, "dlg_openfilename(Title, FileType, FileExt, Filename)").
    std("dlg_savefilename", 4, dlg_savefilename, "dlg_savefilename(Title, FileType, FileExt, Filename)").

clauses
    op_univ(Pie, _Name, [cmp(ID, TermL), Term]) :-
        !,
        list_terml(List, TermL),
        Pie:unify(Term, list(atom(ID), List)).

    op_univ(Pie, _Name, [Term, list(atom(ID), LIST)]) :-
        list_terml(LIST, TermL),
        Pie:unify(Term, cmp(ID, TermL)).

clauses
    op_eeq(_Pie, _Name, [T1, T2]) :-
        eeq(T1, T2).

clauses
    op_neeq(_Pie, _Name, [T1, T2]) :-
        not(eeq(T1, T2)).

clauses
    op_eq(Pie, _Name, [T1, T2]) :-
        !,
        Pie:unify(T1, T2).

clauses
    op_neq(Pie, _Name, [T1, T2]) :-
        !,
        not(Pie:unify(T1, T2)).

class predicates
    arg_3 : invokePredicate.
    char_int_2 : invokePredicate.
    concat_3 : invokePredicate.
    consult_1 : invokePredicate.
    date_3 : invokePredicate.
    directories_2 : invokePredicate.
    display_n : invokePredicate.
    enquote_str_2 : invokePredicate.
    eof_0 : invokePredicate.
    fail_0 : invokePredicate.
    filedetails_9 : invokePredicate.
    filenameext_3 : invokePredicate.
    filenamepath_3 : invokePredicate.
    files_2 : invokePredicate.
    for_3 : invokePredicate.
    back_for_3 : invokePredicate.
    fronttoken_3 : invokePredicate.
    functor_3 : invokePredicate.
    halt_0 : invokePredicate.
    integer_1 : invokePredicate.
    is_2 : invokePredicate.
    list_0 : invokePredicate.
    list_1 : invokePredicate.
    list_2 : invokePredicate.
    nl_0 : invokePredicate.
    nonvar_1 : invokePredicate.
    notrace_0 : invokePredicate.
    op_3 : invokePredicate.
    reconsult_1 : invokePredicate.
    repeat_0 : invokePredicate.
    retract_1 : invokePredicate.
    save_1 : invokePredicate.
    see_1 : invokePredicate.
    seeing_1 : invokePredicate.
    seen_0 : invokePredicate.
    str_atom_2 : invokePredicate.
    str_int_2 : invokePredicate.
    str_toklist_2 : invokePredicate.
    tell_1 : invokePredicate.
    telling_1 : invokePredicate.
    time_4 : invokePredicate.
    told_0 : invokePredicate.
    trace_0 : invokePredicate.
    var_1 : invokePredicate.
    write_n : invokePredicate.
    op_univ : invokePredicate.
    op_eeq : invokePredicate.
    op_neeq : invokePredicate.
    op_eq : invokePredicate.
    op_neq : invokePredicate.
    dlg_ask : invokePredicate.
    dlg_note : invokePredicate.
    dlg_str : invokePredicate.
    dlg_term : invokePredicate.
    dlg_openfilename : invokePredicate.
    dlg_savefilename : invokePredicate.

class predicates
    format_1n_r : invokeFunction.
clauses
    arg_3(Pie, _Name, [int(N), cmp(_, TermL), R]) :-
        N > 0,
        N <= list::length(TermL),
        Pie:unify(R, list::nth(N - 1, TermL)).

clauses
    fail_0(_Pie, _Name, []) :-
        fail.

clauses
    repeat_0(Pie, _Name, []) :-
        std::repeat(),
        Pie:backtrackpoint().

clauses
    char_int_2(Pie, _Name, [char(CH), T]) :-
        !,
        INT = convert(integer, string::getCharValue(CH)),
        Pie:unify(T, int(INT)).

    char_int_2(Pie, _Name, [T, int(INT)]) :-
        CH = string::getCharFromValue(tryConvert(core::unsigned16, INT)),
        Pie:unify(T, char(CH)).

clauses
    concat_3(Pie, _Name, [str(A), str(B), TC]) :-
        !,
        C = string::concat(A, B),
        Pie:unify(TC, str(C)).

    concat_3(Pie, _Name, [str(A), TB, str(C)]) :-
        !,
        CH0 = string::search(C, A),
        CH1 = string::length(A),
        B = string::fromPosition(C, CH0 + CH1),
        Pie:unify(TB, str(B)).

    concat_3(Pie, _Name, [TA, str(B), str(C)]) :-
        CH = string::searchLast(C, B),
        string::front(C, CH, A, _),
        Pie:unify(TA, str(A)).

clauses
    consult_1(Pie, _Name, [Term]) :-
        !,
        getfilename(Term, Filename),
        Pie:consult_p(Filename).

class predicates
    getfilename : (term Term, string Filename [out]) determ.
clauses
    getfilename(atom(S), Filename) :-
        !,
        Filename = string::concat(S, ".pro").

    getfilename(str(Filename), Filename).

clauses
    date_3(Pie, _Name, [TY, TM, TD]) :-
        !,
        Time = time::now(),
        Time:getDate(Y, M, D),
        Pie:unify(TY, int(Y)),
        Pie:unify(TM, int(M)),
        Pie:unify(TD, int(D)).

clauses
    directories_2(Pie, _Name, [str(Root), DirList]) :-
        !,
        Slist = [ X || X = filename::getNameWithExtension(directory::getSubDirectories_nd(Root)) ],
        Temp = stringlist_to_term(list::sort(Slist)),
        Pie:unify(DirList, Temp).

clauses
    dlg_ask(Pie, _Name, [str(Prompt), ButtonList, Result]) :-
        !,
        Title = "Prolog Inference Engine",
        Pie:unify(Result, int(vpiCommonDialogs::ask(Title, Prompt, term_to_slist(ButtonList)))).

clauses
    dlg_note(_Pie, _Name, [str(Title), str(Message)]) :-
        !,
        vpiCommonDialogs::note(Title, Message).

clauses
    dlg_str(Pie, _Name, [str(Prompt), str(InitStr), Result]) :-
        !,
        Pie:unify(Result, str(vpiCommonDialogs::getString(pieDlgTitle, Prompt, InitStr))).

clauses
    dlg_term(Pie, _Name, [Term]) :-
        !,
        Sterm = Pie:parse(vpiCommonDialogs::getString(pieDlgTitle, "Enter term:", "")),
        Env = environment2::new(),
        Pie:unify(Term, Env:mk_term(Sterm)).

clauses
    dlg_openfilename(Pie, _Name, [str(Title), str(FileType), str(FileExt), Filename]) :-
        !,
        CurrentDirectory = directory::getCurrentDirectory(),
        String = vpiCommonDialogs::getFilename(FileExt, [FileType, FileExt], Title, [], CurrentDirectory, _),
        file::existExactFile(String),
        Pie:unify(Filename, str(String)).

clauses
    dlg_savefilename(Pie, _Name, [str(Title), str(FileType), str(FileExt), Filename]) :-
        !,
        CurrentDirectory = directory::getCurrentDirectory(),
        String = vpiCommonDialogs::getFilename(FileExt, [FileType, FileExt], Title, [vpiDomains::dlgfn_save], CurrentDirectory, _),
        not(file::existExactFile(String)),
        file::writeString(String, ""),
        Pie:unify(Filename, str(String)).

class predicates
    stringlist_to_term : (string* List) -> term Term.
clauses
    stringlist_to_term([]) = nill.
    stringlist_to_term([H | Tail]) = list(str(H), stringlist_to_term(Tail)).

clauses
    display_n(Pie, _Name, TermL) :-
        !,
        Pie:writeterml(display, TermL).

clauses
    enquote_str_2(Pie, _Name, [str(STR), T]) :-
        !,
        MSG = string::format('"%s"', STR),
        Pie:unify(T, str(MSG)).

clauses
    eof_0(_Pie, _Name, []) :-
        !,
        tellSeeSupport::handle_eof.

clauses
    filedetails_9(Pie, _Name, [str(File), TAttr, THour, TMin, TSec, TYear, TMonth, TDay, TSize]) :-
        !,
        file::getFileProperties(File, Attr, Size, _, _, LastChange),
        Time = time::newFromGMT(LastChange),
        Time:getDate(Year, Month, Day),
        Time:getTime(Hour, Min, Sec),
        Pie:unify(TAttr, int(uncheckedConvert(integer, fileSystem_api::toApiAttributes(Attr)))),
        Pie:unify(THour, int(Hour)),
        Pie:unify(TMin, int(Min)),
        Pie:unify(TSec, int(Sec)),
        Pie:unify(TYear, int(Year)),
        Pie:unify(TMonth, int(Month)),
        Pie:unify(TDay, int(Day)),
        Pie:unify(TSize, int(convert(integer, Size))).

clauses
    filenameext_3(Pie, _Name, [str(Filename), Name, EXT]) :-
        !,
        EXT1 = filename::getExtension(Filename, Name1),
        Pie:unify(Name, str(Name1)),
        Pie:unify(EXT, str(EXT1)).

    filenameext_3(Pie, _Name, [var(FilenameVar), str(Name), str(EXT)]) :-
        !,
        TEMP = string::concat(Name, ".", EXT),
        Pie:unify(var(FilenameVar), str(TEMP)).

clauses
    filenamepath_3(Pie, _Name, [str(Filename), TPath, TFile]) :-
        !,
        filename::getPathAndName(Filename, Path1, File),
        PLen = string::length(Path1),
        PLen > 0,
        Len = PLen - 1,
        string::front(Path1, Len, Path2, _),
        Pie:unify(TPath, str(Path2)),
        Pie:unify(TFile, str(File)).

    filenamepath_3(Pie, _Name, [T, str(Path), str(File)]) :-
        !,
        TEMP = string::concat(Path, @"\", File),
        Pie:unify(T, str(TEMP)).

clauses
    files_2(Pie, _Name, [str(Wild), FileLIST]) :-
        !,
        filename::getPathAndName(Wild, Path1, File),
        Path = if Path1 = "" then directory::getCurrentDirectory() else Path1 end if,
        Slist = [ X || X = filename::getNameWithExtension(directory::getFilesInDirectory_nd(Path, File)) ],
        Temp = stringlist_to_term(Slist),
        Pie:unify(FileList, Temp).

clauses
    for_3(Pie, _Name, [T, int(FROM), int(TO)]) :-
        !,
        I = fromTo(FROM, TO),
        Pie:backtrackpoint(),
        Pie:unify(T, int(I)).

class predicates
    tryTerm2any : (term Term) -> any Any determ.
clauses
    tryTerm2any(str(X)) = toAny(X) :-
        !.
    tryTerm2any(int(X)) = toAny(X) :-
        !.
    tryTerm2any(int64(X)) = toAny(X) :-
        !.
    tryTerm2any(char(X)) = toAny(X) :-
        !.
    tryTerm2any(real(X)) = toAny(X) :-
        !.
    tryTerm2any(pointer(X)) = toAny(X) :-
        !.

clauses
    format_1n_r(_Pie, _Name, [str(FormatString) | Ellipsys]) = str(Result) :-
        !,
        try
            AnyList =
                [ A ||
                    V in Ellipsys,
                    A = tryTerm2any(V) otherwise toAny("?")
                ],
            Result = string::format(FormatString, toEllipsis(AnyList))
        catch _TraceId do
            Result = FormatString
        end try.

    format_1n_r(_Pie, Name, _Args) = str(Name).

clauses
    back_for_3(Pie, _Name, [T, int(FROM), int(TO)]) :-
        !,
        I = std::downTo(TO, FROM),
        Pie:backtrackpoint(),
        Pie:unify(T, int(I)).

clauses
    fronttoken_3(Pie, _Name, [str(IN), TFront, TRest]) :-
        !,
        string::fronttoken(IN, Front, Rest),
        Pie:unify(TFront, str(Front)),
        Pie:unify(TRest, str(Rest)).

    fronttoken_3(Pie, _Name, [T, str(Front), str(Rest)]) :-
        IN = string::concat(Front, Rest),
        Pie:unify(T, str(IN)).

clauses
    functor_3(Pie, _Name, [var(Variable), atom(IdTerm), NTerm]) :-
        N = Pie:evalInt(NTerm),
        TermL =
            [ X ||
                _ = std::fromTo(1, N),
                X = variable::newAnonymous()
            ],
        Pie:unify(var(Variable), cmp(IdTerm, TermL)).

    functor_3(Pie, _Name, [cmp(ID, TermL), IdTerm, NTerm]) :-
        N = list::length(TermL),
        Pie:unify(atom(ID), IdTerm),
        Pie:unify(int(N), NTerm).

    functor_3(Pie, _Name, [atom(S), F, NTerm]) :-
        Pie:unify(atom(S), F),
        Pie:unify(int(0), NTerm).

clauses
    halt_0(Pie, _Name, []) :-
        Pie:outputStream:write("Execution terminated"),
        Pie:outputStream:nl(),
        Pie:halt_p().

clauses
    integer_1(_Pie, _Name, [T]) :-
        T = int(_).

clauses
    is_2(Pie, _Name, [R, T2]) :-
        Pie:unify(R, Pie:evalTerm(T2)).

clauses
    list_0(Pie, _Name, []) :-
        Pie:list_p().

clauses
    list_1(Pie, _Name, [str(Name)]) :-
        Pie:list_p(Name).

clauses
    list_2(Pie, _Name, [str(Name), int(Arity)]) :-
        Pie:list_p(Name, Arity).

clauses
    nl_0(Pie, _Name, []) :-
        Pie:outputStream:nl().

clauses
    nonvar_1(_Pie, _Name, [Term]) :-
        boundTerm(Term).

clauses
    notrace_0(Pie, _Name, []) :-
        Pie:traceOn := false.

clauses
    op_3(_Pie, _Name, [int(Prior), atom(Assoc), atom(Op)]) :-
        !,
        operator::setOp(Prior, Assoc, Op).

    op_3(Pie, _Name, [Prior, Assoc, Op]) :-
        operator::getop_nd(P1, A1, O1),
        Pie:backtrackpoint(),
        Pie:unify(Prior, int(P1)),
        Pie:unify(Assoc, atom(A1)),
        Pie:unify(Op, atom(O1)).

clauses
    reconsult_1(Pie, _Name, [Term]) :-
        getfilename(Term, Filename),
        Pie:reconsult_p(Filename).

clauses
    retract_1(Pie, _Name, [Term]) :-
        Pie:retract_p(Term).
        % Pie:backtrackpoint called in handle_retract

clauses
    save_1(Pie, _Name, [Term]) :-
        getfilename(Term, Filename),
        not(file::isReadOnly(Filename)),
        Pie:save_p(Filename).

clauses
    see_1(_Pie, _Name, [str(Filename)]) :-
        if file::existExactFile(Filename) then
            tellSeeSupport::see(Filename)
        end if.

clauses
    seeing_1(Pie, _Name, [T]) :-
        tellSeeSupport::seeingP(Filename),
        Pie:unify(T, str(Filename)).

clauses
    seen_0(_Pie, _Name, []) :-
        tellSeeSupport::seen.

clauses
    str_atom_2(Pie, _Name, [str(STR), T]) :-
        !,
        string::length(STR) > 0,
        string::front(STR, 1, First, _),
        string::isLowerCase(First),
        Pie:unify(T, atom(string::toLowerCase(STR))).

    str_atom_2(Pie, _Name, [T, atom(STR)]) :-
        Pie:unify(T, str(STR)).

clauses
    str_int_2(Pie, _Name, [str(STR), TI]) :-
        !,
        Int = if I32 = tryToTerm(integer, STR) then int(I32) else int64(tryToTerm(integer64, STR)) end if,
        Pie:unify(TI, Int).

    str_int_2(Pie, _Name, [TS, int(I)]) :-
        STR = toString(I),
        Pie:unify(TS, str(STR)).

clauses
    str_toklist_2(Pie, _Name, [str(String), var(TokListVar)]) :-
        !,
        TokL = string_slist(String),
        TokList = stringlist_to_term(TokL),
        Pie:unify(var(TokListVar), TokList).

    str_toklist_2(Pie, _Name, [var(StringVar), Term]) :-
        not(var(_) = Term),
        SList = term_to_slist(Term),
        String = string::concatList(Slist),
        Pie:unify(var(StringVar), str(String)).

clauses
    tell_1(_Pie, _Name, [str(Filename)]) :-
        not(file::isReadOnly(Filename)),
        tellSeeSupport::tell(Filename).

clauses
    telling_1(Pie, _Name, [T]) :-
        Filename = tellSeeSupport::telling_name,
        Pie:unify(T, str(Filename)).

clauses
    time_4(Pie, _Name, [TH, TM, TS, THH]) :-
        !,
        Time = time::now(),
        Time:getTimeDetailed(H, M, SReal),
        S = math::trunc(SReal),
        HH = math::round((SReal - S) * 100),
        Pie:unify(TH, int(H)),
        Pie:unify(TM, int(M)),
        Pie:unify(TS, int(S)),
        Pie:unify(THH, int(HH)).

clauses
    told_0(_Pie, _Name, []) :-
        !,
        tellSeeSupport::told.

clauses
    trace_0(Pie, _Name, _) :-
        Pie:traceOn := true.

clauses
    var_1(_Pie, _Name, [T]) :-
        T = var(_),
        freeTerm(T).

clauses
    write_n(Pie, _Name, TermL) :-
        PIE:writeterml(write, TermL).

class predicates
    string_slist : (string String) -> string* Slist.
clauses
    string_slist(String) = [H | string_slist(Rest)] :-
        string::frontToken(String, H, Rest),
        !.

    string_slist(String) = [String].

class predicates
    term_to_slist : (term Term) -> string* List determ.
clauses
    term_to_slist(nill) = [].
    term_to_slist(list(str(S), Tail)) = [S | term_to_slist(Tail)].

class predicates
    list_terml : (term, terml) determ (i,o) (o,i).
    % Conversion between list and TermL
clauses
    list_terml(nill, []).
    list_terml(list(H, T), [H | TT]) :-
        list_terml(T, TT).

class predicates
    eeq : (term T1, term T2) determ.
clauses
    eeq(A, B) :-
        eeq2(variable::normalize(A), variable::normalize(B)).

class predicates
    eeq2 : (term, term) determ.
clauses
    eeq2(cmp(ID1, TermL1), cmp(ID2, TermL2)) :-
        !,
        ID1 = ID2,
        eeqterml(TermL1, TermL2).

    eeq2(list(H1, T1), list(H2, T2)) :-
        !,
        eeq(H1, H2),
        eeq(T1, T2).

    eeq2(T, T).

class predicates
    eeqterml : (terml, terml) determ.
clauses
    eeqterml([], []) :-
        !.
    eeqterml([H1 | T1], [H2 | T2]) :-
        eeq(H1, H2),
        eeqterml(T1, T2).

end implement stdPredicate
