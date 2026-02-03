% Copyright PDC

namespace pfc\pie

implement output
    open pie

domains
    assoc = x; y.
    % @short the associavity to the left or the right
    % @end

class predicates
    wterml : (outputStream, outputMode, pie::terml).
    wcmp : (outputStream, outputMode, string, pie::terml).
    wfix : (outputStream, outputMode, operator::prior, operator::xfy, operator::op, pie::terml) determ.
    wleft : (outputStream, outputMode, assoc, operator::prior, pie::term).
    wright : (outputStream, outputMode, assoc, operator::prior, pie::term).
    wpfixop : (outputStream, operator::op).
    quotepfixop : (string) determ.
    prefix : (operator::xfy, assoc [out]) determ.
    suffix : (operator::xfy, assoc [out]) determ.
    infix : (operator::xfy, assoc [out], assoc [out]) determ.
    brackets_needed : (assoc, operator::prior, pie::term) determ.
    wlist : (outputStream, outputMode, pie::term).
    wlist1 : (outputStream, outputMode, pie::term Head, pie::term Tail).

clauses
    wclause(Stream, Head, atom("true")) :-
        !,
        Stream:write(" "),
        wsterm(Stream, list, Head),
        Stream:write(".\n").

    wclause(Stream, Head, Body) :-
        Stream:write(" "),
        Env = environment2::new(),
        Head1 = Env:mk_term(Head),
        Body1 = Env:mk_term(Body),
        wterm(Stream, list, Head1),
        Stream:write(" :-\n"),
        wor(Stream, Body1, 1),
        Stream:write(".\n").

class predicates
    wor : (outputStream, pie::term Term, integer Indent).
clauses
    wor(Stream, pie::cmp(";", [G, GG]), Indent) :-
        !,
        Indent1 = Indent + 1,
        wor(Stream, G, Indent1),
        Stream:nl(),
        indent(Stream, Indent),
        Stream:write(";\n"),
        wor(Stream, GG, Indent1).

    wor(Stream, G, Indent) :-
        wand(Stream, G, Indent).

class predicates
    wand : (outputStream, pie::term Term, integer Indent).
clauses
    wand(Stream, pie::cmp(", ", [G, GG]), Indent) :-
        !,
        indent(Stream, Indent),
        output::wterm(Stream, list, G),
        Stream:write(", \n"),
        wand(Stream, GG, Indent).

    wand(Stream, G, Indent) :-
        indent(Stream, Indent),
        output::wterm(Stream, list, G).

class predicates
    indent : (outputStream, integer).
clauses
    indent(_, 0) :-
        !.
    indent(Stream, N) :-
        N1 = N - 1,
        Stream:write('\t'),
        indent(Stream, N1).

clauses
    wsterm(Stream, Mode, Sterm) :-
        Env = environment2::new(),
        Term = Env:mk_term(Sterm),
        wterm(Stream, Mode, Term).

clauses
    wterm(Stream, Mode, Term) :-
        wterm1(Stream, Mode, variable::normalize(Term)).

class predicates
    wterm1 : (outputStream, outputMode Mode, pie::term Term).
clauses
    wterm1(Stream, _, pie::int(X)) :-
        Stream:write(X).

    wterm1(Stream, _, pie::int64(X)) :-
        Stream:write(X).

    wterm1(Stream, _, pie::real(X)) :-
        Stream:write(X).

    wterm1(Stream, write, pie::str(X)) :-
        Stream:write(X).

    wterm1(Stream, display, pie::str(X)) :-
        % toString add quotes and escape special characters
        Stream:write(toString(X)).

    wterm1(Stream, list, pie::str(X)) :-
        Stream:write(toString(X)).

    wterm1(Stream, write, pie::char(X)) :-
        !,
        Stream:write(X).

    wterm1(Stream, _, pie::char(X)) :-
        Stream:write(toString(X)).

    wterm1(Stream, _, pie::atom(X)) :-
        if pfc\pie\scanner::atomNeedNotBeQuoted(X) then
            Stream:write(X)
        else
            QuotedEscaped = toString(X),
            UnQuotedEscaped = string::subString(QuotedEscaped, 1, string::length(QuotedEscaped) - 2),
            Stream:write("'", UnQuotedEscaped, "'")
        end if.

    wterm1(Stream, _, pie::object(X)) :-
        Stream:write("<object ", X, ">").

    wterm1(Stream, _, pie::pointer(X)) :-
        Stream:write("<pointer ", X, ">").

    wterm1(Stream, _, pie::var(X)) :-
        Stream:write(X:printName).

    wterm1(Stream, _, pie::nill) :-
        Stream:write("[]").

    wterm1(Stream, MODE, pie::list(HEAD, TAIL)) :-
        Stream:write('['),
        wlist(Stream, MODE, pie::list(HEAD, TAIL)),
        Stream:write(']').

    wterm1(Stream, MODE, pie::cmp(FID, TERML)) :-
        wcmp(Stream, MODE, FID, TERML).

clauses
    wcmp(Stream, MODE, FID, TERML) :-
        not(MODE = display),
        OP = FID,
        operator::getop(PRIOR, ASSOC, convert(operator::op, OP)),
        wfix(Stream, MODE, PRIOR, ASSOC, convert(operator::op, OP), TERML),
        !.

    wcmp(Stream, MODE, FID, TERML) :-
        FID = OP,
        wpfixop(Stream, convert(operator::op, OP)),
        Stream:write('('),
        wterml(Stream, MODE, TERML),
        Stream:write(')').

clauses
    wpfixop(Stream, OP) :-
        quotepfixop(OP),
        !,
        Stream:write('\'', OP, '\'').

    wpfixop(Stream, OP) :-
        Stream:write(OP).

clauses
    quotepfixop(OP) :-
        operator::getop(_, _, convert(operator::op, OP)),
        !.

    quotepfixop(OP) :-
        string::frontChar(OP, CH, _),
        CH <> '_',
        LOP = string::toLowerCase(OP),
        string::frontChar(LOP, LO, _),
        CH = LO,
        string::isName(OP),
        !,
        fail.

    quotepfixop(_).

clauses
    prefix("fx", x).
    prefix("fy", y).

clauses
    suffix("xf", x).
    suffix("yf", y).

clauses
    infix("xfx", x, x).
    infix("xfy", x, y).
    infix("yfx", y, x).
    infix("yfy", y, y).

clauses
    wfix(Stream, MODE, PRIOR, ASSOC, OP, [TERM]) :-
        prefix(ASSOC, XY),
        !,
        Stream:write(OP, ' '),
        wright(Stream, MODE, XY, PRIOR, TERM).

    wfix(Stream, MODE, PRIOR, ASSOC, OP, [TERM]) :-
        suffix(ASSOC, XY),
        !,
        wleft(Stream, MODE, XY, PRIOR, TERM),
        Stream:write(' ', OP).

    wfix(Stream, MODE, PRIOR, ASSOC, OP, [TERM1, TERM2]) :-
        infix(ASSOC, LEFT_XY, RIGHT_XY),
        wleft(Stream, MODE, LEFT_XY, PRIOR, TERM1),
        Stream:write(' ', OP, ' '),
        wright(Stream, MODE, RIGHT_XY, PRIOR, TERM2).

clauses
    brackets_needed(_, PRIOR, TERM) :-
        bound(TERM),
        TERM = pie::cmp(FID, _),
        OP = FID,
        operator::getop(PRIOR1, _, OP),
        PRIOR1 > PRIOR,
        !.

    brackets_needed(x, PRIOR, TERM) :-
        bound(TERM),
        TERM = pie::cmp(FID, _),
        OP = FID,
        operator::getop(PRIOR, _, OP).

clauses
    wright(Stream, MODE, XY, PRIOR, TERM) :-
        brackets_needed(XY, PRIOR, TERM),
        !,
        Stream:write('('),
        wterm(Stream, MODE, TERM),
        Stream:write(')').

    wright(Stream, MODE, _, _, TERM) :-
        wterm(Stream, MODE, TERM).

clauses
    wleft(Stream, MODE, XY, PRIOR, TERM) :-
        brackets_needed(XY, PRIOR, TERM),
        !,
        Stream:write('('),
        wterm(Stream, MODE, TERM),
        Stream:write(')').

    wleft(Stream, MODE, _, _, TERM) :-
        wterm(Stream, MODE, TERM).

clauses
    wterml(_, _, []) :-
        !.

    wterml(Stream, MODE, [H]) :-
        !,
        wterm(Stream, MODE, H).

    wterml(Stream, MODE, [H | T]) :-
        wterm(Stream, MODE, H),
        Stream:write(","),
        wterml(Stream, MODE, T).

clauses
    wlist(_, _Mode, pie::nill) :-
        !.

    wlist(Stream, Mode, pie::list(H, T)) :-
        !,
        wlist1(Stream, Mode, variable::normalize(H), variable::normalize(T)).

    wlist(Stream, MODE, T) :-
        Stream:write("Not a List <<<"),
        wterm(Stream, MODE, T),
        Stream:write(">>>").

clauses
    wlist1(Stream, MODE, H, pie::var(Var)) :-
        !,
        wterm(Stream, MODE, H),
        Stream:write("|"),
        wterm(Stream, Mode, pie::var(VAR)).

    wlist1(Stream, MODE, H, pie::nill) :-
        !,
        wterm(Stream, MODE, H).

    wlist1(Stream, MODE, H, T) :-
        !,
        wterm(Stream, MODE, H),
        Stream:write(","),
        wlist(Stream, MODE, T).

clauses
    termToString(Mode, Term) = outputStream_string::getString(wterm, Mode, Term).

clauses
    stermToString(Mode, Sterm) = outputStream_string::getString(wsterm, Mode, STerm).

clauses
    clauseToString(Head, Body) = outputStream_string::getString(wclause, Head, Body).

end implement output
