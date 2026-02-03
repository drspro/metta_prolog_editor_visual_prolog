% Copyright PDC

namespace pfc\pie

implement pieParser
    open pie

domains
    assoc = x; y.
    % the associavity to the left or the right

clauses
    s_term(IL, OL, Term) :-
        s_lowerterm(IL, OL, Term).

class predicates
    s_lowerterm : (scanner::tokl TokenList1, scanner::tokl TokenList0 [out], sterm STerm [out]).
clauses
    s_lowerterm(LL1, LL0, Term0) :-
        Prior = 1201,
        s_basisterm(Prior, y, LL1, LL2, Term1),
        s_higher(Prior, y, LL2, LL3, Term1, Term2),
        s_lowerterm1(LL3, LL0, Term2, Term0),
        !.

class predicates
    s_basisterm : (operator::prior, assoc, scanner::tokl, scanner::tokl [out], sterm [out]).
clauses
    s_basisterm(_, _, [scanner::t(scanner::atom("+"), _), scanner::t(scanner::int(I), _) | LL], LL, narrowestInteger(I)) :-
        !.

    s_basisterm(_, _, [scanner::t(scanner::atom("+"), _), scanner::t(scanner::real(R), _) | LL], LL, real(R)) :-
        !.

    s_basisterm(_, _, [scanner::t(scanner::atom("-"), _), scanner::t(scanner::int(X), _) | LL], LL, narrowestInteger(I)) :-
        !,
        I = -X.

    s_basisterm(_, _, [scanner::t(scanner::atom("-"), _), scanner::t(scanner::real(X), _) | LL], LL, real(R)) :-
        !,
        R = -X.

    s_basisterm(Prior, Assoc, [scanner::t(scanner::atom(FID), Cursor) | LL1], LL2, cmp(FID, [Term])) :-
        operator::isPrefixOp(FID, NewPrior, XFY),
        !,
        check_ok_rightop(Cursor, Prior, Assoc, NewPrior),
        treat_prefix(NewPrior, XFY, LL1, LL2, Term).

    s_basisterm(_, _, [scanner::t(scanner::var(Name), _) | LL], LL, var(Name)) :-
        !.

    s_basisterm(_, _, [scanner::t(scanner::atom(Name), _) | LL1], LL0, Term0) :-
        !,
        s_term6(LL1, LL0, Name, Term0).

    s_basisterm(_, _, [scanner::t(scanner::int(X), _) | LL], LL, narrowestInteger(X)) :-
        !.

    s_basisterm(_, _, [scanner::t(scanner::real(X), _) | LL], LL, real(X)) :-
        !.

    s_basisterm(_, _, [scanner::t(scanner::char(X), _) | LL], LL, char(X)) :-
        !.

    s_basisterm(_, _, [scanner::t(scanner::str(X), _) | LL], LL, str(X)) :-
        !.

    s_basisterm(_, _, [scanner::t(scanner::lbrack, _) | LL1], LL0, Term) :-
        !,
        s_list(LL1, LL0, Term).

    s_basisterm(_, _, [scanner::t(scanner::lpar, _) | LL1], LL0, Term) :-
        !,
        s_priorterm(1201, y, LL1, LL2, Term),
        expectToken(scanner::rpar, LL2, LL0).

    s_basisterm(_, _, LL, _, _) :-
        raiseUnexpectedToken(LL).

class predicates
    expectToken : (scanner::tok T, scanner::tokl LL1, scanner::tokl LL0 [out]).
clauses
    expectToken(T, [scanner::t(T, _) | LL0], LL0) :-
        !.
    expectToken(Expect, LL1, _) :-
        errorToken(LL1, Cursor, Found),
        pie::raiseSyntaxError(Cursor, "Expecting '%s', found '%s'", tok2str(Expect), Found).

class predicates
    raiseUnexpectedToken : (scanner::tokl LL) erroneous.
clauses
    raiseUnexpectedToken(LL) :-
        errorToken(LL, Cursor, Tok),
        pie::raiseSyntaxError(Cursor, "Unexpected token: %s", Tok).

class predicates
    tok2str : (scanner::tok Tok) -> string Str.
clauses
    tok2str(scanner::lpar) = "(" :-
        !.
    tok2str(scanner::rpar) = ")" :-
        !.
    tok2str(scanner::lbrack) = "[" :-
        !.
    tok2str(scanner::rbrack) = "]" :-
        !.
    tok2str(scanner::lcurly) = "{" :-
        !.
    tok2str(scanner::rcurly) = "}" :-
        !.
    tok2str(scanner::comma) = "," :-
        !.
    tok2str(Tok) = string::write(Tok).

class predicates
    errorToken : (scanner::tokl LL, core::charCount Cursor [out], string Token [out]).
clauses
    errorToken([], 0, "<EOS>").
    errorToken([scanner::t(T, Cursor) | _], Cursor, tok2str(T)).

class predicates
    s_list : (scanner::tokl, scanner::tokl [out], sterm [out]).
clauses
    s_list([scanner::t(scanner::rbrack, _) | IL], IL, nill) :-
        !.
    s_list(IL, OL, list(Term, Rest)) :-
        s_priorterm(1000, x, IL, OL1, Term),
        s_list1(OL1, OL, Rest).

class predicates
    s_list1 : (scanner::tokl, scanner::tokl [out], sterm [out]).
clauses
    s_list1([scanner::t(scanner::rbrack, _) | IL], IL, nill) :-
        !.

    s_list1([scanner::t(scanner::comma, _) | IL], OL, list(Term, Rest)) :-
        !,
        s_priorterm(1000, x, IL, OL1, Term),
        s_list1(OL1, OL, Rest).

    s_list1([scanner::t(scanner::bar, _) | IL], OL, Term) :-
        !,
        s_priorterm(1000, x, IL, OL1, Term),
        expectToken(scanner::rbrack, OL1, OL).

    s_list1(LL, _, _) :-
        raiseUnexpectedToken(LL).

class predicates
    s_term6 : (scanner::tokl, scanner::tokl [out], string, sterm [out]).
clauses
    s_term6([scanner::t(scanner::atom("::"), _), scanner::t(scanner::atom(ID2), _) | LL1], LL0, ID, Term) :-
        !,
        QualifiedId = string::concat(ID, "::", ID2),
        s_term6(LL1, LL0, QualifiedId, Term).

    s_term6([scanner::t(scanner::lpar, _) | LL1], LL0, FID, cmp(FID, TermL)) :-
        !,
        s_terml(LL1, LL2, TermL),
        expectToken(scanner::rpar, LL2, LL0).

    s_term6(LL, LL, Name, atom(Name)).

class predicates
    s_terml : (scanner::tokl, scanner::tokl [out], sterml [out]).
clauses
    s_terml(LL, LL, []) :-
        [scanner::t(scanner::rpar, _) | _] = LL,
        !.

    s_terml(LL1, LL0, [Term | TermL]) :-
        s_priorterm(999, y, LL1, LL2, Term),
        s_terml1(LL2, LL0, TermL).

class predicates
    s_terml1 : (scanner::tokl, scanner::tokl [out], sterml [out]).
clauses
    s_terml1([scanner::t(scanner::comma, _) | LL1], LL2, TermL) :-
        !,
        s_terml(LL1, LL2, TermL).

    s_terml1(LL, LL, []).

class predicates
    treat_prefix : (operator::prior, operator::xfy, scanner::tokl, scanner::tokl [out], sterm [out]).
clauses
    treat_prefix(Prior, "fx", LL1, LL2, Term) :-
        !,
        s_priorterm(Prior, x, LL1, LL2, Term).

    treat_prefix(Prior, "fy", LL1, LL2, Term) :-
        !,
        s_priorterm(Prior, y, LL1, LL2, Term).

    treat_prefix(_, FX, _, _, _) :-
        exception::raise_errorf("Unexpected fx value: %", FX).

class predicates
    s_lowerterm1 : (scanner::tokl TL1, scanner::tokl TL0 [out], sterm T1, sterm T0 [out]).
clauses
    s_lowerterm1([], [], Term, Term) :-
        !.
    s_lowerterm1([scanner::t(scanner::dot, _) | LL], LL, Term, Term) :-
        !.
    s_lowerterm1(LL2, LL0, Term1, Term0) :-
        Prior = 1201,
        s_higher_something(Prior, y, LL2, LL3, Term1, Term2),
        s_lowerterm1(LL3, LL0, Term2, Term0).

class predicates
    s_priorterm : (operator::prior, assoc, scanner::tokl, scanner::tokl [out], sterm [out]).
clauses
    s_priorterm(Prior, Assoc, LL1, LL0, Term0) :-
        s_basisterm(Prior, Assoc, LL1, LL2, Term),
        s_higher(Prior, Assoc, LL2, LL0, Term, Term0).

class predicates
    check_ok_rightop : (core::charCount, operator::prior, assoc, operator::prior).
clauses
    check_ok_rightop(_, Prior, Assoc, NewPrior) :-
        ok_rightop(Prior, Assoc, NewPrior),
        !.

    check_ok_rightop(Cursor, Prior, Assoc, NewPrior) :-
        pie::raiseSyntaxError(Cursor, "Prior = %, Assoc = %, NewPrior = %", Prior, Assoc, NewPrior).

class predicates
    ok_rightop : (operator::prior, assoc, operator::prior) determ.
clauses
    ok_rightop(Prior, x, NewPrior) :-
        NewPrior < Prior.
    ok_rightop(Prior, y, NewPrior) :-
        NewPrior <= Prior.

class predicates
    is_op : (scanner::tok, operator::prior [out], operator::xfy [out], operator::op [out]) determ.
clauses
    is_op(scanner::comma, Prior, XFY, OP) :-
        OP = ",",
        operator::getop(Prior, XFY, OP).

    is_op(scanner::dot, Prior, XFY, OP) :-
        OP = ".",
        operator::getop(Prior, XFY, OP).

    is_op(scanner::atom(OP), Prior, XFY, OP) :-
        operator::getop(Prior, XFY, OP).

class predicates
    s_higher_something : (operator::prior, assoc, scanner::tokl, scanner::tokl [out], sterm, sterm [out]).
clauses
    s_higher_something(Prior, Assoc, [scanner::t(TOK, _) | LL1], LL0, Term, Term0) :-
        is_op(TOK, NewPrior, XFY, OP),
        ok_rightop(Prior, Assoc, NewPrior),
        !,
        treat_sufinfix(NewPrior, XFY, OP, LL1, LL2, Term, Term1),
        s_higher(Prior, Assoc, LL2, LL0, Term1, Term0).

    s_higher_something(_Prior, _Assoc, LL, _, _Term, _) :-
        raiseUnexpectedToken(LL).

class predicates
    s_higher : (operator::prior, assoc, scanner::tokl, scanner::tokl [out], sterm, sterm [out]).
clauses
    s_higher(Prior, Assoc, [scanner::t(TOK, _) | LL1], LL0, Term, Term0) :-
        is_op(TOK, NewPrior, XFY, OP),
        ok_rightop(Prior, Assoc, NewPrior),
        !,
        treat_sufinfix(NewPrior, XFY, OP, LL1, LL2, Term, Term1),
        s_higher(Prior, Assoc, LL2, LL0, Term1, Term0).

    s_higher(_, _, LL, LL, Term, Term).

class predicates
    treat_sufinfix : (operator::prior, operator::xfy, operator::op, scanner::tokl, scanner::tokl [out], sterm, sterm [out]).
clauses
    treat_sufinfix(NewPrior, XFY, OP, LL1, LL2, Term, Term1) :-
        treat_sufinfix_dt(NewPrior, XFY, OP, LL1, LL2, Term, Term1),
        !.

    treat_sufinfix(_NewPrior, XFY, _OP, _LL1, _LL2, _Term, _Term1) :-
        exception::raise_errorf("Unexpected xfy value: %s", XFY).

class predicates
    treat_sufinfix_dt : (operator::prior, operator::xfy, operator::op, scanner::tokl, scanner::tokl [out], sterm, sterm [out]) determ.
clauses
    treat_sufinfix_dt(Prior, "yfx", OP, LL1, LL2, Term1, cmp(OP, [Term1, Term2])) :-
        s_priorterm(Prior, x, LL1, LL2, Term2).
    treat_sufinfix_dt(Prior, "xfx", OP, LL1, LL2, Term1, cmp(OP, [Term1, Term2])) :-
        s_priorterm(Prior, x, LL1, LL2, Term2).
    treat_sufinfix_dt(Prior, "xfy", OP, LL1, LL2, Term1, cmp(OP, [Term1, Term2])) :-
        s_priorterm(Prior, y, LL1, LL2, Term2).
    treat_sufinfix_dt(_, "xf", OP, LL, LL, Term, cmp(OP, [Term])).
    treat_sufinfix_dt(_, "yf", OP, LL, LL, Term, cmp(OP, [Term])).

class predicates
    narrowestInteger : (integer64 X) -> pie::termBase{string}.
clauses
    narrowestInteger(X) = R :-
        R = pie::int(tryConvert(integer, X)),
        !.

    narrowestInteger(X) = pie::int64(X).

end implement pieParser
