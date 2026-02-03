% Copyright PDC

namespace pfc\pie

implement variable
    open pie

class facts
    nextVarNum : integer := 0.

facts
    name : string [constant].
    printName_fact : (string PrintName) determ.
    term_fact : (term Term) determ.

clauses
    new(Name) :-
        name := Name.

clauses
    newAnonymous() = var(new("_")).

clauses
    normalize(var(Var)) = Var:getTerm() :-
        !.

    normalize(T) = T.

clauses
    getTerm() = getTerm_term(Term) :-
        term_fact(Term),
        !.

    getTerm() = var(This).

class predicates
    getTerm_term : (term Term1) -> term Term0.
clauses
    getTerm_term(var(V)) = V:getTerm() :-
        !.

    getTerm_term(T) = T.

clauses
    setTerm(Term) :-
        assert(term_fact(Term)).

clauses
    setTerm(Term, CP) :-
        assert(term_fact(Term)),
        CP:addResetAction(reset).

predicates
    reset : resetPoint::resetAction.
clauses
    reset() :-
        retractAll(term_fact(_)).

clauses
    isFreeGetVariable() = V :-
        var(V) = getTerm().

clauses
    printName() = Name :-
        printName_fact(Name),
        !.

    printName() = Name :-
        Name = string::format("%$%", name, nextVarNum),
        assert(printName_fact(Name)),
        nextVarNum := nextVarNum + 1.

end implement variable
