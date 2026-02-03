% Copyright PDC

namespace pfc\pie

implement clauseChain

facts
    clause_fact : (clause Clause).

clauses
    getClause_nd() = C :-
        clause_fact(C).

clauses
    getClauseList() = [ C || clause_fact(C) ].

clauses
    asserta(Clause) :-
        ::asserta(clause_fact(Clause)).

clauses
    assertz(Clause) :-
        ::assertz(clause_fact(Clause)).

clauses
    retract(Clause) :-
        ::retract(clause_fact(Clause)),
        !.

end implement clauseChain
