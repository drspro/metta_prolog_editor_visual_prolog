% Copyright PDC

namespace pfc\pie

implement clauseBase
    open core

facts
    clauseChainMap : redBlackTree::tree{string Name, clauseChain Chain} := redBlackTree::emptyUnique().

clauses
    clause(Head, Body) :-
        tuple(_Name, Chain) = redBlackTree::getAll_nd(clauseChainMap),
        clauseChain::clause(Head, Body) = Chain:getClause_nd().

clauses
    named_clause(Name, ArgList, Body) :-
        Chain = redBlackTree::tryLookUp(clauseChainMap, Name),
        clauseChain::clause(pie::cmp(Name, ArgList), Body) = Chain:getClause_nd().

clauses
    named_clauseList(Name) = redBlackTree::tryLookUp(clauseChainMap, Name):getClauseList().

clauses
    getPredicateName_nd() = Name :-
        tuple(Name, _Chain) = redBlackTree::getAll_nd(clauseChainMap).

clauses
    assertClause(Position, pie::cmp(Name, ArgList), Body) :-
        Chain = getChain(Name),
        assertClause_chain(Position, Chain, clauseChain::clause(pie::cmp(Name, ArgList), Body)).

class predicates
    assertClause_chain : (position Position, clauseChain Chain, clauseChain::clause Clause).
clauses
    assertClause_chain(first(), Chain, Clause) :-
        Chain:asserta(Clause).

    assertClause_chain(last(), Chain, Clause) :-
        Chain:assertz(Clause).

predicates
    getChain : (string Name) -> clauseChain Chain.
clauses
    getChain(Name) = Chain :-
        Chain = redBlackTree::tryLookUp(clauseChainMap, Name),
        !.

    getChain(Name) = Chain :-
        Chain = clauseChain::new(),
        clauseChainMap := redBlackTree::insert(clauseChainMap, Name, Chain).

clauses
    retractAllClauses(Name) :-
        if clauseChainMap := redBlackTree::tryDeleteOne(clauseChainMap, Name, _Chain) then
            succeed
        end if.

clauses
    retractClause(pie::cmp(Name, ArgList), Body) :-
        Chain = redBlackTree::tryLookUp(clauseChainMap, Name),
        !,
        Chain:retract(clauseChain::clause(pie::cmp(Name, ArgList), Body)).

end implement clauseBase
