% Copyright PDC

namespace pfc\pie

interface clauseBase

domains
    position = first; last.

predicates
    getPredicateName_nd : () -> string Name nondeterm.
    clause : (pie::sterm Head, pie::sterm Body) nondeterm (o,o) (cmp(i,o),o) (cmp(o,o),o).
    named_clause : (string PredicateName, pie::sterml Arguments, pie::sterm Body) nondeterm (i,o,o) (i,o,i).
    named_clauseList : (string PredicateName) -> clauseChain::clause* ClauseList determ.

predicates
    assertClause : (position Position, pie::sterm Head, pie::sterm Body) determ.
    retractClause : (pie::sterm Head, pie::sterm Body) determ.
    % Used to give a deterministic retract
    retractAllClauses : (string Name).

end interface clauseBase
