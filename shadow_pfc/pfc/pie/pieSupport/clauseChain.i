% Copyright PDC

namespace pfc\pie

interface clauseChain

domains
    clause = clause(pie::sterm Head, pie::sterm Body).

predicates
    getClause_nd : () -> clause Clause nondeterm.

predicates
    getClauseList : () -> clause* ClauseList.

predicates
    asserta : (clause Clause).
    assertz : (clause Clause).
    retract : (clause Clause) determ.

end interface clauseChain
