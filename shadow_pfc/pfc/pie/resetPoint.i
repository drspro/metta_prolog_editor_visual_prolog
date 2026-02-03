% Copyright PDC

namespace pfc\pie

interface resetPoint

domains
    resetAction = ().

predicates
    addResetAction : (resetAction).

predicates
    reset : resetAction.

end interface
