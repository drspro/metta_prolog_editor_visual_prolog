% Copyright PDC

namespace pfc\pie

implement resetPointClass

facts
    resetAction : (resetAction).

clauses
    reset() :-
        foreach retract(resetAction(RA)) do
            RA()
        end foreach.

clauses
    addResetAction(RA) :-
        assert(resetAction(RA)).

end implement resetPointClass
