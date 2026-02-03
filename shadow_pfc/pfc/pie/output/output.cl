% Copyright PDC

namespace pfc\pie

class output
    open pie

predicates
    % Input term is expected to be normalized
    wterm : (outputStream Stream, outputMode Mode, term Term).
    termToString : (outputMode Mode, term Term) -> string.
    wsterm : (outputStream Stream, outputMode Mode, sterm STerm).
    stermToString : (outputMode Mode, sterm STerm) -> string.
    wclause : (outputStream Stream, sterm, sterm).
    clauseToString : (sterm, sterm) -> string.

end class output
