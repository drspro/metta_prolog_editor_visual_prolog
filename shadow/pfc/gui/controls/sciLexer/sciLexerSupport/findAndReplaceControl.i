% Copyright PDC

namespace sciLexerSupport

interface findAndReplaceControl supports control

predicates
    reactivateModeless : ().

predicates
    incrementalFindNextRange : (sciLexer Editor) -> boolean Found.
    incrementalFindPreviousRange : (sciLexer Editor) -> boolean Found.

end interface findAndReplaceControl
