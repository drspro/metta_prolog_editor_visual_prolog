% Copyright PDC

namespace sciLexerSupport

class findAndReplaceControl : findAndReplaceControl
    open core
    [noDefaultConstructor]

predicates
    displayModeless : (sciLexer Owner, boolean AsDialog) -> findAndReplaceControl Dialog.
    displayModal : (sciLexer Owner) -> findAndReplaceControl Dialog.

predicates
    tryGetModeless : () -> findAndReplaceControl FindAndReplaceControl determ.

predicates
    findNext : ().
    findPrevious : ().

domains
    historyReader = (boolean ForReplace) -> string* History.
    historyAppender = (boolean ForReplace, string Text).
    settingWriter = (boolean* Settings).

properties
    readHistory : historyReader.
    appendHistory : historyAppender.
    writeSettings : settingWriter.
    % @short Callback predicates to read/append the find/replace history, and update the settings
    % @end

predicates
    loadSettings : (boolean* Settings).

properties
    writeRect : predicate{vpiDomains::rct} (i).
    readRect : function_dt{vpiDomains::rct} (i).

end class findAndReplaceControl
