% Copyright PDC

implement sciLexerStatusBar inherits statusBarControl

clauses
    new(Parent) :-
        statusBarControl::new(Parent),
        Cell_0 = statusBarCell::new(This, statusBarCellWidth),
        cell_position := statusBarCell::new(This, statusBarCellWidth),
        cell_modified := statusBarCell::new(This, statusBarCellWidth),
        cell_insert := statusBarCell::new(This, statusBarCellWidth),
        cells := [Cell_0, cell_position, cell_modified, cell_insert].

clauses
    attach(SciLexer) :-
        SciLexer:updateUIEvent:addListener({  :- onUpdateUI(SciLexer) }),
        SciLexer:modifiedEvent:addListener({ (Action, _, _, _, _) :- onModified(SciLexer, Action) }),
        SciLexer:savepointEvent:addListener({  :- statusBar_updateModified(SciLexer) }).

predicates
    onUpdateUI : (sciLexer Lexer).
clauses
    onUpdateUI(Lexer) :-
        Pos = Lexer:currentPos,
        cell_position:text := string::format("%d:%d", Lexer:lineFromPosition(Pos), Lexer:getColumn(Pos)),
        cell_insert:text := if true = Lexer:overType then statusBar_overwrite else statusBar_insert end if.

predicates
    onModified : (sciLexer Lexer, sciLexerBase::modifyAction Action).
clauses
    onModified(Lexer, Action) :-
        if sciLexerBase::insertText(_, _, _, _, _) = Action or sciLexerBase::beforeDelete(_, _) = Action then
            postAction({  :- statusBar_updateModified(Lexer) })
        end if.

predicates
    statusBar_updateModified : (sciLexer Lexer).
clauses
    statusBar_updateModified(Lexer) :-
        cell_modified:text := if Lexer:isModified() then statusBar_modified else "" end if.

constants
    statusBarCellWidth = 120.
    statusBar_modified = "Modified".
    statusBar_insert = "Insert".
    statusBar_overwrite = "Overwrite".

facts
    cell_position : statusBarCell [constant].
    cell_modified : statusBarCell [constant].
    cell_insert : statusBarCell [constant].

end implement sciLexerStatusBar
