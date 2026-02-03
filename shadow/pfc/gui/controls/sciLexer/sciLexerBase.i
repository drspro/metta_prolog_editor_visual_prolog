% Copyright PDC
%
% Copyright 1998-2002 by Neil Hodgson <neilh@scintilla.org>
%
% All Rights Reserved
%
% Permission to use, copy, modify, and distribute this software and its
% documentation for any purpose and without fee is hereby granted,
% provided that the above copyright notice appear in all copies and that
% both that copyright notice and this permission notice appear in
% supporting documentation.
%
% NEIL HODGSON DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
% AND FITNESS, IN NO EVENT SHALL NEIL HODGSON BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
% WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
% TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE
% OR PERFORMANCE OF THIS SOFTWARE.

interface sciLexerBase
    open core, sciLexer_native

constants
%    default_styleFore = 0x000000.
% 31-10-2025
    default_styleFore = 0x82FFFF.
% 0x82FFFF geel


%    key_major_styleFore = 0x008080.
    key_major_styleFore = 0x80FF80.

%    key_minor_styleFore = 0x993333.
%    key_directive_styleFore = 0x800080.

    key_minor_styleFore = 0x80FF80.
    key_directive_styleFore = 0x80FF80.


% 31-10-2025
%    identifier_styleFore = 0x000000.
    identifier_styleFore = 0xCCDDDC.
%    variable_styleFore = 0x008000.
%    variable_styleFore = 0x35FFFF.
% 9-11-2025
%    variable_styleFore = 0xC6FFFF.
% last above

%    variable_styleFore = vpiDomains::color_Magenta.

    variable_styleFore = 0xFF95CA.
%
% 31-10-2025
%    anonymous_styleFore = 0xA0C0A0.
    anonymous_styleFore = 0x80FF80.

%    number_styleFore = 0xC00000.
    number_styleFore = 0x9D9DFF.

%    operator_styleFore = 0x0000A0.
    operator_styleFore = vpiDomains::color_Cyan.

    cpp_keyword_styleFore = 0xFF0000.
    cpp_keyword2_styleFore = 0x8000FF.
    uuid_styleFore = 0xFF0000.
    regex_styleFore = 0xFF0000.
    embedded_styleFore = string_verbatim_styleFore.
    placeholder_styleFore = string_styleFore.
    % @short Default colors used by Lexers (0xBBGGRR (blue-green-red))
    % @end

constants
%    comment_block_styleFore = 0x808080.
%    comment_block_styleFore = 0x36BC10.
%    comment_block_styleFore = 0x4B9700.

% was deze leger groen
%     comment_block_styleFore = 0x22955F.
     comment_block_styleFore = 0x767676.
%

    comment_line_styleFore = comment_block_styleFore.
    comment_key_styleFore = 0xA05080.
    comment_key_error_styleFore = comment_block_styleFore.
    comment_doc_styleFore = 0x809595.
    comment_doc_line_styleFore = 0x809595.
    % @short Default comment colors used by Lexers (0xBBGGRR (blue-green-red))
    % @end

constants
%    string_styleFore = 0xAD8F27.
    string_styleFore = 0x9D9DFF.

    string_verbatim_styleFore = 0xB6217E.
    string_escape_styleFore = 0x0070F0.
    string_verbatim_special_styleFore = string_escape_styleFore.
    string_verbatim_eol_styleBack = 0xE8F4FF.
    % background needs to be brighter to appear the same as the text
    % @short Default string and character literal colors used by Lexers (0xBBGGRR (blue-green-red))
    % @end

constants
    string_eol_open_styleFore = 0xFFFFFF.
    % White
    string_eol_open_styleBack = 0x4040FF.
    % "Alert Red"
    string_escape_error_styleFore = string_eol_open_styleFore.
    string_escape_error_styleBack = string_eol_open_styleBack.
    character_too_many_styleFore = string_eol_open_styleFore.
    character_too_many_styleBack = string_eol_open_styleBack.
    character_escape_error_styleFore = string_eol_open_styleFore.
    character_escape_error_styleBack = string_eol_open_styleBack.
    % @short Default colors used by Visual Prolog lexer for illegal string and character literals (0xBBGGRR (blue-green-red))
    % @end

constants
    embedJavaScript_styleBack = 0xD7F7FF.
    % @short Default background color for embedded javascript.
    % @end

constants
    aspJavaScript_styleBack = 0xDFDF7F.
    % @short Default background color for ASP Javascript.
    % @end

constants
    embedSGML_styleBack = 0xE6DDCC.
    % @short Default background color for embeded SGML.
    % @end

constants
    embedVbScript_styleBack = 0xE6DDCC.
    % @short Default background color for embeded VB script.
    % @end

constants
    aspVbScript_styleBack = 0xCFCFF0.
    % @short Default background color for asp VB script.
    % @end

constants
    embedPython_styleBack = 0xF0F0FF.
    % @short Default background color for embeded Python.
    % @end

constants
    aspPython_styleBack = 0xCFF0CF.
    % @short Default background color for asp Python.
    % @end

constants
    embedPHP_styleBack = 0xFFF8F8.
    % @short Default background color for embeded PHP.
    % @end

constants
% 31-10-2025
%    braceLight_styleFore = 0x000000.
%    braceLight_styleBack = 0xFFFF50.

    braceLight_styleFore = 0xFFFFFF.

%    braceLight_styleBack = 0x000000  vpiDomains::color_Red.
    braceLight_styleBack = vpiDomains::color_Red.

    % @short Default bracelight foreground and background color used by Lexers (0xBBGGRR (blue-green-red))
    % @end

constants
    highlightSelectedWords_none = 0.
    highlightSelectedWords_sameStyle = 1.
    highlightSelectedWords_all = 2.

properties
    highlightSelectedWords : integer.
    % @short
    % Determines whether the selected word will be highlighted in the rest of the file<br>
    % #highlightSelectedWords_none none are highlighted<br>
    % #highlightSelectedWords_sameStyle words with same lexer style are highlighted<br>
    % #highlightSelectedWords_all all same words are highlighted<br>
    % @end

properties
    extraLineSeparation : integer.
    % @short
    % extraLineSeperation is the sum of extraAscent and extraDescent, when setting it the value will be divided between extraAscent and extraDescent
    % (optionally with one more to extraDescent).
    % @end

predicates
    setDefaultLexerStyle : ().
    % @short
    % Sets style_default to a "nice" choice.
    % This predicate is called when an editor is created, so it is mainly necessary to reset to defaults.
    % @end

predicates
    lexerDefault_visualProlog : ().
    lexerDefault_Metta : ().
    lexerDefault_vipGrammar : ().
    lexerDefault_cpp : ().
    lexerDefault_html : ().
    lexerDefault_xml : ().
    lexerDefault_markdown : ().
    % @short
    % Use lexer and set default styles for the lexer.
    % Before calling this predicate set style_default as desired (for exampel by calling setDefaultLexerStyle)
    % @end

predicates
    lexerDefault_extension : (string Extension).
    % @short
    % Set the lexer based on a file #Extension. The selection is based on the sciLexer::fileExtension_<lexer> properties.
    % @end

predicates
    removeTrailingSpaces : ().
    % @short
    % Remove all trailing spaces on all lines in in the editor.
    % @end

properties
    text : string.
    textUtf8 : string8.
    textSegmented_utf16 : segmented::segmented_utf16.
    textSegmented_utf8 : segmented::segmented_utf8.
    % @short
    % The entire text of the editor.
    % If you then save the text, you should use setSavepoint to mark the text as unmodified.
    % @end

predicates
    readFromStreamAndClose : (inputStream Stream).
    % @short
    % Set the entire text of the editor to the text read from #Stream.  The #Stream is also closed.
    % @end

predicates
    writeToStreamAndClose : (outputStream Stream).
    % @short
    % Write the entire text of the editor to #Stream. #Stream is also closed.
    % @end

predicates
    setSavePoint : ().
    % @short
    % This message tells Scintilla that the current state of the document is unmodified. This is usually done when the file
    % is saved or loaded, hence the name "save point". As Scintilla performs undo and redo operations, it notifies the
    % container that it has entered or left the save point with SCN_SAVEPOINTREACHED and SCN_SAVEPOINTLEFT notification
    % messages, allowing the container to know if the file should be considered dirty or not.
    %
    % See also: SCI_EMPTYUNDOBUFFER, SCI_GETMODIFY
    % @end

predicates
    getLine : (lineNumber Line) -> string Text.
    % @short
    % This fills the buffer defined by text with the contents of the nominated line (lines start at 1).
    % The returned text includes any end of line characters.
    % If you ask for a line number outside the range of lines in the document, 0 characters are copied.
    % @end

predicates
    replaceSel : (string Text).
    replaceSel_utf8 : (string8 Text).
    % @short
    % The currently selected text between the anchor and the current position is replaced by the #Text string.
    % If the anchor and current position are the same, the text is inserted at the caret position.
    % The caret is positioned after the inserted text and the caret is scrolled into view.
    % @end

properties
    readOnly : boolean.
    % @short
    % ReadOnly state.
    % If you mark a document as read only, attempts to modify the text cause the SCN_MODIFYATTEMPTRO notification.
    % @end

predicates
    getTextRange : (integer CpMin, integer CpMax) -> string Text.
    getTextRange_utf8 : (integer CpMin, integer CpMax) -> string8 Text.
    % @short
    % This collects the #Text between the positions #CpMin and #CpMax.  If #CpMax is -1, text is returned to the end of the document.
    % If CpMin or CpMax is in the middle of an utf8 char they will be adjusted to the next character (see adjustPositionNext).
    % @end

predicates
    allocate : (byteCount Size).
    % @short
    % Allocate an internal buffer of #Size bytes (will not be less than the current content requirement).
    % @end

predicates
    addText : (string Text).
    addText_utf8 : (string8 Text).
    % @short
    % Inserts the #Text before the insertion point.
    % The current position is set at the end of the inserted text, but it is not scrolled into view.
    % @end

predicates
    appendText : (string Text).
    appendText_utf8 : (string8 Text).
    % @short
    % Append #Text to the editor contents.
    % @end

predicates
    insertText : (string Text).
    insertText_utf8 : (string8 Text).
    % @short
    % Inserts the #Text after the insertion point.
    % The current position is not moved.
    % @end

predicates
    insertText : (integer Pos, string Text).
    insertText_utf8 : (integer Pos, string8 Text).
    % @short
    % Inserts the #Text string at position #Pos.
    % If the current position is after the insertion point then the current position is moved along with its surrounding text, but no scrolling is performed.
    % @end

predicates
    clearAll : ().
    % @short
    % Clear all the text (unless the document is read-only).
    % @end

predicates
    deleteRange : (integer Pos, integer DeleteLength).
    % @short
    % Deletes a range of text in the document.
    % @end

predicates
    clearDocumentStyle : ().
    % @short
    % Clear all styling information and reset the folding state.
    % For example to completely restyle the document after choosing a lexer.
    % @end

predicates
    tryGetChar8At : (integer Pos) -> char8 Char8 determ.
    % @short
    % This returns the #Char8 at #Pos in the document.
    % Fails if outside the document.
    % @end

predicates
    tryGetCharAt : (integer Pos) -> char Char determ.
    % @short
    % This returns the #Char at #Pos in the document.  Any position within a multi byte UTF-8 character will return that character (in Unicode).
    % Fails if outside the document.
    % @end

predicates
    getStyleAt : (integer Pos) -> sci_style Style.
    % @short
    % This returns the #Style at #Pos in the document.
    % Fails if outside the document.
    % @end

% Search and Replace
predicates
    tryFindText : (integer SearchFlags, sci_characterRange SearchRange, string Text) -> sci_characterRange MatchRange determ.
    tryFindText_utf8 : (integer SearchFlags, sci_characterRange SearchRange, string8 Text) -> sci_characterRange MatchRange determ.
    % @short
    % Search for #Text in #SearchRange.
    % If sciLexer_native::scfind_regexp is not included in the flags, you can search backwards by setting Max less than Min in #SearchRange.
    % If sciLexer_native::scfind_regexp is included, the search is always forwards.
    % @end

predicates
    searchAnchor : ().
    trySearchNext : (integer SearchFlags, string Text) -> integer FoundPos determ.
    trySearchNext_utf8 : (integer SearchFlags, string8 Text) -> integer FoundPos determ.
    trySearchPrev : (integer SearchFlags, string Text) -> integer FoundPos determ.
    trySearchPrev_utf8 : (integer SearchFlags, string8 Text) -> integer FoundPos determ.
    % @short
    % searchAnchor, trySearchNext and trySearchPrev provide relocatable search support.
    % This allows multiple incremental interactive searches to be macro recorded while still setting the selection to found text so the find/select operation is self-contained.
    % These predicates sends sciLexer_native::scn_macroRecord notifications if macro recording is enabled.
    %
    %
    % searchAnchor sets the search start point used by trySearchNext and trySearchPrev to the start of the current selection, that is, the end of the
    % selection that is nearer to the start of the document.
    % You should always call searchAnchor before calling either of trySearchNext or trySearchPrev.
    % trySearchNext and trySearchPrev search for the next and previous occurrence of #Text.
    % The search is modified by the #SearchFlags.
    % If you request a regular expression, trySearchPrev finds the first occurrence of the search string in the document, not the previous one before the anchor point.
    % The selection is updated to show the matched text, but is not scrolled into view.
    % @end
    %
    % Search and replace using the target
    % Using SCI_REPLACESEL, modifications cause scrolling and other visible changes, which may take some time and cause
    % unwanted display updates. If performing many changes, such as a replace all command, the target can be used instead.
    % First, set the target, ie. the range to be replaced. Then call SCI_REPLACETARGET or SCI_REPLACETARGETRE.
    % Searching can be performed within the target range with SCI_SEARCHINTARGET, which uses a counted string to allow
    % searching for null characters. It returns the position of the start of the matching text range or -1 for failure, in
    % which case the target is not moved. The flags used by SCI_SEARCHINTARGET such as SCFIND_MATCHCASE, SCFIND_WHOLEWORD,
    % SCFIND_WORDSTART, and SCFIND_REGEXP can be set with SCI_SETSEARCHFLAGS. SCI_SEARCHINTARGET may be simpler for some
    % clients to use than SCI_FINDTEXT, as that requires using a pointer to a structure.
    % SCI_SETTARGETSTART(int pos)
    % SCI_GETTARGETSTART
    % SCI_SETTARGETEND(int pos)
    % SCI_GETTARGETEND
    % SCI_TARGETFROMSELECTION
    % SCI_SETSEARCHFLAGS(int searchFlags)
    % SCI_GETSEARCHFLAGS
    % SCI_SEARCHINTARGET(int length, const char8 *text)
    % SCI_REPLACETARGET(int length, const char8 *text)
    % SCI_REPLACETARGETRE(int length, const char8 *text)
    % SCI_GETTAG(int tagNumber, char8 *tagValue)
    %

properties
    targetStart : integer.
    targetEnd : integer.
    % @short
    % Search and search-replace target start/end.
    % When searching in non-regular expression mode, you can set start greater than end to find the last matching text in the target rather than the first matching text.
    % The target is also set by a successful trySearchInTarget.
    % @end

predicates
    targetFromSelection : ().
    % @short
    % Sets targetStart and targetEnd to the start and end positions of the selection.
    % @end

properties
    searchFlags : unsigned.
    % @short
    % The searchFlags used by trySearchInTarget.
    % There are several option flags including a simple regular expression search.
    % @end

predicates
    trySearchInTarget : (string Text) -> integer Pos determ.
    trySearchInTarget_utf8 : (string8 Text) -> integer Pos determ.
    % @short
    % This searches for the first occurrence of a #Text string in the target defined by targetStart and targetEnd.
    % The search is modified by the search flags set by searchFlags.
    % If the search succeeds, the target is set to the found text and the return value is the position of the start of the matching text.
    %
    % Using replaceSel, modifications cause scrolling and other visible changes, which may take some time and cause unwanted
    % display updates.
    % If performing many changes, such as a replace all command, the target can be used instead.
    % First, set the target, ie. the range to be replaced.
    % Then call replaceTarget or replaceTargetRE.
    % @end

predicates
    replaceTarget : (string Text).
    replaceTarget_utf8 : (string8 Text).
    replaceTarget : (string Text) -> integer ReplaceLengthUtf8.
    replaceTarget_utf8 : (string8 Text) -> integer ReplaceLengthUtf8.
    % @short
    % Replace the target with #Text.
    % After replacement, the target range refers to the replacement text.
    % The #ReplaceLengthUtf8 is the length of the replacement UTF8 string.
    % Note that the recommended way to delete text in the document is to set the target to the text to be removed, and to perform a replace target with an empty string.
    % @end

predicates
    replaceTargetRE : (string Text).
    replaceTargetRE_utf8 : (string8 Text).
    replaceTargetRE : (string Text) -> integer ReplaceLengthUtf8.
    replaceTargetRE_utf8 : (string8 Text) -> integer ReplaceLengthUtf8.
    % @short
    % This replaces the target using regular expressions.
    % The replacement string is formed from the #Text string with any sequences of \1 through \9 replaced by tagged
    % matches from the most recent regular expression search.
    % After replacement, the target range refers to the replacement text.
    % The #ReplaceLengthUtf8 is the length of the replacement UTF8 string.
    % @end

predicates
    getTag : (integer TagNumber) -> string TagValue.
    % getTag(int tagNumber, char *tagValue)
    % Discover what text was matched by tagged expressions in a regular expression search.
    % This is useful if the application wants to interpret the replacement string itself.

properties
    overType : boolean.
    % @short
    % Overtype mode.
    % @end

% Cut, copy and paste
predicates
    cut : ().
    copy : ().
    paste : ().
    % @short
    % cut/copy/paste
    % @end

predicates
    clear : ().
    % @short
    % clear
    % @end

properties
    canCut : boolean (o).
    canCopy : boolean (o).
    canPaste : boolean (o).
    % @short
    % Succeed if is it possible to cut/copy/paste (writable and something on clipboard).
    % @end

properties
    allowReadOnlyCut : boolean.
    % @short
    % If true (default) the editor will signal canCut when it is in read-only mode, allowing the modifyAttemptRO even to make the editor writable
    % @end

predicates
    copyAllowLine : ().
    % @short
    % copyAllowLine works the same as copy except that if the selection is empty then the current line is copied.
    % An extra "MSDEVLineSelect" marker is added to the clipboard which is then used in paste to paste the whole line before the current line.
    % @end

predicates
    copyRange : (integer Start, integer End).
    % @short
    % copyRange copies a range of text from the document to the system clipboard

predicates
    copyText : (string Text).
    copyText_utf8 : (string8 Text).
    % @short
    % copyText copies a supplied piece of #Text to the system clipboard.
    % @end

properties
    pasteConvertEndings : boolean.
    % @short
    % If this property is set then when text is pasted any line ends are converted to match the document's end of line mode as set with eolMode.
    % @end

% Error handling
properties
    status : integer.
    % SCI_GETSTATUS
    % If an error occurs, Scintilla may set an internal error number that can be retrieved with SCI_GETSTATUS. To clear the
    % error status call SCI_SETSTATUS(0). Status values from 1 to 999 are errors and status SC_STATUS_WARN_START (1000) and
    % above are warnings. The currently defined statuses are:
    %
    % SC_STATUS_OK
    % 0 No failures
    %
    % SC_STATUS_FAILURE
    % 1 Generic failure
    %
    % SC_STATUS_BADALLOC
    % 2 Memory is exhausted
    %
    % SC_STATUS_WARN_REGEX
    % 1001 Regular expression is invalid
    %

% Undo and Redo
predicates
    undo : ().
    % @short
    % undo undoes one runnable, or if the undo buffer has reached an endUndoAction point, all the actions back to the
    % corresponding beginUndoAction.
    % @detail
    % Scintilla has multiple level undo and redo.
    % It will continue to collect undoable actions until memory runs out.
    % Scintilla saves actions that change the document.
    % Scintilla does not save caret and selection movements, view scrolling and the like.
    % Sequences of typing or deleting are compressed into single transactions to make it easier to undo and redo at a sensible level of detail.
    % Sequences of actions can be combined into transactions that are undone as a unit.
    % These sequences occur between beginUndoAction and endUndoAction messages.
    % These transactions can be nested and only the top-level sequences are undone as units.
    % @end

predicates
    redo : ().
    % @short
    % undoes the effect of the last undo operation.
    % @end

properties
    canUndo : boolean (o).
    canRedo : boolean (o).
    % @short
    % succeeds if there is something to undo/redo.
    % You would typically use the result of this message to enable/disable the Edit menu Undo/Redo command.
    % @end

predicates
    emptyUndoBuffer : ().
    % @short
    % This command tells Scintilla to forget any saved undo or redo history.
    % It also sets the save point to the start of the undo buffer, so the document will appear to be unmodified.
    % This does not cause the scn_savepointReached notification to be sent to the container.
    % @end

properties
    undoCollection : boolean.
    % @short
    % Scintilla collects undo information if undoCollection is true.
    % The undo buffer is emptied when set to false.
    % @detail
    % You might wish to turn off saving undo information if you use the Scintilla to store text generated by a program (a Log view) or in a
    % display window where text is often deleted and regenerated.
    % @end

predicates
    beginUndoAction : ().
    endUndoAction : ().
    % @short
    % Mark the beginning/end of a set of operations that you want to undo all as one operation.
    % @detail
    % Alternatively, you can use these to mark a set of operations that you do not want to have combined with the preceding or following operations if they are undone.
    % @end

predicates
    addUndoAction : (integer Token, integer Flags).
    % @short
    % The container can add its own actions into the undo stack by calling addUndoAction and an scn_modified notification will be
    % sent to the container with the sc_mod_container flag when it is time to undo (sc_performed_undo) or redo
    % (sc_performed_redo) the runnable.
    % The token argument supplied is returned in the token field of the notification.
    % @detail
    % For example, if the container wanted to allow undo and redo of a 'toggle bookmark' command then it could call
    % addUndoAction(line, 0) each time the command is performed.
    % Then when it receives a notification to undo or redo it toggles a bookmark on the line given by the token field.
    % If there are different types of commands or parameters that need to be stored into the undo stack then the container should maintain a stack of its own for the
    % document and use the current position in that stack as the argument to addUndoAction(line).
    % addUndoAction commands are not combined togetHer into a single undo transaction unless grouped with beginUndoAction and endUndoAction.<br>
    % The flags argument can be undo_may_coalesce (1) if the container runnable may be coalesced along with any insertion and deletion
    % actions into a single compound runnable, otherwise 0.
    % Coalescing treats coalescible container actions as transparent so will still only group togetHer insertions that look like typing or deletions that look like multiple uses of
    % the Backspace or Delete keys.
    % @end

% Selection and information
properties
    lastPos : byteCount (o).
    % @short
    % The size of the internal text buffer. Notice that this is not the same as the length of the Unicode representation.
    % @end

properties
    lineCount : lineNumber (o).
    % @short
    % The number of lines in the editor.
    % An empty document contains 1 line.
    % A document holding only an end of line sequence has 2 lines.
    % @end

properties
    firstVisibleLine : lineNumber.
    % @short
    % set or get the line number of the first visible line in the Scintilla view.
    % The first line in the document is numbered 0.
    % The value is a visible line rather than a document line.
    % @end

properties
    linesOnScreen : integer (o).
    % @short
    % The number of complete lines visible on the screen.
    % With a constant line height, this is the vertical space available divided by the line separation.
    % Unless you arrange to size your window to an integral number of lines, there may be a partial line visible at the bottom of the view.
    % @end

predicates
    isModified : () determ.
    % @short
    % Succeeds if the document is modified
    % @detail
    % The modified status of a document is determined by the undo position relative to the save point.
    % The save point is set by setSavepoint, usually when you have saved data to a file.
    % If you need to be notified when the document becomes modified, Scintilla notifies the container that it has entered or left the save point
    % with the scn_savepointReached and scn_savepointLeft notification messages.
    % @end

predicates
    setSel : (integer AnchorPos, integer CurrentPos).
    % @short
    % Sets both the anchor and the current position.
    % If currentPos is negative, it means the end of the document.
    % If anchorPos is negative, it means remove any selection (i.e. set the anchor to the same position as currentPos).
    % The caret is scrolled into view after this operation.
    % @end

predicates
    gotoPos : (integer Pos).
    % @short
    % This removes any selection, sets the caret at pos and scrolls the view to make the caret visible, if necessary.
    % The anchor position is set the same as the current position.
    % @end

predicates
    gotoLine : (lineNumber Line).
    % @short
    % This removes any selection and sets the caret at the start of line number line and scrolls the view (if needed) to make it visible.
    % The anchor position is set the same as the current position.
    % If line is outside the lines in the document (first line is 0), the line set is the first or last.
    % @end

properties
    currentPos : integer.
    % @short
    % Gets the current position or sets the current position and creates a selection between the anchor and the current position.
    % The caret is not scrolled into view.
    % @end

properties
    anchor : integer.
    % @short
    % Gets the current anchor position or sets the anchor position and creates a selection between the anchor position and the current position.
    % The caret is not scrolled into view.
    % @end

properties
    selectionStart : integer.
    selectionEnd : integer.
    % @short
    % The start/end of the selection.
    % selectionStart is the smaller of the anchor point and the current position.
    % selectionEnd is the larger.
    % They do not make the caret visible.
    %<table cellpadding="3" cellspacing="0" border="1" summary="SetSelection caret positioning">
    %   <thead align="center">
    %   <tr>
    %       <th></th>
    %       <th>anchor</th>
    %       <th>current</th>
    %   </tr>
    %   </thead>
    %   <tbody align="center">
    %   <tr>
    %       <th><code>selectionStart</code></th>
    %       <td><code>pos</code></td>
    %       <td><code>max(pos, current)</code></td>
    %   </tr>
    %   <tr>
    %       <th><code>selectionStart</code></th>
    %       <td><code>min(anchor, pos)</code></td>
    %       <td><code>pos</code></td>
    %   </tr>
    %   </tbody>
    %</table>
    % @end

predicates
    setEmptySelection : (integer Pos).
    % @short
    % This removes any selection and sets the caret at pos.
    % The caret is not scrolled into view.
    % @end

predicates
    selectAll : ().
    % @short
    % This selects all the text in the document.
    % The current position is not scrolled into view.
    % @end

predicates
    lineFromPosition : (integer Pos) -> lineNumber Line.
    % @short
    % lineFromPosition returns the line that contains the position pos in the document.
    % @detail
    % The return value is 0 if pos <= 0.
    % The return value is the last line if pos is beyond the end of the document.
    % @end

predicates
    positionFromLine : (lineNumber Line) -> integer Pos.
    % @short
    % positionFromLine returns the document position that corresponds with the start of the line.
    % @detail
    % If line is negative, the position of the line holding the start of the selection is returned.
    % If line is greater than the lines in the document, the return value is -1.
    % If line is equal to the number of lines in the document (i.e. 1 line past the last line), the return value is the end of the document.
    % @end

predicates
    getLineEndPosition : (lineNumber Line) -> integer Pos.
    % @short
    % getLineEndPosition returns the position at the end of the line, before any line end characters.
    % @detail
    % If line is the last line in the document (which does not have any end of line characters), the result is the size of the document.
    % If line is negative or line >= lineCount, the result is undefined.
    % @end

predicates
    lineLength : (lineNumber Line) -> integer Length.
    % @short
    % lineLength returns the length of the line, including any line end characters.
    % @detail
    % If line is negative or beyond the last line in the document, the result is 0.
    % If you want the length of the line not including any end of line characters, use getLineEndPosition(Line) - positionFromLine(Line).
    % @end

properties
    selText : string (o).
    selTextMultiple : string (o).
    % @short
    % The currently selected text.
    % See Multiple Selection for information on how multiple and rectangular selections and virtual space are copied.
    % @end

properties
    curLine : string (o).
    % @short
    % The line containing the caret.
    % @end

predicates
    selectionIsRectangle : () determ.
    % @short
    % Succeeds if the current selection is in rectangle mode.
    % @end

properties
    selectionMode : integer.
    % @short
    % Set and get the selection mode, which can be stream (sc_sel_stream=0) or rectangular (sc_sel_rectangle=1)
    % or by lines (sc_sel_lines=2) or thin rectangular (sc_sel_thin=3).
    % @detail
    % When set in these modes, regular caret moves will extend or reduce the selection, until the mode is cancelled by a call with same value or with cancel.
    % The property returns the current mode even if the selection was made by mouse or with regular extended moves.
    % sc_sel_thin is the mode after a rectangular selection has been typed into and ensures that no characters are selected.
    % @end

predicates
    tryGetLineSelStartPosition : (lineNumber Line) -> integer Pos determ.
    tryGetLineSelEndPosition : (lineNumber Line) -> integer Pos determ.
    % @short
    % Retrieve the position of the start/end of the selection at the given #Line.
    % Fails if there is no selection on this #Line.
    % @end

predicates
    moveCaretInsideView : ().
    % @short
    % If the caret is off the top or bottom of the view, it is moved to the nearest line that is visible to its current position.
    % Any selection is lost.
    % @end

predicates
    wordEndPosition : (integer Position, boolean OnlyWordCharacters = true) -> integer Pos.
    wordStartPosition : (integer Position, boolean OnlyWordCharacters = true) -> integer Pos.
    % @short
    % return the start and end of words using the same definition of words as used internally within Scintilla.
    % @detail
    % You can set your own list of characters that count as words with the wordchars property.
    % The position sets the start or the search, which is forwards when searching for the end and backwards when searching for the start.

predicates
    adjustPositionStart : (integer Position) -> integer Adjusted.
    % @short
    % #Adjsted will point to the start of the character that #Position points to.  So if #Position points to the middle of an utf8 character #Adjusted will point to the start of that character.
    % @end

predicates
    adjustPositionNext : (integer Position) -> integer Adjusted.
    % @short
    % If #Position points to the inner of an utf8 character, then Adjusted will point to the start of the next character.
    % @end

% Set #OnlyWordCharacters to true to stop searching at the first non-word character in the search direction.
% If #OnlyWordCharacters is false, the first character in the search direction sets the type of the search as word or non-word and the search stops at the first non-matching character.
% Searches are also terminated by the start or end of the document.
% If "w" represents word characters and "." represents non-word characters and "|" represents the position and true or false is the state of onlyWordCharacters:
%<table border="1" cellSpacing="0" summary="Word start and end positions" cellPadding="3">
%   <thead align="middle">
%       <tr><th>Initial state</th><th>end, true</th><th>end, false</th><th>start, true</th><th>start, false</th></tr>
%   </thead>
%   <tbody align="middle">
%       <tr><td>..ww..|..ww..</td><td>..ww..|..ww..</td><td>..ww....|ww..</td><td>..ww..|..ww..</td><td>..ww|....ww..</td></tr>
%       <tr><td>....ww|ww....</td><td>....wwww|....</td><td>....wwww|....</td><td>....|wwww....</td><td>....|wwww....</td></tr>
%       <tr><td>..ww|....ww..</td><td>..ww|....ww..</td><td>..ww....|ww..</td><td>..|ww....ww..</td><td>..|ww....ww..</td></tr>
%       <tr><td>..ww....|ww..</td><td>..ww....ww|..</td><td>..ww....ww|..</td><td>..ww....|ww..</td><td>..ww|....ww..</td></tr>
%   </tbody>
%</table>
% @end
predicates
    tryPositionBefore : (integer Position) -> integer Before determ.
    tryPositionAfter : (integer Position) -> integer After determ.
    % @short
    % Returns the position #Before/#After another #Position taking into account varying length of UTF-8 characters.
    % The minimum position returned is 0 and the maximum is the last position in the document.
    % If called with a position within a multi byte character will return the position of the start/end of that character.
    % @end

predicates
    positionRelative : (integer Position, integer Offset) -> integer Relative.
    % SCI_POSITIONRELATIVE(int position, int relative)
    % Count a number of whole characters before or after the argument position and return that position. The minimum position
    % returned is 0 and the maximum is the last position in the document. If the position goes past the document end then 0 is
    % returned.
    %

predicates
    countCharacters : (integer StartPos, integer EndPos) -> charCount Count.
    % @short
    % Returns the number of whole characters between two positions..
    % @end

predicates
    textWidth : (sci_style StyleNumber, string Text) -> integer PixelWidth.
    textWidth_utf8 : (sci_style StyleNumber, string8 Text) -> integer PixelWidth.
    % @short
    % textWidth returns the pixel width of a string drawn in the given styleNumber which can be used, for example, to decide how wide to make the
    % line number margin in order to display a given number of numerals.
    % @end

predicates
    textHeight : (lineNumber Line) -> integer PixelHeight.
    % @short
    % textHeight returns the height in pixels of a particular line.
    % Currently all lines are the same height.
    % @end

predicates
    getColumn : (integer Pos) -> linePosition Column.
    % @short
    % getColumn returns the #Column number of a position #Pos within the document taking the width of tabs into account.
    % @detail
    % This returns the column number of the last tab on the line before pos, plus the number of characters between the last tab and pos.
    % If there are no tab characters on the line, the return value is the number of characters up to the position on the line.
    % In both cases, multi-byte characters count as a single character.
    % This is probably only useful with monospaced fonts.
    % @end

predicates
    findColumn : (lineNumber Line, linePosition Column) -> integer Position.
    % @short
    % findColumn returns the #Position of a #Column on a line taking the width of tabs into account.
    % It treats a multi-byte character as a single column.
    % Bot column and line start a 1.
    % @end

predicates
    positionFromPoint : (integer X, integer Y) -> integer Pos.
    tryPositionFromPointClose : (integer X, integer Y) -> integer Pos determ.
    % @short
    % positionFromPoint finds the closest character position to a point.
    % tryPositionFromPointClose is similar but fails if the point is outside the window or not close to any characters.
    % @end

predicates
    charPositionFromPoint : (integer X, integer Y) -> integer Pos.
    tryCharPositionFromPointClose : (integer X, integer Y) -> integer Pos determ.
    % @short
    % charPositionFromPoint finds the closest character to a point.
    % tryCharPositionFromPointClose is similar but fails  if the point is outside the window or not close to any characters.
    % This is similar to positionFromPoint/positionFromPoint but finds characters rather than inter-character positions.
    % @end

predicates
    pointXFromPosition : (integer Pos) -> integer X.
    pointYFromPosition : (integer Pos) -> integer Y.
    % @short
    % Returns the x and y display pixel location of text at position Pos in the document.
    % @end

predicates
    hideSelection : (boolean Hide).
    % @short
    % The normal state is to make the selection visible by drawing it as set by SCI_SETSELFORE and SCI_SETSELBACK.
    % However, if you hide the selection, it is drawn as normal text.
    % @end

predicates
    chooseCaretX : ().
    % Scintilla remembers the x value of the last position horizontally moved to explicitly by the user and this value is then used when moving
    % vertically such as by using the up and down keys.
    % This message sets the current x position of the caret as the remembered value.

predicates
    moveSelectedLinesUp : ().
    % SCI_MOVESELECTEDLINESUP
    % Move the selected lines up one line, shifting the line above after the selection. The selection will be automatically extended to the
    % beginning of the selection's first line and the end of the seletion's last line. If nothing was selected, the line the cursor is currently at will
    % be selected.

predicates
    moveSelectedLinesDown : ().
    % SCI_MOVESELECTEDLINESDOWN
    % Move the selected lines down one line, shifting the line below before the selection. The selection will be automatically extended to the
    % beginning of the selection's first line and the end of the seletion's last line. If nothing was selected, the line the cursor is currently at will
    % be selected.
    %
    % Multiple Selection and Virtual Space
    % There may be multiple selections active at one time.
    % More selections are made by holding down the Ctrl key while dragging with the mouse.
    % The most recent selection is the main selection and determines which part of the document is shown automatically.
    % Any selection apart from the main selection is called an additional selection.
    %
    % The calls in the previous section operate on the main selection.
    % There is always at least one selection.
    % Rectangular selections are handled as multiple selections although the original rectangular range is remembered so that subsequent
    % operations may be handled differently for rectangular selections.
    % For example, pasting a rectangular selection places each piece in a vertical column.
    % Virtual space is space beyond the end of each line.
    %
    % The caret may be moved into virtual space but no real space will be added to the document until there is some text typed or some other text insertion command is used.
    %
    % When discontiguous selections are copied to the clipboard, each selection is added to the clipboard text in order with no delimiting characters.
    % For rectangular selections the document's line end is added after each line's text.
    % Rectangular selections are always copied from top line to bottom, not in the in order of selection.Virtual space is not copied.

properties
    multipleSelection : boolean.
    % Enable or disable multiple selection.

properties
    additionalSelectionTyping : boolean.
    % Whether typing, backspace, or delete works with multiple selections simultaneously.

properties
    multiPaste : integer.
    % When pasting into multiple selections, the pasted text can go into just the main selection with SC_MULTIPASTE_ONCE=0 or into each
    % selection with SC_MULTIPASTE_EACH=1.
    % SC_MULTIPASTE_ONCE is the default.

properties
    virtualSpaceOptions : integer.
    % Virtual space can be enabled or disabled for rectangular selections or in other circumstances or in both.
    % There are two bit flags SCVS_RECTANGULARSELECTION=1 and SCVS_USERACCESSIBLE=2 which can be set independently.
    % SCVS_NONE=0, the default, disables all use of virtual space.

properties
    selections : integer (o).
    % Return the number of selections currently active.

predicates
    clearSelections : ().
    % Set a single empty selection at 0 as the only selection.

predicates
    setSelection : (integer Caret, integer Anchor).
    % Set a single selection from anchor to caret as the only selection.

predicates
    addSelection : (integer Caret, integer Anchor).
    % Add a new selection from anchor to caret as the main selection retaining all other selections as additional selections.
    % Since there is always at least one selection, to set a list of selections, the first selection should be added with SCI_SETSELECTION and later selections
    % added with SCI_ADDSELECTION

properties
    mainSelection : integer.
    % One of the selections is the main selection which is used to determine what range of text is automatically visible.
    % The main selection may be displayed in different colours or with a differently styled caret.
    % Only an already existing selection can be made main.

predicates
    setSelectionNCaret : (integer Selection, integer Pos).

predicates
    getSelectionNCaret : (integer Selection) -> integer Pos.

predicates
    setSelectionNCaretVirtualSpace : (integer Selection, integer Space).

predicates
    getSelectionNCaretVirtualSpace : (integer Selection) -> integer Space.

predicates
    setSelectionNAnchor : (integer Selection, integer PosAnchor).

predicates
    getSelectionNAnchor : (integer Selection) -> integer PosAnchor.

predicates
    setSelectionNAnchorVirtualSpace : (integer Selection, integer Space).

predicates
    getSelectionNAnchorVirtualSpace : (integer Selection) -> integer Space.
    % Set or query the position and amount of virtual space for the caret and anchor of each already existing selection.

predicates
    setSelectionNStart : (integer Selection, integer Pos).

predicates
    getSelectionNStart : (integer Selection) -> integer Pos.

predicates
    setSelectionNEnd : (integer Selection, integer Pos).

predicates
    getSelectionNEnd : (integer Selection) -> integer Pos.
    % Set or query the start and end position of each already existing selection.
    % Mostly of use to query each range for its text.

properties
    rectangularSelectionCaret : integer.

properties
    rectangularSelectionCaretVirtualSpace : integer.

properties
    rectangularSelectionAnchor : integer.

properties
    rectangularSelectionAnchorVirtualSpace : integer.
    % Set or query the position and amount of virtual space for the caret and anchor of the rectangular selection.
    % After setting the rectangular selection, this is broken down into multiple selections, one for each line.

properties
    additionalSelAlpha : integer.

predicates
    setAdditionalSelFore : (unsigned Color).

predicates
    setAdditionalSelBack : (unsigned Color).
    % Modify the appearence of additional selections so that they can be differentiated from the main selection which has its appearence set
    % with SCI_SETSELALPHA, SCI_GETSELALPHA, SCI_SETSELFORE, and SCI_SETSELBACK.

properties
    additionalCaretFore : unsigned.

properties
    additionalCaretsBlink : boolean.
    % Modify the appearence of additional carets so that they can be differentiated from the main caret which has its appearence set with
    % SCI_SETCARETFORE, SCI_GETCARETFORE, SCI_SETCARETPERIOD, and SCI_GETCARETPERIOD.

properties
    additionalCaretsVisible : boolean.
    % Determine whether to show additional carets (defaults to true).

predicates
    swapMainAnchorCaret : ().

predicates
    rotateSelection : ().
    % These commands may be assigned to keys to make it possible to manipulate multiple selections.
    % SCI_SWAPMAINANCHORCARET moves the caret to the opposite end of the main selection.
    % SCI_ROTATESELECTION makes the next selection be the main selection.

% Scrolling and automatic scrolling
predicates
    lineScroll : (linePosition Column, lineNumber Line).
    % This will attempt to scroll the display by the number of columns and lines that you specify.
    % Positive line values increase the line number at the top of the screen (i.e. they move the text upwards as far as the user is concerned), Negative line values do the reverse.
    %
    % The column measure is the width of a space in the default style.
    % Positive values increase the column at the left edge of the view (i.e. they move the text leftwards as far as the user is concerned).
    % Negative values do the reverse.
    %
    % See also: SCI_SETXOFFSET
    %

predicates
    scrollCaret : ().
    % If the current position (this is the caret if there is no selection) is not visible, the view is scrolled to make it visible according to the
    % current caret policy.

predicates
    setXCaretPolicy : (integer CaretPolicy, integer CaretSlop).
    setYCaretPolicy : (integer CaretPolicy, integer CaretSlop).
    % These set the caret policy.
    % The value of caretPolicy is a combination of CARET_SLOP, CARET_STRICT, CARET_JUMPS and CARET_EVEN.
    %
    % CARET_SLOP
    % If set, we can define a slop value: caretSlop. This value defines an unwanted zone (UZ) where the caret is... unwanted.
    % This zone is defined as a number of pixels near the vertical margins, and as a number of lines near the horizontal
    % margins. By keeping the caret away from the edges, it is seen within its context. This makes it likely that the
    % identifier that the caret is on can be completely seen, and that the current line is seen with some of the lines
    % following it, which are often dependent on that line.
    %
    % CARET_STRICT
    % If set, the policy set by CARET_SLOP is enforced... strictly. The caret is centred on the display if caretSlop is not
    % set, and cannot go in the UZ if caretSlop is set.
    %
    % CARET_JUMPS
    % If set, the display is moved more energetically so the caret can move in the same direction longer before the policy is
    % applied again. '3UZ' notation is used to indicate three time the size of the UZ as a distance to the margin.
    %
    % CARET_EVEN
    % If not set, instead of having symmetrical UZs, the left and bottom UZs are extended up to right and top UZs
    % respectively. This way, we favour the displaying of useful information: the beginning of lines, where most code reside,
    % and the lines after the caret, for example, the body of a function.
    %
    % slop
    %
    % strict
    %
    % jumps
    %
    % even
    %
    % Caret can go to the margin
    %
    % On reaching limit (going out of visibility
    % or going into the UZ) display is...
    %
    % 0 0 0 0 Yes moved to put caret on top/on right
    % 0 0 0 1 Yes moved by one position
    % 0 0 1 0 Yes moved to put caret on top/on right
    % 0 0 1 1 Yes centred on the caret
    % 0 1 - 0 Caret is always on top/on right of display -
    % 0 1 - 1 No, caret is always centred -
    % 1 0 0 0 Yes moved to put caret out of the asymmetrical UZ
    % 1 0 0 1 Yes moved to put caret out of the UZ
    % 1 0 1 0 Yes moved to put caret at 3UZ of the top or right margin
    % 1 0 1 1 Yes moved to put caret at 3UZ of the margin
    % 1 1 - 0 Caret is always at UZ of top/right margin -
    % 1 1 0 1 No, kept out of UZ moved by one position
    % 1 1 1 0 No, kept out of UZ moved to put caret at 3UZ of the margin
    %

predicates
    setVisiblePolicy : (integer CaretPolicy, integer CaretSlop).
    % This determines how the vertical positioning is determined when SCI_ENSUREVISIBLEENFORCEPOLICY is called.
    % It takes VISIBLE_SLOP and VISIBLE_STRICT flags for the policy parameter.
    % It is similar in operation to SCI_SETYCARETPOLICY(int caretPolicy, int caretSlop).

properties
    hScrollbar : boolean.
    % The horizontal scroll bar is only displayed if it is needed for the assumed width.
    % If you never wish to see it, call SCI_SETHSCROLLBAR(0).
    % Use SCI_SETHSCROLLBAR(1) to enable it again.
    % SCI_GETHSCROLLBAR returns the current state.
    % The default state is to display it when needed.
    %
    % See also: SCI_SETSCROLLWIDTH.
    %

properties
    vScrollbar : boolean.
    % By default, the vertical scroll bar is always displayed when required.
    % You can choose to hide or show it with SCI_SETVSCROLLBAR and get the current state with SCI_GETVSCROLLBAR.
    %

properties
    xOffset : integer.
    % The xOffset is the horizontal scroll position in pixels of the start of the text view.
    % A value of 0 is the normal position with the first text column visible at the left of the view.
    %
    % See also: SCI_LINESCROLL
    %

properties
    scrollWidth : integer.
    % For performance, Scintilla does not measure the display width of the document to determine the properties of the horizontal scroll bar.
    % Instead, an assumed width is used.
    % These messages set and get the document width in pixels assumed by Scintilla.
    % The default value is
    % 2000.
    % To ensure the width of the currently visible lines can be scrolled use SCI_SETSCROLLWIDTHTRACKING
    %

properties
    scrollWidthTracking : boolean.
    % If scroll width tracking is enabled then the scroll width is adjusted to ensure that all of the lines currently displayed can be completely
    % scrolled.
    % This mode never adjusts the scroll width to be narrower.

properties
    endatLastLine : boolean.
    % SCI_SETENDATLASTLINE sets the scroll range so that maximum scroll position has the last line at the bottom of the view (default).
    % Setting this to false allows scrolling one page below the last line.

% White space
properties
    viewWs : integer.
    % White space can be made visible which may be useful for languages in which white space is significant, such as Python.
    % Space characters appear as small centred dots and tab characters as light arrows pointing to the right.
    % There are also ways to control the display of end of line characters.
    % The two messages set and get the white space display mode.
    % The wsMode argument can be one of:
    %
    % SCWS_INVISIBLE
    % 0 The normal display mode with white space displayed as an empty background colour.
    %
    % SCWS_VISIBLEALWAYS
    % 1 White space characters are drawn as dots and arrows,
    %
    % SCWS_VISIBLEAFTERINDENT
    % 2 White space used for indentation is displayed normally but after the first visible character, it is shown as dots and
    % arrows.
    %
    % SCWS_VISIBLEONLYININDENT
    % 3 White space used for indentation is displayed as dots and arrows.
    %
    % The effect of using any other wsMode value is undefined.
    %

predicates
    setWhiteSpaceFore : (boolean UseWhiteSpaceForeColor, unsigned Color).

predicates
    setWhiteSpaceBack : (boolean UseWhiteSpaceBackColor, unsigned Color).
    % By default, the colour of visible white space is determined by the lexer in use.
    % The foreground and/or background colour of all visible white space can be set globally, overriding the lexer's colours with SCI_SETWHITESPACFORE and SCI_SETWHITESPACEBACK.

properties
    whiteSpaceSize : integer.
    % SCI_SETWHITESPACESIZE sets the size of the dots used for mark space characters.
    % The SCI_GETWHITESPACESIZE message retrieves the current size.

properties
    extraAscent : integer.

properties
    extraDescent : integer.
    % Text is drawn with the base of each character on a 'baseline'.
    % The height of a line is found from the maximum that any style extends above the baseline (its 'ascent'), added to the maximum that any style extends below the baseline (its 'descent').
    % Space may be added to the maximum ascent (SCI_SETEXTRAASCENT) and the maximum descent (SCI_SETEXTRADESCENT) to allow for more space
    % between lines.
    % This may done to make the text easier to read or to accomodate underlines or highLights.
    %

% Cursor
properties
    cursor : integer.
    % The cursor is normally chosen in a context sensitive way, so it will be different over the margin than when over the text.
    % When performing a slow runnable, you may wish to change to a wait cursor.
    % You set the cursor type with SCI_SETCURSOR.
    % The curType argument can be:

% SC_CURSORNORMAL -1 The normal cursor is displayed.
% SC_CURSORWAIT  4 The wait cursor is displayed when the mouse is over or owned by the Scintilla window.
% Cursor values 1 through 7 have defined cursors, but only SC_CURSORWAIT is usefully controllable.
% Other values of curType cause a pointer to be displayed.
% The SCI_GETCURSOR message returns the last cursor type you set, or SC_CURSORNORMAL (-1) if you have not set a cursor type.
% Mouse capture
properties
    mousedownCaptures : boolean.
    % When the mouse is pressed inside Scintilla, it is captured so future mouse movement events are sent to Scintilla.
    % This behavior may be turned off with SCI_SETMOUSEDOWNCAPTURES(0).

% Line endings
%
% Scintilla can interpret any of the three major line end conventions, Macintosh (\r), Unix (\n) and CP/M / DOS / Windows (\r\n).
%
% When the user presses the Enter key, one of these line end strings is inserted into the buffer.
% The default is \r\n in Windows and \n in Unix, but this can be changed with the SCI_SETEOLMODE message.
% You can also convert the entire document to one of these line endings with SCI_CONVERTEOLS.
% Finally, you can choose to display the line endings with SCI_SETVIEWEOL.
properties
    eolMode : integer.
    % SCI_SETEOLMODE sets the characters that are added into the document when the user presses the Enter key.
    % You can set eolMode to one of SC_EOL_CRLF (0), SC_EOL_CR (1), or SC_EOL_LF (2).
    % The SCI_GETEOLMODE message retrieves the current state.

predicates
    convertEols : (integer EolMode).
    % This message changes all the end of line characters in the document to match eolMode.
    % Valid values are: SC_EOL_CRLF (0), SC_EOL_CR (1), or SC_EOL_LF (2).

properties
    viewEol : boolean.
    % Normally, the end of line characters are hidden, but SCI_SETVIEWEOL allows you to display (or hide) them by setting visible true (or
    % false).
    % The visible rendering of the end of line characters is similar to (CR), (LF), or (CR)(LF).
    % SCI_GETVIEWEOL returns the current state.

% Styling
%
% The styling messages allow you to assign styles to text.
% The standard Scintilla settings divide the  style bits available for each character into 5 bits (0 to 4 = styles 0 to 31) that set a style and three bits (5 to 7) that define indicators.
% You can change the balance between styles and indicators with SCI_SETSTYLEBITS.
% If your styling needs can be met by one of the standard lexers, or if you can write your own, then a lexer is probably the easiest way to style your document.
% If you choose to use the container to do the styling you can use the SCI_SETLEXER command to select SCLEX_CONTAINER, in which case the container is sent a SCN_STYLENEEDED
% notification each time text needs styling for display.
% As another alternative, you might use idle time to style the document.
% Even if you use a lexer, you might use the styling commands to mark errors detected by a compiler.
% The following commands can be used.
properties
    endStyled : integer (o).
    % Scintilla keeps a record of the last character that is likely to be styled correctly.
    % This is moved forwards when characters after it are styled and moved backwards if changes are made to the text of the document before it.
    % Before drawing text, this position is checked to see if any styling is needed and, if so, a SCN_STYLENEEDED notification message is sent to the container.
    % The container can send SCI_GETENDSTYLED to work out where it needs to start styling.
    % Scintilla will always ask to style whole lines.

predicates
    startStyling : (integer Pos, integer Mask).
    % This prepares for styling by setting the styling position pos to start at and a mask indicating which bits of the style bytes can be set.
    % The mask allows styling to occur over several passes, with, for example, basic styling done on an initial pass to ensure that the text of the
    % code is seen quickly and correctly, and then a second slower pass, detecting syntax errors and using indicators to show where these
    % are.
    % For example, with the standard settings of 5 style bits and 3 indicator bits, you would use a mask value of 31 (0x1f) if you were setting
    % text styles and did not want to change the indicators.
    % After SCI_STARTSTYLING, send multiple SCI_SETSTYLING messages for each lexical entity to style.

predicates
    setStyling : (integer Length, sci_style Style).
    % This message sets the style of length characters starting at the styling position and then increases the styling position by length, ready
    % for the next call.
    % If sCell is the style byte, the operation is: if ((sCell & mask) != style) sCell = (sCell & ~mask) | (style & mask);

predicates
    setStylingeX : (integer Length, string8 Styles).
    % As an alternative to SCI_SETSTYLING, which applies the same style to each byte, you can use this message which specifies the styles
    % for each of length bytes from the styling position and then increases the styling position by length, ready for the next call.
    % The length styling bytes pointed at by styles should not contain any bits not set in mask.

predicates
    setLineState : (lineNumber Line, integer Value).
    % By default, SC_IDLESTYLING_NONE (0), syntax styling is performed for all the currently visible text before displaying it.
    % On very large files, this may make scrolling down slow. With SC_IDLESTYLING_TOVISIBLE (1), a small amount of styling
    % is performed before display and then further styling is performed incrementally in the background as an idle-time task.
    % This may result in the text initially appearing uncoloured and then, some time later, it is coloured. Text after the
    % currently visible portion may be styled in the background with SC_IDLESTYLING_AFTERVISIBLE (2). To style both before and
    % after the visible text in the background use SC_IDLESTYLING_ALL (3).
    %
    % Since wrapping also needs to perform styling and also uses idle time, this setting has no effect when the document is
    % displayed wrapped.
    %

predicates
    getLineState : (lineNumber Line) -> integer Value.
    % As well as the  bits of lexical state stored for each character there is also an integer stored for each line.
    % This can be used for longer lived parse states such as what the current scripting language is in an ASP page.
    % Use SCI_SETLINESTATE to set the integer value and SCI_GETLINESTATE to get the value.
    % Changing the value produces a SC_MOD_CHANGELINESTATE notification.

properties
    maxLineState : lineNumber (o).
    % This returns the last line that has any line state.

% Style definition
%
% While the style setting messages mentioned above change the style numbers associated with text, these messages define how those
% style numbers are interpreted visually.
% There are 256 lexer styles that can be set, numbered 0 to STYLE_MAX (255).
% Unless you use SCI_SETSTYLEBITS to change the number of style bits, styles 0 to 31 are used to set the text attributes.
% There are also some predefined numbered styles starting at 32, The following STYLE_* constants are defined.
%
% STYLE_DFAULT
% 32 This style defines the attributes that all styles receive when the
% SCI_STYLECLEARALL message is used.
%
% STYLE_LINENUMBER
% 33 This style sets the attributes of the text used to display line numbers
% in a line number margin.
% The background colour set for this style also sets the background
% colour for all margins that do not have any folding mask bits set.
% That is, any margin for which mask & SC_MASK_FOLDERS is 0.
% See SCI_SETMARGINMASKN for more about masks.
%
% STYLE_BRACELIGHT
% 34 This style sets the attributes used when highLighting braces with the SCI_BRACEHIGHLIGHT message and when highLighting the
% corresponding indentation with SCI_SETHIGHLIGHTGUIDE.
%
% STYLE_BRACEBAD
% 35 This style sets the display attributes used when marking an unmatched brace with the SCI_BRACEBADLIGHT
% message.
%
% STYLE_CONTROLCHAR
% 36 This style sets the font used when drawing control characters.
% Only the font, size, bold, italics, and character set attributes are used and not the colour attributes.
% See also: SCI_SETCONTROLCHARSYMBOL.
%
% STYLE_INDENTGUIDE
% 37 This style sets the foreground and background colours used when drawing the indentation guides.
%
% STYLE_CALLTIP
% 3 Call tips normally use the font attributes defined by STYLE_DFAULT.
% Use of SCI_CALLTIPUSESTYLE causes call tips to use this style instead.
% Only the font face name, font size, foreground and background colours and character set attributes are used.
%
% STYLE_LASTPREDFINED
% 39 To make it easier for client code to discover the range of styles that are predefined, this is set to the style number of the last predefined style.
% This is currently set to 39 and the last style with an identifier is 3, which reserves space for one future predefined style.
%
% STYLE_MAX
% 255 This is not a style but is the number of the maximum style that can be set.
% Styles between STYLE_LASTPREDFINED and STYLE_MAX would be appropriate if you used SCI_SETSTYLEBITS to set more than 5 style bits.
%
% For each style you can set the font name, size and use of bold, italic and underline, foreground and background colour and the
% character set.
% You can also choose to hide text with a given style, display all characters as upper or lower case and fill from the last character on a
% line to the end of the line (for embedded languages).
% There is also an experimental attribute to make text read-only.
%
% It is entirely up to you how you use styles.
% If you want to use syntax colouring you might use style 0 for white space, style 1 for numbers, style 2 for keywords, style 3 for strings, style 4
% for preprocessor, style 5 for operators, and so on.
%
predicates
    styleResetDefault : ().
    % This message resetS STYLE_DFAULT to its state when Scintilla was initialised.

predicates
    styleClearAll : ().
    % This message sets all styles to have the same attributes as STYLE_DFAULT.
    % If you are setting up Scintilla for syntax colouring, it is likely that the lexical styles you set will be very similar.
    % One way to set the styles is to:
    % 1.
    % Set STYLE_DFAULT to the common features of all styles.
    % 2.
    % Use SCI_STYLECLEARALL to copy this to all styles.
    % 3.
    % Set the style attributes that make your lexical styles different.

predicates
    styleSetFont : (sci_style StyleNumber, string FontName).
    styleGetFont : (sci_style StyleNumber) -> string FontName.

predicates
    styleSetSize : (sci_style StyleNumber, integer SizeInPoints).
    styleGetSize : (sci_style StyleNumber) -> integer SizeInPoints.

predicates
    styleSetSizeFractional : (sci_style StyleNumber, integer SizeInPoints).
    styleGetSizeFractional : (sci_style StyleNumber) -> integer SizeInPoints.

predicates
    styleSetBold : (sci_style StyleNumber, boolean Bold).
    styleGetBold : (sci_style StyleNumber) -> boolean Bold.

predicates
    styleSetWeight : (sci_style StyleNumber, integer Weight).
    styleGetWeight : (sci_style StyleNumber) -> integer Weight.

predicates
    styleSetItalic : (sci_style StyleNumber, boolean Italic).
    styleGetItalic : (sci_style StyleNumber) -> boolean Italic.
    % These messages (plus SCI_STYLESETCHARACTERSET) set the font attributes that are used to match the fonts you request to
    % those available.
    %
    % The fontName is a zero terminated string holding the name of a font. Under Windows, only the first 32 characters of the
    % name are used, the name is decoded as UTF-8, and the name is not case sensitive. For internal caching, Scintilla tracks
    % fonts by name and does care about the casing of font names, so please be consistent. On GTK+, Pango is used to display
    % text and the name is sent directly to Pango without transformation. On Qt, the name is decoded as UTF-8. On Cocoa, the
    % name is decoded as MacRoman.
    %
    % Sizes can be set to a whole number of points with SCI_STYLESETSIZE or to a fractional point size in hundredths of a
    % point with SCI_STYLESETSIZEFRACTIONAL by multiplying the size by 100 (SC_FONT_SIZE_MULTIPLIER). For example, a text size
    % of 9.4 points is set with SCI_STYLESETSIZEFRACTIONAL(<style>, 940).
    %
    % The weight or boldness of a font can be set with SCI_STYLESETBOLD or SCI_STYLESETWEIGHT. The weight is a number between
    % 1 and 999 with 1 being very light and 999 very heavy. While any value can be used, fonts often only support between 2
    % and 4 weights with three weights being common enough to have symbolic names: SC_WEIGHT_NORMAL (400), SC_WEIGHT_SEMIBOLD
    % (600), and SC_WEIGHT_BOLD (700). The SCI_STYLESETBOLD message takes a boolean argument with 0 choosing SC_WEIGHT_NORMAL
    % and 1 SC_WEIGHT_BOLD.
    %

predicates
    styleSetUnderline : (sci_style StyleNumber, boolean UnderLine).
    styleGetUnderline : (sci_style StyleNumber) -> boolean Italic.
    % You can set a style to be underlined.
    % The underline is drawn in the foreground colour.
    % All characters with a style that includes the underline attribute are underlined, even if they are white space.

predicates
    styleSetFore : (sci_style StyleNumber, unsigned Color).
    styleGetFore : (sci_style StyleNumber) -> unsigned Color.

predicates
    styleSetBack : (sci_style StyleNumber, unsigned Color).
    styleGetBack : (sci_style StyleNumber) -> unsigned Color.
    % Text is drawn in the foreground colour.
    % The space in each character cell that is not occupied by the character is drawn in the background colour.

predicates
    styleSetEolFilled : (sci_style StyleNumber, boolean EolFilled).
    styleGetEolFilled : (sci_style StyleNumber) -> boolean EolFilled.
    % If the last character in the line has a style with this attribute set, the remainder of the line up to the right edge of the window is filled
    % with the background colour set for the last character.
    % This is useful when a document contains embedded sections in another language such as HTML pages with embedded JavaScript.
    % By setting eolFilled to true and a consistent background colour (different from the background colour set for the HTML styles) to all JavaScript styles then
    % JavaScript sections will be easily distinguished from HTML.

%%%% This editor only uses utf-8
% You can set a style to use a different character set than the default.
% The places where such characters sets are likely to be useful are comments and literal strings.
% For example, SCI_STYLESETCHARACTERSET(SCE_C_STRING, SC_CHARSET_RUSSIAN) would ensure that strings in Russian would display correctly in C and C++ (SCE_C_STRING
% is the style number used by the C and C++ lexer to
% display literal strings; it has the value 6).
% This feature works differently on Windows and GTK+.
% The character sets supported on Windows are:
% SC_CHARSET_ANSI, SC_CHARSET_ARABIC, SC_CHARSET_BALTIC, SC_CHARSET_CHINESEBIG5, SC_CHARSET_DFAULT,
% SC_CHARSET_EASTEUROPE, SC_CHARSET_GB2312, SC_CHARSET_GREEK, SC_CHARSET_HANGUL, SC_CHARSET_HEBREW,
% SC_CHARSET_JOHAB, SC_CHARSET_MAC, SC_CHARSET_OEM, SC_CHARSET_RUSSIAN (code page 1251), SC_CHARSET_SHIFTJIS,
% SC_CHARSET_SYMBOL, SC_CHARSET_THAI, SC_CHARSET_TURKISH, and SC_CHARSET_VIETNAMESE.
% The character sets supported on GTK+ are:
% SC_CHARSET_ANSI, SC_CHARSET_CYRILLIC (code page 1251), SC_CHARSET_EASTEUROPE, SC_CHARSET_GB2312,
% SC_CHARSET_HANGUL, SC_CHARSET_RUSSIAN (KOI-R), SC_CHARSET_SHIFTJIS, and SC_CHARSET_59_15.
predicates
    styleSetCase : (sci_style StyleNumber, integer CaseMode).
    styleGetCase : (sci_style StyleNumber) -> integer CaseMode.
    % The value of caseMode determines how text is displayed.
    % You can set upper case (SC_CASE_UPPER, 1) or lower case
    % (SC_CASE_LOWER, 2) or display normally (SC_CASE_MIXED, 0).
    % This does not change the stored text, only how it is displayed.

predicates
    styleSetVisible : (sci_style StyleNumber, boolean Visible).
    styleGetVisible : (sci_style StyleNumber) -> boolean Visible.
    % Text is normally visible.
    % However, you can completely hide it by giving it a style with the visible set to 0.
    % This could be used to hide embedded formatting instructions or hypertext keywords in HTML or XML.

predicates
    styleSetChangeable : (sci_style StyleNumber, boolean Changeable).
    styleGetChangeable : (sci_style StyleNumber) -> boolean Changeable.
    % This is an experimental and incompletely implemented style attribute.
    % The default setting is changeable set true but when set false it makes text read-only.
    % Currently it only stops the caret from being within not-changeable text and does not yet stop deleting a range that contains not-changeable text.

predicates
    styleSetHotSpot : (sci_style StyleNumber, boolean HotSpot).
    styleGetHotSpot : (sci_style StyleNumber) -> boolean HotSpot.
    % This style is used to mark ranges of text that can detect mouse clicks.
    % The cursor changes to a hand over hotSpots, and the foreground, and background colours may change and an underline appear to indicate that these areas are sensitive to clicking.
    % This may be used to allow hyperlinks to other documents.

% Caret, selection, and hotSpot styles
%
% The selection is shown by changing the foreground and/or background colours.
% If one of these is not set then that attribute is not changed for the selection.
% The default is to show the selection by changing the background to light gray and leaving the foreground the same as when it was not selected.
% When there is no selection, the current insertion point is marked by the text caret.
% This is a vertical line that is normally blinking on and off to attract the users attention.
%
predicates
    setSelFore : (boolean UseSelectionForeColor, unsigned Color).
    setSelBack : (boolean UseSelectionBackColor, unsigned Color).
    % You can choose to override the default selection colouring with these two messages.
    % The colour you provide is used if you set useSelection*Colour to true.
    % If it is set to false, the default styled colouring is used and the colour argument has no effect.

properties
    selAlpha : integer.
    % The selection can be drawn translucently in the selection background colour by setting an alpha value.

properties
    selEolFilled : boolean.
    % The selection can be drawn up to the right hand border by setting this property.

properties
    caretFore : unsigned.
    % The colour of the caret can be set with SCI_SETCARETFORE and retrieved with SCI_GETCARETFORE.

properties
    caretLineVisible : boolean.

properties
    caretLineBack : unsigned.

properties
    caretLineBackAlpha : integer.
    % You can choose to make the background colour of the line containing the caret different with these messages.
    % To do this, set the desired background colour with SCI_SETCARETLINEBACK, then use SCI_SETCARETLINEVISIBLE(true) to enable the effect.
    % You can cancel the effect with SCI_SETCARETLINEVISIBLE(false).
    % The two SCI_GETCARET* functions return the state and the colour.
    % This form of background colouring has highest priority when a line has markers that would otherwise change the background colour.
    % The caret line may also be drawn translucently which allows other background colours to show through.
    % This is done by setting the alpha
    % (translucency) value by calling SCI_SETCARETLINEBACKALPHA.
    % When the alpha is not SC_ALPHA_NOALPHA, the caret line is drawn after all other features so will affect the colour of all other features.

properties
    caretPeriod : integer.
    % The rate at which the caret blinks can be set with SCI_SETCARETPERIOD which determines the time in milliseconds that the caret is
    % visible or invisible before changing state.
    % Setting the period to 0 stops the caret blinking.
    % The default value is 500 milliseconds.
    % SCI_GETCARETPERIOD returns the current setting.

properties
    caretStyle : integer.
    % The style of the caret can be set with SCI_SETCARETSTYLE to be a line caret (CARETSTYLE_LINE=1), a block caret
    % (CARETSTYLE_BLOCK=2) or to not draw at all (CARETSTYLE_INVISIBLE=0).
    % The default value is the line caret (CARETSTYLE_LINE=1).
    % You can determine the current caret style setting using SCI_GETCARETSTYLE.
    %
    % The block character draws most combining and multibyte character sequences successfully, though some fonts like Thai Fonts (and
    % possibly others) can sometimes appear strange when the cursor is positioned at these characters, which may result in only drawing a
    % part of the cursor character sequence.
    % This is most notable on Windows platforms.

properties
    caretWidth : integer.
    % The width of the line caret can be set with SCI_SETCARETWIDTH to a value of 0, 1, 2 or 3 pixels.
    % The default width is 1 pixel.
    % You can read back the current width with SCI_GETCARETWIDTH.
    % A width of 0 makes the caret invisible (added at version 1.50), similar to setting the caret style to CARETSTYLE_INVISIBLE (though not interchangable).
    % This setting only affects the width of the cursor when the cursor style is set to line caret mode, it does not affect the width for a block caret.

predicates
    setHotSpotActiveFore : (boolean UseHotSpotForeColor, unsigned Color).

predicates
    getHotSpotActiveFore : () -> unsigned Color.

predicates
    setHotSpotActiveBack : (boolean UseHotSpotBackColor, unsigned Color).

predicates
    getHotSpotActiveBack : () -> unsigned Color.

properties
    hotSpotActiveUnderline : boolean.

properties
    hotSpotSingleLine : boolean.
    % While the cursor hovers over text in a style with the hotSpot attribute set, the default colouring can be modified and an underline drawn
    % with these settings.
    % Single line mode stops a hotSpot from wrapping onto next line.

properties
    controlCharSymbol : integer.
    % By default, Scintilla displays control characters (characters with codes less than 32) in a rounded rectangle as ASCII mnemonics:
    % "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3",
    % "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US".
    % These mnemonics come from the early days of signaling, though some are still used (LF = Line Feed, BS = Back Space, CR = Carriage Return, for example).
    %
    % You can choose to replace these mnemonics by a nominated symbol with an ASCII code in the range 32 to 255.
    % If you set a symbol value less than 32, all control characters are displayed as mnemonics.
    % The symbol you set is rendered in the font of the style set for the character.
    % You can read back the current symbol with the SCI_GETCONTROLCHARSYMBOL message.
    % The default symbol value is 0.

properties
    caretSticky : integer.

predicates
    toggleCaretSticky : ().
    % These messages set, get or toggle the caretSticky setting which controls when the last position of the caret on the line is saved.
    %
    % When set to SC_CARETSTICKY_OFF (0), the sticky flag is off; all text changes (and all caret position changes) will remember the
    % caret's new horizontal position when moving to different lines.
    % This is the default.
    % When set to SC_CARETSTICKY_ON (1), the sticky flag is on, and the only thing which will cause the editor to remember the horizontal
    % caret position is moving the caret with mouse or keyboard (left/right arrow keys, home/end keys, etc).
    % When set to SC_CARETSTICKY_WHITESPACE (2), the caret acts like mode 0 (sticky off) except under one special case; when space or
    % tab characters are inserted. (Including pasting only space/tabs -- undo, redo, etc. do not exhibit this behavior..).
    % SCI_TOGGLECARETSTICKY switches from SC_CARETSTICKY_ON and SC_CARETSTICKY_WHITESPACE to SC_CARETSTICKY_OFF and
    % from SC_CARETSTICKY_OFF to SC_CARETSTICKY_ON.

%
% Character representations
%
% Some characters, such as control characters and invalid bytes, do not have a visual glyph or use a glyph that is hard
% to distinguish.
%
% Control characters (characters with codes less than 32, or between 128 and 159 in some encodings) are displayed by
% Scintilla using their mnemonics inverted in a rounded rectangle. These mnemonics come from the early days of signalling,
% though some are still used (LF = Line Feed, BS = Back Space, CR = Carriage Return, for example).
%
% For the low 'C0' values: "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF", "CR",
% "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US".
%
% For the high 'C1' values: "PAD", "HOP", "BPH", "NBH", "IND", "NEL", "SSA", "ESA", "HTS", "HTJ", "VTS", "PLD", "PLU",
% "RI", "SS2", "SS3", "DCS", "PU1", "PU2", "STS", "CCH", "MW", "SPA", "EPA", "SOS", "SGCI", "SCI", "CSI", "ST", "OSC",
% "PM", "APC".
%
% Invalid bytes are shown in a similar way with an 'x' followed by their value in hexadecimal, like "xFE".
% SCI_SETREPRESENTATION(const char8 *encodedCharacter, const char8 *representation)
% SCI_GETREPRESENTATION(const char8 *encodedCharacter, char8 *representation)
% SCI_CLEARREPRESENTATION(const char8 *encodedCharacter)
% SCI_SETCONTROLCHARSYMBOL(int symbol)
% SCI_GETCONTROLCHARSYMBOL
%
% SCI_SETREPRESENTATION(const char8 *encodedCharacter, const char8 *representation)
% SCI_GETREPRESENTATION(const char8 *encodedCharacter, char8 *representation NUL-terminated)
% SCI_CLEARREPRESENTATION(const char8 *encodedCharacter)
% Any character, including those normally displayed as mnemonics may be represented by a string inverted in a rounded
% rectangle.
%
% For example, the Ohm sign Ω U+2126 looks very similar to the Greek Omega character Ω U+03C9 so, for the UTF-8 encoding,
% to distinguish the Ohm sign as "U+2126 Ω" this call could be made: SCI_SETREPRESENTATION("\xe2\x84\xa6", "U+2126
% \xe2\x84\xa6")
%
% The encodedCharacter parameter is a NUL-terminated string of the bytes for one character in the current encoding. This
% can not be used to set a representation for multiple-character strings.
%
% The NUL (0) character is a special case since the encodedCharacter parameter is NUL terminated, the NUL character is
% specified as an empty string.
%
% SCI_SETCONTROLCHARSYMBOL(int symbol)
% SCI_GETCONTROLCHARSYMBOL
% The mnemonics may be replaced by a nominated symbol with an ASCII code in the range 32 to 255. If you set a symbol
% value less than 32, all control characters are displayed as mnemonics. The symbol you set is rendered in the font of the
% style set for the character. You can read back the current symbol with the SCI_GETCONTROLCHARSYMBOL message. The default
% symbol value is 0.
%
% Margins
% There may be up to five margins to the left of the text display, plus a gap either side of the text.
% Each margin can be set to display either symbols or line numbers with SCI_SETMARGINTYPEN.
% The markers that can be displayed in each margin are set with SCI_SETMARGINMASKN.
% Any markers not associated with a visible margin will be displayed as changes in background colour in the text.
% A width in pixels can be set for each margin.
% Margins with a zero width are ignored completely.
% You can choose if a mouse click in a margin sends a SCN_MARGINCLICK notification to the container or selects a line of text.
% The margins are numbered 0 to 4.
% Using a margin number outside the valid range has no effect.
% By default, margin 0 is set to display line numbers, but is given a width of 0, so it is hidden.
% Margin 1 is set to display non-folding symbols and is given a width of 16 pixels, so it is visible.
% Margin 2 is set to display the folding symbols, but is given a width of 0, so it is hidden.
% Of course, you can set the margins to be whatever you wish.
% Styled text margins used to show revision and blame information:
predicates
    setMarginTypeN : (integer Margin, integer IType).

predicates
    getMarginTypeN : (integer Margin) -> integer IType.
    % These two routines set and get the type of a margin.
    % The margin argument should be 0, 1, 2, 3 or 4.
    % You can use the predefined constants SC_MARGIN_SYMBOL (0) and SC_MARGIN_NUMBER (1) to set a margin as either a line number or a symbol margin.
    % A margin with application defined text may use SC_MARGIN_TEXT (4) or SC_MARGIN_RTEXT (5) to right justify the text.
    % By convention, margin 0 is used for line numbers and the next two are used for symbols.
    % You can also use the constants SC_MARGIN_BACK (2) and SC_MARGIN_FORE (3) for symbol margins that set their background colour to match the STYLE_DFAULT background and foreground colours.

predicates
    setMarginWidthN : (integer Margin, integer PixelWidth).

predicates
    getMarginWidthN : (integer Margin) -> integer PixelWidth.
    % These routines set and get the width of a margin in pixels.
    % A margin with zero width is invisible.
    % By default, Scintilla sets margin 1 for symbols with a width of 16 pixels, so this is a reasonable guess if you are not sure what would be appropriate.
    % Line number margins widths should take into account the number of lines in the document and the line number style.
    % You could use something like SCI_TEXTWIDTH(STYLE_LINENUMBER, "_99999") to get a suitable width.

predicates
    setMarginMaskN : (integer Margin, unsigned Mask).

predicates
    getMarginMaskN : (integer Margin) -> unsigned Mask.
    % The mask is a 32-bit value.
    % Each bit corresponds to one of 32 logical symbols that can be displayed in a margin that is enabled for symbols.
    % There is a useful constant, SC_MASK_FOLDERS (0xFE000000 or -33554432), that is a mask for the 7 logical symbols used to denote folding.
    % You can assign a wide range of symbols and colours to each of the 32 logical symbols, see Markers for more information.
    % If (mask & SC_MASK_FOLDERS)==0, the margin background colour is controlled by style 33 (STYLE_LINENUMBER).

% You add logical markers to a line with SCI_MARKERADD.
% If a line has an associated marker that does not appear in the mask of any margin with a non-zero width, the marker changes the background colour of the line.
% For example, suppose you decide to use logical marker 10 to mark lines with a syntax error and you want to show such lines by changing the background colour.
% The mask for this marker is 1 shifted left 10 times (1<<10) which is 0x400.
% If you make sure that no symbol margin includes 0x400 in its mask, any line with the marker getS the background colour changed.
% To set a non-folding margin 1 use SCI_SETMARGINMASKN(1, ~SC_MASK_FOLDERS) which is the default set by Scintilla.
% To set a folding margin 2 use SCI_SETMARGINMASKN(2, SC_MASK_FOLDERS).
% ~SC_MASK_FOLDERS is 0x1FFFFFF in hexadecimal or 33554431 decimal.
% Of course, you may need to display all 32 symbols in a margin, in which case use SCI_SETMARGINMASKN(margin, -1).
predicates
    setMarginSensitiveN : (integer Margin, boolean Sensitive).

predicates
    getMarginSensitiveN : (integer Margin) -> integer Sensitive.
    % Each of the five margins can be set sensitive or insensitive to mouse clicks.
    % A click in a sensitive margin sends a SCN_MARGINCLICK notification to the container.
    % Margins that are not sensitive act as selection margins which make it easy to select ranges of lines.
    % By default, all margins are insensitive.

predicates
    setMarginCursorN : (integer Margin, integer Cursor).

predicates
    getMarginCursorN : (integer Margin) -> integer Cursor.
    % A reversed arrow cursor is normally shown over all margins.
    % This may be changed to a normal arrow with SCI_SETMARGINCURSORN(margin, SC_CURSORARROW) or restored to a reversed arrow with SCI_SETMARGINCURSORN(margin,
    % SC_CURSORREVERSEARROW).

properties
    marginLeft : integer.

properties
    marginRight : integer.
    % These messages set and get the width of the blank margin on both sides of the text in pixels.
    % The default is to one pixel on each side.

predicates
    setFoldmarginColor : (boolean UseSetting, unsigned Color).

predicates
    setFoldmarginhiColor : (boolean UseSetting, unsigned Color).
    % These messages allow changing the colour of the fold margin and fold margin highLight.
    % On Windows the fold margin colour defaults to
    % ::GetSysColor(COLOR_3DFACE) and the fold margin highLight colour to ::GetSysColor(COLOR_3DHIGHLIGHT).

predicates
    marginSetText : (lineNumber Line, string Text).

predicates
    marginGetText : (lineNumber Line) -> string Text.

predicates
    marginSetStyle : (lineNumber Line, integer Style).

predicates
    marginGetStyle : (lineNumber Line) -> integer Style.

predicates
    marginSetStyles : (lineNumber Line, string8 Styles).

predicates
    marginGetStyles : (lineNumber Line) -> string8 Styles.

predicates
    marginTextClearAll : ().
    % Text margins are created with the type SC_MARGIN_TEXT or SC_MARGIN_RTEXT.
    % A different string may be set for each line with SCI_MARGINSETTEXT.
    % The whole of the text margin on a line may be displayed in a particular style with SCI_MARGINSETSTYLE or each character may be individually styled with SCI_MARGINSETSTYLES which uses an
    % array of bytes with each byte setting the style of
    % the corresponding text byte similar to SCI_SETSTYLINGEX.
    % Setting a text margin will cause a SC_MOD_CHANGEMARGIN notification to be sent.

predicates
    marginSetStyleOffset : (integer Style).

predicates
    marginGetStyleOffset : () -> integer Style.
    % Margin styles may be completely separated from standard text styles by setting a style offset.
    % For example, SCI_MARGINSETSTYLEOFFSET(256) would allow the margin styles to be numbered from 256 upto 511 so they do not overlap styles set
    % by lexers.
    % Each style number set with SCI_MARGINSETSTYLE or SCI_MARGINSETSTYLES has the offset added before looking up the style.

% Annotations
% Annotations are read-only lines of text underneath each line of editable text.
% An annotation may consist of multiple lines separated by
% '\n'.
% Annotations can be used to display an assembler version of code for debugging or to show diagnostic messages inline or to line up different versions of text in a merge tool.
% Annotations used for inline diagnostics:
predicates
    annotationSetText : (lineNumber Line, string Text).

predicates
    annotationGetText : (lineNumber Line) -> string Text.

predicates
    annotationSetStyle : (lineNumber Line, integer Style).

predicates
    annotationGetStyle : (lineNumber Line) -> integer Style.

predicates
    annotationSetStyles : (lineNumber Line, string8 Styles).

predicates
    annotationGetStyles : (lineNumber Line) -> string8 Styles.

predicates
    annotationGetLines : (lineNumber Line) -> integer LineCount.

predicates
    annotationClearAll : ().
    % A different string may be set for each line with SCI_ANNOTATIONSETTEXT.
    % To clear annotations call SCI_ANNOTATIONSETTEXT with a NULL pointer.
    % The whole of the text ANNOTATION on a line may be displayed in a particular style with SCI_ANNOTATIONSETSTYLE or each character may be individually styled with SCI_ANNOTATIONSETSTYLES
    % which uses an array of bytes with each byte setting the
    % style of the corresponding text byte similar to SCI_SETSTYLINGEX.
    % The text must be set first as it specifies how long the annotation is so how many bytes of styling to read.
    % Setting an annotation will cause a SC_MOD_CHANGEANNOTATION notification to be sent.

% The number of lines annotating a line can be retrieved with SCI_ANNOTATIONGETLINES.
% All the lines can be cleared of annotations with SCI_ANNOTATIONCLEARALL which is equivalent to clearing each line (setting to 0) and then deleting other memory used for this
% feature.
properties
    annotationVisible : integer.
    % Annotations can be made visible in a view and there is a choice of display style when visible.
    % The two messages set and get the annotation display mode.
    % The visible argument can be one of:
    %
    % ANNOTATION_HIDDEN
    % 0 Annotations are not displayed.
    %
    % ANNOTATION_STANDARD
    % 1 Annotations are drawn left justified with no adornment.
    %
    % ANNOTATION_BOXED
    % 2 Annotations are indented to match the text and are surrounded by a box.
    %
    % ANNOTATION_INDENTED
    % 3 Annotations are indented to match the text.
    %

properties
    annotationStyleOffset : integer.
    % Annotation styles may be completely separated from standard text styles by setting a style offset.
    % For example, SCI_ANNOTATIONSETSTYLEOFFSET(512) would allow the annotation styles to be numbered from 512 upto 767 so they do not overlap
    % styles set by lexers (or margins if margins offset is 256).
    % Each style number set with SCI_ANNOTATIONSETSTYLE or SCI_ANNOTATIONSETSTYLES has the offset added before looking up the style.
    %

% Other settings
% On Windows, there are some problems with visual flashing when switching between applications with palettes and it is also necessary
% for the application containing the Scintilla control to forward some messages to Scintilla for its palette code to work.
% Because of this, by default, the palette is not used and the application must tell Scintilla to use one.
% If Scintilla is not using a palette, it will only display in those colours already available, which are often the 20 Windows system colours.
% To see an example of how to enable palette support in Scintilla, search the text of SciTE for WM_PALETTECHANGED,
% WM_QUERYNEWPALETTE and SCI_SETUSEPALETTE.
% The Windows messages to forward are: WM_SYSCOLORCHANGE, WM_PALETTECHANGED, WM_QUERYNEWPALETTE (should return TRUE).
% To forward a message (WM_XXXX, WPARAM, LPARAM) to Scintilla, you can use SendMessage(hScintilla, WM_XXXX, WPARAM, LPARAM)
% where hScintilla is the handle to the Scintilla window you created as your editor.
% While we are on the subject of forwarding messages in Windows, the top level window should forward any WM_SETTINGCHANGE
% messages to Scintilla (this is currently used to collect changes to mouse settings, but could be used for other user interface items in the
% future).
properties
    bufferedDraw : boolean.
    % These messages turn buffered drawing on or off and report the buffered drawing state.
    % Buffered drawing draws each line into a bitmap rather than directly to the screen and then copies the bitmap to the screen.
    % This avoids flickering although it does take longer.
    % The default is for drawing to be buffered.

properties
    twoPhaseDraw : boolean.
    % Two phase drawing is a better but slower way of drawing text.
    % In single phase drawing each run of characters in one style is drawn along with its background.
    % If a character overhangs the end of a run, such as in "V_" where the "V" is in a different style from the "_", then this can cause the right hand side of the "V" to be
    % overdrawn by the background of the "_" which cuts it off.
    % Two phase drawing fixes this by drawing all the backgrounds first and then drawing the text in transparent mode.
    % Two phase drawing may flicker more than single phase unless buffered drawing is on.
    % The default is for drawing to be two phase.

properties
    technology : integer.
    % The technology property allows choosing between different drawing APIs and options. On most platforms, the only choice
    % is SC_TECHNOLOGY_DFAULT (0). On Windows Vista or later, SC_TECHNOLOGY_DIRECTWRITE (1) can be chosen to use the Direct2D
    % and DirectWrite APIs for higher quality antialiased drawing. Since Direct2D buffers drawing, Scintilla's buffering can
    % be turned off with SCI_SETBUFFEREDDRAW(0).

properties
    fontQuality : integer.
    % Manage font quality (antialiasing method).
    % Currently, the following values are available on Windows: SC_FF_QUALITY_DFAULT
    % (backward compatible), SC_FF_QUALITY_NON_ANTIALIASED, SC_FF_QUALITY_ANTIALIASED, SC_FF_QUALITY_LCD_OPTIMIZED.
    %
    % In case it is necessary to squeeze more options into this property, only a limited number of bits defined by
    % SC_EFF_QUALITY_MASK (0xf) will be used for quality.
    %

properties
    codePage : codePage.
    % Scintilla has some support for Japanese, Chinese and Korean DBCS.
    % Use this message with codePage set to the code page number to set Scintilla to use code page information to ensure double byte characters are treated as one character rather than two.
    % This also stops the caret from moving between the two bytes in a double byte character.
    % Do not use this message to choose between different single byte character sets: it doesn't do that.
    % Call with codePage set to zero to disable DBCS support.
    % The default is SCI_SETCodePage(0).
    %
    % Code page SC_CP_UTF (65001) sets Scintilla into Unicode mode with the document treated as a sequence of characters expressed in
    % UTF-.
    % The text is converted to the platform's normal Unicode encoding before being drawn by the OS and thus can display Hebrew, Arabic, Cyrillic, and Han characters.
    %
    % Languages which can use two characters stacked vertically in one horizontal space, such as Thai, will mostly work but there are some issues where the
    % characters are drawn separately leading to visual glitches.
    % Bi-directional text is not supported.
    % Code page can be set to 932 (Japanese Shift-JIS), 936 (Simplified Chinese GBK), 949 (Korean Unified Hangul Code), 950 (Traditional
    % Chinese Big5), or 1361 (Korean Johab) although these may require installation of language specific support.
    %

predicates
    setWordChars : (string Chars).
    % Scintilla has several functions that operate on words, which are defined to be contiguous sequences of characters from a particular set
    % of characters.
    % This message defines which characters are members of that set.
    % The character sets are set to default values before processing this function.
    % For example, if you don't allow '_' in your set of characters use: SCI_SETWORDCHARS(0, "abcdefghijklmnopqrstuvwxyzABCDFGHIJKLMNOPQRSTUVWXYZ012345679");

predicates
    setWhiteSpaceChars : (string Chars).
    % Similar to SCI_SETWORDCHARS, this message allows the user to define which chars Scintilla considers as whitespace.
    % Setting the whitespace chars allows the user to fine-tune Scintilla's behaviour doing such things as moving the cursor to the start or end of a word;
    % for example, by defining punctuation chars as whitespace, they will be skipped over when the user presses ctrl+left or ctrl+right.
    % This function should be called after SCI_SETWORDCHARS as it will reset the whitespace characters to the default set.

predicates
    setCharsDefault : ().
    % Use the default sets of word and whitespace characters.
    % This sets whitespace to space, tab and other characters with codes less than
    % 0x20, with word characters set to alphanumeric and '_'.

predicates
    grabFocus : ().

properties
    focus : boolean.
    % Scintilla can be told to grab the focus with this message.
    %
    % This is needed more on GTK+ where focus handling is more complicated than on Windows.
    %
    % The internal focus flag can be set with SCI_SETFOCUS.
    % This is used by clients that have complex focus requirements such as having their own window that getS the real focus but with the need to indicate that Scintilla has the logical focus.

% Brace highLighting
predicates
    braceHighLight : (integer Pos1, integer Pos2).
    % Up to two characters can be highLighted in a 'brace highLighting style', which is defined as style number STYLE_BRACELIGHT (34).
    % If you have enabled indent guides, you may also wish to highLight the indent that corresponds with the brace.
    % You can locate the column with SCI_GETCOLUMN and highLight the indent with SCI_SETHIGHLIGHTGUIDE.

predicates
    braceBadLight : (integer Pos1).
    % If there is no matching brace then the brace badLighting style, style STYLE_BRACEBAD (35), can be used to show the brace that is
    % unmatched.
    % Using a position of INVALID_POSITION (-1) removes the highLight.

predicates
    braceHighlightIndicator : (boolean UseBraceHighlightIndicator, integer IndicatorNumber).
    % SCI_BRACEHIGHLIGHTINDICATOR(bool useBraceHighlightIndicator, int indicatorNumber)
    % Use specified indicator to highlight matching braces instead of changing their style.

predicates
    braceBadlightIndicator : (boolean UseBraceBadlightIndicator, integer IndicatorNumber).
    % SCI_BRACEBADLIGHTINDICATOR(bool useBraceBadLightIndicator, int indicatorNumber)
    % Use specified indicator to highlight non matching brace instead of changing its style.

predicates
    tryBraceMatch : (integer Pos, integer MaxReStyle) -> integer MatchPosition determ.
    % The SCI_BRACEMATCH message finds a corresponding matching brace given pos, the position of one brace.
    % The brace characters handled are '(', ')', '[', ']', '{', '}', '<', and '>'.
    % The search is forwards from an opening brace and backwards from a closing brace.
    % If the character at position is not a brace character, or a matching brace cannot be found, the return value is -1.
    % Otherwise, the return value is the position of the matching brace.
    %
    % A match only occurs if the style of the matching brace is the same as the starting brace or the matching brace is beyond the end of
    % styling.
    % Nested braces are handled correctly.
    % The maxReStyle parameter must currently be 0 - it may be used in the future to limit the length of brace searches.

% Tabs and Indentation Guides
% Indentation (the white space at the start of a line) is often used
%  by programmers to clarify program structure and in some languages,
% for example Python, it may be part of the language syntax.
% Tabs are normally used in editors to insert a tab character or to pad text
%  with spaces up to the next tab.
% Scintilla can be set to treat tab and backspace in the white space at the start
%  of a line in a special way: inserting a tab indents the line to
% the next indent position rather than just inserting a tab at the current
%  character position and backspace unindents the line rather than
% deleting a character.
% Scintilla can also display indentation guides (vertical lines) to help you
%  to generate code.


properties
    tabWidth : charCount.
    % SCI_SETTABWIDTH sets the size of a tab as a multiple of the size of a space character in STYLE_DFAULT.
    % The default tab width is characters.
    % There are no limits on tab sizes, but values less than 1 or large values may have undesirable effects.

properties
    useTabs : boolean.
    % SCI_SETUSETABS determines whether indentation should be created out of a mixture of tabs and spaces or be based purely on spaces.
    % Set useTabs to false (0) to create all tabs and indents out of spaces.
    % The default is true.
    % You can use SCI_GETCOLUMN to get the column of a position taking the width of a tab into account.

properties
    indent : integer.
    % SCI_SETINDENT sets the size of indentation in terms of the width of a space in
%    STYLE_DFAULT.
    % If you set a width of 0, the indent size is the same as the tab size.
    % There are no limits on indent sizes, but values less than 0 or large values
    %  may have undesirable effects.

properties
    indentStep : integer (o).
    % returns the effective indent step (i.e. if indent is 0 it returns tabWidth otherwise is returns indent).

properties
    maintainIndentation : boolean.
    % @short If true new-line will indent the next line the same amount as the previous.
    % @end

properties
    tabIndents : boolean.

properties
    backSpaceUnIndents : boolean.
    %
    % Inside indentation white space, the tab and backspace keys can be made to indent and unindent rather than insert a tab character or
    % delete a character with the SCI_SETTABINDENTS and SCI_SETBACKSPACEUNINDENTS functions.
    %

predicates
    setLineIndentation : (lineNumber Line, linePosition Indentation).

predicates
    getLineIndentation : (lineNumber Line) -> linePosition Indentation.
    % The amount of indentation on a line can be discovered and set with SCI_GETLINEINDENTATION and SCI_SETLINEINDENTATION.
    % The indentation is measured in character columns, which correspond to the width of space characters.

predicates
    getLineIndentPosition : (lineNumber Line) -> integer Pos.
    % This returns the position at the end of indentation of a line.

properties
    indentationGuides : integer.
    % Indentation guides are dotted vertical lines that appear within indentation white space every indent size columns.
    % They make it easy to see which constructs line up especially when they extend over multiple pages.
    % Style STYLE_INDENTGUIDE (37) is used to specify the foreground and background colour of the indentation guides.
    %
    % There are 4 indentation guide views.
    % SC_IV_NONE turns the feature off but the other 3 states determine how far the guides appear on empty lines.
    %
    % SC_IV_NONE No indentation guides are shown.
    % SC_IV_REAL Indentation guides are shown inside real indentation white space.
    % SC_IV_LOOKFORWARD Indentation guides are shown beyond the actual indentation up to the level of the next non-empty line.
    % If the previous non-empty line was a fold header then indentation guides are shown for one more level of indent than that line.
    % This setting is good for Python.
    % SC_IV_LOOKBOTH Indentation guides are shown beyond the actual indentation up to the level of the next non-empty line or previous
    % non-empty line whichever is the greater.
    % This setting is good for most languages.
    %

properties
    highLightGuide : integer.
    % When brace highLighting occurs, the indentation guide corresponding to the braces may be highLighted with the brace highLighting style,
    % STYLE_BRACELIGHT (34).
    % Set column to 0 to cancel this highLight.

% Markers
%
% There are 32 markers, numbered 0 to MARKER_MAX (31), and you can assign any combination of them to each line in the document.
% Markers appear in the selection margin to the left of the text.
% If the selection margin is set to zero width, the background colour of the whole line is changed instead.
% Marker numbers 25 to 31 are used by Scintilla in folding margins, and have symbolic names of the form SC_MARKNUM_*, for example SC_MARKNUM_FOLDEROPEN.
%
% Marker numbers 0 to 24 have no pre-defined function; you can use them to mark syntax errors or the current point of execution, break
% points, or whatever you need marking.
% If you do not need folding, you can use all 32 for any purpose you wish.
% Each marker number has a symbol associated with it.
%
% You can also set the foreground and background colour for each marker number, so you can use the same symbol more than once with different colouring for different uses.
% Scintilla has a set of symbols you can assign (SC_MARK_*) or you can use characters.
% By default, all 32 markers are set to SC_MARK_CIRCLE with a black foreground and a white background.
%
% The markers are drawn in the order of their numbers, so higher numbered markers appear on top of lower numbered ones.
% Markers try to move with their text by tracking where the start of their line moves.
% When a line is deleted, its markers are combined, by an OR operation, with the markers of the previous line.
%
predicates
    markerDefine : (integer MarkerNumber, integer MarkerSymbols).
    % This message associates a marker number in the range 0 to 31 with one of the marker symbols or an ASCII character.
    % The general- purpose marker symbols currently available are:
    % SC_MARK_CIRCLE, SC_MARK_ROUNDRECT, SC_MARK_ARROW, SC_MARK_SMALLRECT, SC_MARK_SHORTARROW, SC_MARK_EMPTY,
    % SC_MARK_ARROWDOWN, SC_MARK_MINUS, SC_MARK_PLUS, SC_MARK_ARROWS, SC_MARK_DOTDOTDOT,
    % SC_MARK_BACKGROUND, SC_MARK_LFTRECT, SC_MARK_FULLRECT, and SC_MARK_UNDERLINE.
    %
    % The SC_MARK_BACKGROUND marker changes the background colour of the line only.
    % The SC_MARK_FULLRECT symbol mirrors this, changing only the margin background colour.
    % SC_MARK_UNDERLINE draws an underline across the text.
    % The SC_MARK_EMPTY symbol is invisible, allowing client code to track the movement of lines.
    % You would also use it if you changed the folding style and wanted one or more of the SC_FOLDERNUM_* markers to have no associated symbol.
    %
    % Applications may use the marker symbol SC_MARK_AVAILABLE to indicate that plugins may allocate that marker number.
    %
    % There are also marker symbols designed for use in the folding margin in a flattened tree style.
    % SC_MARK_BOXMINUS, SC_MARK_BOXMINUSCONNECTED, SC_MARK_BOXPLUS, SC_MARK_BOXPLUSCONNECTED,
    % SC_MARK_CIRCLEMINUS, SC_MARK_CIRCLEMINUSCONNECTED, SC_MARK_CIRCLEPLUS, SC_MARK_CIRCLEPLUSCONNECTED,
    % SC_MARK_LCORNER, SC_MARK_LCORNERCURVE, SC_MARK_TCORNER, SC_MARK_TCORNERCURVE, and SC_MARK_VLINE.
    % Characters can be used as markers by adding the ASCII value of the character to SC_MARK_CHARACTER (10000).
    % For example, to use 'A' (ASCII code 65) as marker number 1 use:
    % SCI_MARKERDFINE(1, SC_MARK_CHARACTER+65).
    %
    % The marker numbers SC_MARKNUM_FOLDER and SC_MARKNUM_FOLDEROPEN are used for showing that a fold is present and open or
    % closed.
    % Any symbols may be assigned for this purpose although the (SC_MARK_PLUS, SC_MARK_MINUS) pair or the
    % (SC_MARK_ARROW, SC_MARK_ARROWDOWN) pair are good choices.
    % As well as these two, more assignments are needed for the flattened tree style: SC_MARKNUM_FOLDEREND, SC_MARKNUM_FOLDERMIDTAIL, SC_MARKNUM_FOLDEROPENMID,
    % SC_MARKNUM_FOLDERSUB, and SC_MARKNUM_FOLDERTAIL.
    % The bits used for folding are specified by SC_MASK_FOLDERS, which is commonly used as an argument to SCI_SETMARGINMASKN when defining a margin to be used for folding.
    %
    % This table shows which SC_MARK_* symbols should be assigned to which SC_MARKNUM_* marker numbers to obtain four folding
    % styles: Arrow (mimics Macintosh), plus/minus shows folded lines as '+' and opened folds as '-', Circle tree, Box tree.
    %
    % SC_MARKNUM_*
    %
    % Arrow
    %
    % Plus/minus
    %
    % Circle tree
    %
    % Box tree
    %
    % FOLDEROPEN
    % ARROWDOWN MINUS CIRCLEMINUS BOXMINUS
    %
    % FOLDER
    % ARROW PLUS CIRCLEPLUS BOXPLUS
    %
    % FOLDERSUB
    % EMPTY EMPTY VLINE VLINE
    %
    % FOLDERTAIL
    % EMPTY EMPTY LCORNERCURVE LCORNER
    %
    % FOLDEREND
    % EMPTY EMPTY CIRCLEPLUSCONNECTED BOXPLUSCONNECTED
    %
    % FOLDEROPENMID
    % EMPTY EMPTY CIRCLEMINUSCONNECTED BOXMINUSCONNECTED
    %
    % FOLDERMIDTAIL
    % EMPTY EMPTY TCORNERCURVE TCORNER
    %
    % Marker samples
    %

predicates
    markerDefinePixmap : (integer MarkerNumber, string8 Xpm).
    % Markers can be set to pixmaps with this message.
    % The XPM format is used for the pixmap and it is limited to pixmaps that use one character per pixel with no named colours.
    % The transparent colour may be named 'None'.
    % The data should be null terminated.
    % Pixmaps use the SC_MARK_PIXMAP marker symbol.
    % You can find descriptions of the XPM format from here: http://en.wikipedia.org/wiki/X_PixMap

predicates
    markerDefineRgbaImage : (integer MarkerNumber, integer Width, integer Height, string8 Pixels).
    % SCI_RGBAIMAGESETWIDTH(int width)
    % SCI_RGBAIMAGESETHEIGHT(int height)
    % SCI_MARKERDFINERGBAIMAGE(int markerNumber, const char8 *pixels)
    % Markers can be set to translucent pixmaps with this message. The RGBA format is used for the pixmap. The width and height must
    % previously been set with the SCI_RGBAIMAGESETWIDTH and SCI_RGBAIMAGESETHEIGHT messages. Pixmaps use the
    % SC_MARK_RGBAIMAGE marker symbol.

predicates
    markerSymbolDefined : (integer MarkerNumber) -> integer MarkerSymbol.
    % Returns the symbol defined for a markerNumber with SCI_MARKERDFINE or SC_MARK_PIXMAP if defined with
    % SCI_MARKERDFINEPIXMAP.

predicates
    markerSetFore : (integer MarkerNumber, unsigned Color).

predicates
    markerSetBack : (integer MarkerNumber, unsigned Color).
    % These two messages set the foreground and background colour of a marker number.

predicates
    markerSetBackSelected : (integer MarkerNumber, unsigned Color).
    % SCI_MARKERSETBACKSELECTED(int markerNumber, int colour)
    % This message sets the highlight background colour of a marker number when its folding block is selected. The default colour is
    % #FF0000.

predicates
    markerEnableHighlight : (boolean Enabled).
    % SCI_MARKERENABLEHIGHLIGHT(bool enabled)
    % This message allows to enable/disable the highlight folding block when it is selected. (i.e. block that contains the caret)

predicates
    markerSetAlpha : (integer MarkerNumber, integer Alpha).
    % When markers are drawn in the content area, either because there is no margin for them or they are of SC_MARK_BACKGROUND or
    % SC_MARK_UNDERLINE types, they may be drawn translucently by setting an alpha value.

predicates
    tryMarkerAdd : (lineNumber Line, integer MarkerNumber) -> integer MarkerHandle determ.
    % This function adds marker number markerNumber to a line.
    % The function fails if this fails (illegal line number, out of memory) or it returns a marker handle number that identifies the added marker.
    % You can use this returned handle with SCI_MARKERLINFROMHANDLE to find where a marker is after moving or combining lines and with SCI_MARKERDELETEHANDLE to
    % delete the marker based on its handle.
    % The function does not check the value of markerNumber, nor does it check if the line already contains the marker.

predicates
    markerAddset : (lineNumber Line, integer MarkerMask).
    % This function can add one or more markers to a line with a single call, specified in the same "one-bit-per-marker" 32-bit integer format
    % returned by SCI_MARKERGET (and used by the mask-based marker search functions SCI_MARKERNEXT and SCI_MARKERPREVIOUS).
    % As with SCI_MARKERADD, no check is made to see if any of the markers are already present on the targetEd line.

predicates
    markerDelete : (lineNumber Line, integer MarkerNumber).
    % This searches the given line number for the given marker number and deletes it if it is present.
    % If you added the same marker more than once to the line, this will delete one copy each time it is used.
    % If you pass in a marker number of -1, all markers are deleted from the line.

predicates
    markerDeleteAll : (integer MarkerNumber).
    % This removes markers of the given number from all lines.
    % If markerNumber is -1, it deletes all markers from all lines.

predicates
    markerGet : (lineNumber Line) -> integer Markers.
    % This returns a 32-bit integer that indicates which markers were present on the line.
    % Bit 0 is set if marker 0 is present, bit 1 for marker 1 and so on.

predicates
    tryMarkerNext : (lineNumber LineStart, integer MarkerMask) -> lineNumber LineNumber determ.

predicates
    tryMarkerPrevious : (lineNumber LineStart, integer MarkerMask) -> lineNumber LineNumber determ.
    % These functions search efficiently for lines that include a given set of markers.
    % The search starts at line number lineStart and continues forwards to the end of the file (SCI_MARKERNEXT) or backwards to the start of the file (SCI_MARKERPREVIOUS).
    % The markerMask argument should have one bit set for each marker you wish to find.
    % Set bit 0 to find marker 0, bit 1 for marker 1 and so on.
    % The function returns the line number of the first line that contains one of the markers in markerMask or fails if no marker is found.

predicates
    tryMarkerLineFromHandle : (integer MarkerHandle) -> lineNumber LineNumber determ.
    % The markerHandle argument is an identifier for a marker returned by SCI_MARKERADD.
    % This function searches the document for the marker with this handle and returns the line number that contains it or fails if it is not found.

predicates
    markerDeleteHandle : (integer MarkerHandle).
    % The markerHandle argument is an identifier for a marker returned by SCI_MARKERADD.
    % This function searches the document for the marker with this handle and deletes the marker if it is found.

% Indicators
%
% Indicators are used to display additional information over the top of styling. They can be used to show, for example,
% syntax errors, deprecated names and bad indentation by drawing underlines under text or boxes around text.
%
% Indicators may have a different "hover" colour and style when the mouse is over them or the caret is moved into them.
% This may be used, for example, to indicate that a URL can be clicked.
%
% Indicators may be displayed as simple underlines, squiggly underlines, a line of small 'T' shapes, a line of diagonal
% hatching, a strike-out or a rectangle around the text. They may also be invisible when used to track pieces of content
% for the application as INDIC_HIDDEN.
%
% The SCI_INDIC* messages allow you to get and set the visual appearance of the indicators. They all use an
% indicatorNumber argument in the range 0 to INDIC_MAX(35) to set the indicator to style. To prevent interference the set
% of indicators is divided up into a range for use by lexers (0..7) a range for use by containers (8=INDIC_CONTAINER ..
% 31=INDIC_IME-1) and a range for IME indicators (32=INDIC_IME .. 35=INDIC_IME_MAX).
%
% Indicators are stored in a format similar to run length encoding which is efficient in both speed and storage for
% sparse information.
%
% An indicator may store different values for each range but currently all values are drawn the same. In the future, it
% may be possible to draw different values in different styles.
%
% Originally, Scintilla used a different technique for indicators but this has been removed and the APIs perform no
% action. While both techniques were supported, the term "modern indicators" was used for the newer implementation.
predicates
    indicSetStyle : (integer IndicatorNumber, integer IndicatorStyle).

predicates
    indicGetStyle : (integer IndicatorNumber) -> integer IndicatorStyle.
    % These two messages set and get the style for a particular indicator.
    % The indicator styles currently available are:
    %
    % Symbol Value Visual effect
    %
    % INDIC_PLAIN 0 Underlined with a single, straight line.
    % INDIC_SQUIGGLE 1 A squiggly underline. Requires 3 pixels of descender space.
    % INDIC_TT 2 A line of small T shapes.
    % INDIC_DIAGONAL 3 Diagonal hatching.
    % INDIC_STRIKE 4 Strike out.
    % INDIC_HIDDEN 5 An indicator with no visual effect.
    % INDIC_BOX 6 A rectangle around the text.
    % INDIC_ROUNDBOX 7 A rectangle with rounded corners around the text using translucent drawing with the interior usually
    % more transparent than the border. You can use SCI_INDICSETALPHA and SCI_INDICSETOUTLINEALPHA to control the alpha
    % transparency values. The default alpha values are 30 for fill colour and 50 for outline colour.
    % INDIC_STRAIGHTBOX 8 A rectangle around the text using translucent drawing with the interior usually more transparent
    % than the border. You can use SCI_INDICSETALPHA and SCI_INDICSETOUTLINEALPHA to control the alpha transparency values.
    % The default alpha values are 30 for fill colour and 50 for outline colour. This indicator does not colour the top pixel
    % of the line so that indicators on contiguous lines are visually distinct and disconnected.
    % INDIC_FULLBOX 16 A rectangle around the text using translucent drawing similar to INDIC_STRAIGHTBOX but covering the
    % entire character area.
    % INDIC_DASH 9 A dashed underline.
    % INDIC_DOTS 10 A dotted underline.
    % INDIC_SQUIGGLELOW 11 Similar to INDIC_SQUIGGLE but only using 2 vertical pixels so will fit under small fonts.
    % INDIC_DOTBOX 12 A dotted rectangle around the text using translucent drawing. Translucency alternates between the alpha
    % and outline alpha settings with the top-left pixel using the alpha setting. SCI_INDICSETALPHA and
    % SCI_INDICSETOUTLINEALPHA control the alpha transparency values. The default values are 30 for alpha and 50 for outline
    % alpha. To avoid excessive memory allocation the maximum width of a dotted box is 4000 pixels.
    % INDIC_SQUIGGLEPIXMAP 13 A version of INDIC_SQUIGGLE that draws using a pixmap instead of as a series of line segments
    % for performance. Measured to be between 3 and 6 times faster than INDIC_SQUIGGLE on GTK+. Appearance will not be as good
    % as INDIC_SQUIGGLE on OS X in HiDPI mode.
    % INDIC_COMPOSITIONTHICK 14 A 2-pixel thick underline located at the bottom of the line to try to avoid touching the
    % character base. Each side is inset 1 pixel so that different indicators in this style covering a range appear isolated.
    % This is similar to an appearance used for the target in Asian language input composition.
    % INDIC_COMPOSITIONTHIN 15 A 1-pixel thick underline located just before the bottom of the line. Each side is inset 1
    % pixel so that different indicators in this style covering a range appear isolated. This is similar to an appearance used
    % for non-target ranges in Asian language input composition.
    % INDIC_TEXTFORE 17 Change the colour of the text to the indicator's fore colour.
    %
    % The default indicator styles are equivalent to:
    % SCI_INDICSETSTYLE(0, INDIC_SQUIGGLE);
    % SCI_INDICSETSTYLE(1, INDIC_TT);
    % SCI_INDICSETSTYLE(2, INDIC_PLAIN);
    %

predicates
    indicSetFore : (integer IndicatorNumber, unsigned Color).

predicates
    indicGetFore : (integer IndicatorNumber) -> unsigned Color.
    % These two messages set and get the colour used to draw an indicator.
    % The default indicator colours are equivalent to:
    % SCI_INDICSETFORE(0, 0x007f00); (dark green)
    % SCI_INDICSETFORE(1, 0xff0000); (light blue)
    % SCI_INDICSETFORE(2, 0x0000ff); (light red)

predicates
    indicSetAlpha : (integer IndicatorNumber, integer Alpha).

predicates
    indicGetAlpha : (integer IndicatorNumber) -> integer Alpha.
    % These two messages set and get the alpha transparency used for drawing the fill color of the INDIC_ROUNDBOX rectangle.
    % The alpha value can range from 0 (completely transparent) to 255 (no transparency).

predicates
    indicSetOutlineAlpha : (integer IndicatorNumber, integer OutlineAlpha).

predicates
    indicGetOutlineAlpha : (integer IndicatorNumber) -> integer OutlineAlpha.
    % These two messages set and get the alpha transparency used for drawing the fill color of the INDIC_ROUNDBOX rectangle.
    % The alpha value can range from 0 (completely transparent) to 255 (no transparency).

predicates
    indicSetUnder : (integer IndicatorNumber, boolean Under).

predicates
    indicGetUnder : (integer IndicatorNumber).
    % These two messages set and get whether an indicator is drawn under text or over(default).
    % Drawing under text works only for modern indicators when two phase drawing is enabled.

% Modern Indicators
% Modern indicators are stored in a format similar to run length encoding which is efficient in both speed and storage for sparse
% information.
% An indicator may store different values for each range but currently all values are drawn the same.
% In the future, it may be possible to draw different values in different styles.
properties
    indicatorCurrent : integer.
    % These two messages set and get the indicator that will be affected by calls to SCI_INDICATORFILLRANGE and
    % SCI_INDICATORCLEARRANGE.

properties
    indicatorValue : integer.
    % These two messages set and get the value that will be set by calls to SCI_INDICATORFILLRANGE.

predicates
    indicatorfillRange : (integer Position, integer FillLength).

predicates
    indicatorClearRange : (integer Position, integer ClearLength).
    % These two messages fill or clear a range for the current indicator.
    % SCI_INDICATORFILLRANGE fills with the the current value.

predicates
    indicatorAllOnFor : (integer Position) -> unsigned IndicatorAllOn.
    % Retrieve a bitmap value representing which indicators are non-zero at a position.

predicates
    indicatorValueAt : (integer Indicator, integer Position).
    % Retrieve the value of a particular indicator at a position.

predicates
    indicatorStart : (integer Indicator, integer Position) -> integer Start.

predicates
    indicatorEnd : (integer Indicator, integer Position) -> integer End.
    % Find the start or end of a range with one value from a position within the range.
    % Can be used to iterate through the document to discover all the indicator positions.
    %

% OS X Find Indicator
%
% On OS X search matches are highlighted with an animated gold rounded rectangle. The indicator shows, then briefly grows
% 25% and shrinks to the original size to draw the user's attention. While this feature is currently only implemented on
% OS X, it may be implemented on other platforms in the future.
%
% SCI_FINDINDICATORSHOW(int start, int end)
% SCI_FINDINDICATORFLASH(int start, int end)
% These two messages show and animate the find indicator. The indicator remains visible with SCI_FINDINDICATORSHOW and
% fades out after showing for half a second with SCI_FINDINDICATORFLASH. SCI_FINDINDICATORSHOW behaves similarly to the OS
% X TextEdit and Safari applications and is best suited to editing documentation where the search target is often a word.
% SCI_FINDINDICATORFLASH is similar to Xcode and is suited to editing source code where the match will often be located
% next to operators which would otherwise be hidden under the indicator's padding.
%
% SCI_FINDINDICATORHIDE
% This message hides the find indicator.
%
% Earlier versions of Scintilla allowed partitioning style bytes between style numbers and indicators and provided APIs
% for setting and querying this.
%
% Autocompletion
%
% Autocompletion displays a list box showing likely identifiers based upon the user's typing. The user chooses the
% currently selected item by pressing the tab character or another character that is a member of the fillup character set
% defined with SCI_AUTOCSETFILLUPS. Autocompletion is triggered by your application. For example, in C if you detect that
% the user has just typed fred. you could look up fred, and if it has a known list of members, you could offer them in an
% autocompletion list. Alternatively, you could monitor the user's typing and offer a list of likely items once their
% typing has narrowed down the choice to a reasonable list. As yet another alternative, you could define a key code to
% activate the list.
%
% When the user makes a selection from the list the container is sent a SCN_AUTOCSELECTION notification message. On
% return from the notification Scintilla will insert the selected text and the container is sent a SCN_AUTOCCOMPLETED
% notification message unless the autocompletion list has been cancelled, for example by the container sending
% SCI_AUTOCCANCEL.
%
% To make use of autocompletion you must monitor each character added to the document. See SciTEBase::CharAdded() in
% SciTEBase.cxx for an example of autocompletion.
predicates
    autoCShow : (integer LenEntered, string List).
    % This message causes a list to be displayed. lenEntered is the number of characters of the word already entered and list
    % is the list of words separated by separator characters. The initial separator character is a space but this can be set
    % or got with SCI_AUTOCSETSEPARATOR and SCI_AUTOCGETSEPARATOR.
    %
    % With default settings, the list of words should be in sorted order. If set to ignore case mode with
    % SCI_AUTOCSETIGNORECASE, then strings are matched after being converted to upper case. One result of this is that the
    % list should be sorted with the punctuation characters '[', '\', ']', '^', '_', and '`' sorted after letters. Alternative
    % handling of list order may be specified with SCI_AUTOCSETORDER
    %

predicates
    autoCCancel : ().
    % This message cancels any displayed autocompletion list.
    % When in autocompletion mode, the list should disappear when the user types a character that can not be part of the autocompletion, such as '.', '(' or '[' when typing an identifier.
    % A set of characters that will cancel autocompletion can be specified with SCI_AUTOCstopS.

predicates
    autoCActive : () -> boolean Active.
    % This message returns non-zero if there is an active autocompletion list and zero if there is not.

properties
    autoCPosStart : integer (o).
    % This returns the value of the current position when SCI_AUTOCSHOW started display of the list.

predicates
    autoCComplete : ().
    % This message triggers autocompletion.
    % This has the same effect as the tab key.

predicates
    autoCStops : (string Chars).
    % The chars argument is a string containing a list of characters that will automatically cancel the autocompletion list.
    % When you start the editor, this list is empty.

properties
    autoCSeparator : char.
    % These two messages set and get the separator character used to separate words in the SCI_AUTOCSHOW list.
    % The default is the space character.

predicates
    autoCSelect : (string Select).

properties
    autoCCurrent : integer (o).
    % This message selects an item in the autocompletion list.
    % It searches the list of words for the first that matches select.
    % By default, comparisons are case sensitive, but you can change this with SCI_AUTOCSETIGNORECASE.
    % The match is character by character for the length of the select string.
    % That is, if select is "Fred" it will match "Frederick" if this is the first item in the list that begins with "Fred".
    % If an item is found, it is selected.
    % If the item is not found, the autocompletion list closes if auto-hide is true (see SCI_AUTOCSETAUTOHIDE).
    % The current selection index can be retrieved with SCI_AUTOCGETCURRENT.

properties
    autoCCurrentText : string (o).
    % This message retrieves the current selected text in the autocompletion list.
    % Normally the SCN_AUTOCSELECTION notification is used instead.
    %
    % The value is copied to the text buffer, returning the length (not including the terminating 0).
    % If not found, an empty string is copied to the buffer and 0 is returned.
    %
    % If the value argument is 0 then the length that should be allocated to store the value is returned; again, the terminating 0 is not
    % included.

properties
    autoCCancelatStart : boolean.
    % The default behavior is for the list to be cancelled if the caret moves to the location it was at when the list was displayed.
    % By calling this message with a false argument, the list is not cancelled until the caret moves before the first character of the word being completed.

properties
    autoCFillups : string (i).
    % If a fillup character is typed with an autocompletion list active, the currently selected item in the list is added into the document, then
    % the fillup character is added.
    % Common fillup characters are '(', '[' and '.' but others are possible depending on the language.
    % By default, no fillup characters are set.

properties
    autoCChooseSingle : boolean.
    % If you use SCI_AUTOCSETCHOOSESINGLE(1) and a list has only one item, it is automatically added and no list is displayed.
    % The default is to display the list even if there is only a single item.

properties
    autoCIgnoreCase : boolean.
    % By default, matching of characters to list members is case sensitive.
    % These messages let you set and get case sensitivity.

properties
    autoCCaseInsensitiveBehaviour : integer.
    % When autocompletion is set to ignore case (SCI_AUTOCSETIGNORECASE), by default it will nonetheless select the first
    % list member that matches in a case sensitive way to entered characters. This corresponds to a behaviour property of
    % SC_CASEINSENSITIVEBEHAVIOUR_RESPECTCASE (0). If you want autocompletion to ignore case at all, choose
    % SC_CASEINSENSITIVEBEHAVIOUR_IGNORECASE (1).
    %
    % SCI_AUTOCSETMULTI(int multi)
    % SCI_AUTOCGETMULTI
    % When autocompleting with multiple selections present, the autocompleted text can go into just the main selection with
    % SC_MULTIAUTOC_ONCE (0) or into each selection with SC_MULTIAUTOC_EACH (1). The default is SC_MULTIAUTOC_ONCE.
    %

% SCI_AUTOCSETORDER(int order)
% SCI_AUTOCGETORDER
% The default setting SC_ORDER_PRESORTED (0) requires that the list be provided in alphabetical sorted order.
%
% Sorting the list can be done by Scintilla instead of the application with SC_ORDER_PERFORMSORT (1). This will take
% additional time.
%
%
% Setting the order should be done before calling SCI_AUTOCSHOW.
%
% SCI_AUTOCSETAUTOHIDE(bool autoHide)
% SCI_AUTOCGETAUTOHIDE
% By default, the list is cancelled if there are no viable matches (the user has typed characters that no longer match a
% list entry). If you want to keep displaying the original list, set autoHide to false. This also effects SCI_AUTOCSELECT.
%
properties
    autoCAutoHide : boolean.
    % By default, the list is cancelled if there are no viable matches (the user has typed characters that no longer match a list entry).
    % If you want to keep displaying the original list, set autoHide to false.
    % This also effects SCI_AUTOCSELECT.

properties
    autoCDropRestOfWord : boolean.
    % When an item is selected, any word characters following the caret are first erased if dropRestOfWord is set true.
    % The default is false.

predicates
    registerImage : (integer Type, string8 XpmData).

predicates
    registerRgbaImage : (integer Type, integer Width, integer Height, string8 Pixels).

predicates
    clearRegisteredImages : ().

properties
    autoCTypeSeparator : char.
    % Autocompletion list items may display an image as well as text.
    % Each image is first registered with an integer type. Then this integer is
    % included in the text of the list separated by a '?' from the text. For example, "fclose?2 fopen" displays image 2 before the string "fclose"
    % and no image before "fopen". The images are in either the XPM format (SCI_REGISTERIMAGE) or RGBA format
    % (SCI_REGISTERRGBAIMAGE). For SCI_REGISTERRGBAIMAGE the width and height must previously been set with the
    % SCI_RGBAIMAGESETWIDTH and SCI_RGBAIMAGESETHEIGHT messages. The set of registered images can be cleared with
    % SCI_CLEARREGISTEREDIMAGES and the '?' separator changed with SCI_AUTOCSETTYPESEPARATOR.

properties
    autoCMaxHeight : integer.
    % Get or set the maximum number of rows that will be visible in an autocompletion list.
    % If there are more rows in the list, then a vertical scrollbar is shown.
    % The default is 5.

properties
    autoCMaxWidth : integer.
    % Get or set the maximum width of an autocompletion list expressed as the number of characters in the longest item that will be totally
    % visible.
    % If zero (the default) then the list's width is calculated to fit the item with the most characters.
    % Any items that cannot be fully displayed within the available width are indicated by the presence of ellipsis.

% User lists
%
% User lists use the same internal mechanisms as autocompletion lists, and all the calls listed for autocompletion work
% on them; you cannot display a user list at the same time as an autocompletion list is active. They differ in the
% following respects:
%
% o The SCI_AUTOCSETCHOOSESINGLE message has no effect.
% o When the user makes a selection you are sent a SCN_USERLISTSELECTION notification message rather than
% SCN_AUTOCSELECTION.
%
% BEWARE: if you have set fillup characters or stop characters, these will still be active with the user list, and may
% result in items being selected or the user list cancelled due to the user typing into the editor.
%
predicates
    userListShow : (integer ListType, string List).
    % The listType parameter is returned to the container as the wParam field of the SCNotification structure.
    % It must be greater than 0 as this is how Scintilla tells the difference between an autocompletion list and a user list.
    % If you have different types of list, for example a list of buffers and a list of macros, you can use listType to tell which one has returned a selection.

% Call tips
%
% Call tips are small windows displaying the arguments to a function and are displayed after the user has typed the name
% of the function. They normally display characters using the font facename, size and character set defined by
% STYLE_DEFAULT. You can choose to use STYLE_CALLTIP to define the facename, size, foreground and background colours and
% character set with SCI_CALLTIPUSESTYLE. This also enables support for Tab characters. There is some interaction between
% call tips and autocompletion lists in that showing a call tip cancels any active autocompletion list, and vice versa.
%
% Call tips are not implemented on Qt.
%
% Call tips can highlight part of the text within them. You could use this to highlight the current argument to a
% function by counting the number of commas (or whatever separator your language uses). See SciTEBase::CharAdded() in
% SciTEBase.cxx for an example of call tip use.
%
% The mouse may be clicked on call tips and this causes a SCN_CALLTIPCLICK notification to be sent to the container.
% Small up and down arrows may be displayed within a call tip by, respectively, including the characters '\001', or
% '\002'. This is useful for showing that there are overloaded variants of one function name and that the user can click
% on the arrows to cycle through the overloads.
%
% Alternatively, call tips can be displayed when you leave the mouse pointer for a while over a word in response to the
% SCN_DWELLSTART notification and cancelled in response to SCN_DWELLEND. This method could be used in a debugger to give
% the value of a variable, or during editing to give information about the word under the pointer.
predicates
    calltipShow : (integer PosStart, string Definition).
    % This message starts the process by displaying the call tip window.
    % If a call tip is already active, this has no effect.
    % posStart is the position in the document at which to align the call tip.
    % The call tip text is aligned to start 1 line below this character unless you have included up and/or down arrows in the call tip text in which case the tip is
    % aligned to the right-hand edge of the rightmost arrow.
    % The assumption is that you will start the text with something like "\001 1 of 3 \002". definition is the call tip text.
    % This can contain multiple lines separated by '\n' (Line Feed, ASCII code 10) characters.
    % Do not include '\r'
    % (Carriage Return, ASCII code 13), as this will most likely print as an empty box.
    % '\t' (Tab, ASCII code 9) is supported if you set a tabsize with SCI_CALLTIPUSESTYLE.

predicates
    calltipCancel : ().
    % This message cancels any displayed call tip.
    % Scintilla will also cancel call tips for you if you use any keyboard commands that are not compatible with editing the argument list of a function.

predicates
    calltipActive : ().
    % This returns 1 if a call tip is active and 0 if it is not active.

predicates
    calltipPosStart : ().
    % This message returns the value of the current position when SCI_CALLTIPSHOW started to display the tip.

predicates
    calltipSetHlt : (integer HlStart, integer HlEnd).
    % This sets the region of the call tips text to display in a highlighted style. hlStart is the zero-based index into the
    % string of the first character to highlight and hlEnd is the index of the first character after the highlight. hlEnd must
    % be greater than hlStart; hlEnd-hlStart is the number of characters to highlight. Highlights can extend over line ends if
    % this is required.
    %
    % Unhighlighted text is drawn in a mid grey. Selected text is drawn in a dark blue. The background is white. These can be
    % changed with SCI_CALLTIPSETBACK, SCI_CALLTIPSETFORE, and SCI_CALLTIPSETFOREHLT.
    %

predicates
    calltipsetBack : (unsigned Color).
    % The background colour of call tips can be set with this message; the default colour is white. It is not a good idea to
    % set a dark colour as the background as the default colour for normal calltip text is mid grey and the default colour for
    % highlighted text is dark blue. This also sets the background colour of STYLE_CALLTIP.
    %

predicates
    calltipsetFore : (unsigned Color).
    % The colour of call tip text can be set with this message; the default colour is mid grey. This also sets the foreground
    % colour of STYLE_CALLTIP.
    %

properties
    calltipForeHlt : unsigned (i).
    % The colour of highlighted call tip text can be set with this message; the default colour is dark blue.
    %

predicates
    calltipUseStyle : (integer TabSize).
    % This message changes the style used for call tips from STYLE_DEFAULT to STYLE_CALLTIP and sets a tab size in screen
    % pixels. If tabsize is less than 1, Tab characters are not treated specially. Once this call has been used, the call tip
    % foreground and background colours are also taken from the style.
    %

predicates
    calltipSetPosition : (boolean Above).
    % By default the calltip is displayed below the text, setting above to true (1) will display it above the text.
    %

% Keyboard commands
%
% To allow the container application to perform any of the actions available to the user with keyboard, all the keyboard
% actions are messages. They do not take any parameters. These commands are also used when redefining the key bindings
% with the SCI_ASSIGNCMDKEY message.
%
predicates
    lineDown : ().
    lineDownExtend : ().
    lineDownRectExtend : ().
    lineScrollDown : ().
    lineUp : ().
    lineUpExtend : ().
    lineUpRectExtend : ().
    lineScrollUp : ().
    paraDown : ().
    paraDownExtend : ().
    paraUp : ().
    paraUpExtend : ().
    charLeft : ().
    charLeftExtend : ().
    charLeftRectExtend : ().
    charRight : ().
    charRightExtend : ().
    charRightRectExtend : ().
    wordLeft : ().
    wordLeftExtend : ().
    wordRight : ().
    wordRightExtend : ().
    wordLeftEnd : ().
    wordLeftEndExtend : ().
    wordRightEnd : ().
    wordRightEndExtend : ().
    wordPartLeft : ().
    wordPartLeftExtend : ().
    wordPartRight : ().
    wordPartRightExtend : ().
    home : ().
    homeExtend : ().
    homeRectExtend : ().
    homeDisplay : ().
    homeDisplayExtend : ().
    homeWrap : ().
    homeWrapExtend : ().
    vcHome : ().
    vcHomeExtend : ().
    vcHomeRectExtend : ().
    vcHomeWrap : ().
    vcHomeWrapExtend : ().
    lineEnd : ().
    lineEndExtend : ().
    lineEndRectExtend : ().
    lineEndDisplay : ().
    lineEndDisplayExtend : ().
    lineEndWrap : ().
    lineEndWrapExtend : ().
    documentStart : ().
    documentStartExtend : ().
    documentEnd : ().
    documentEndExtend : ().
    pageUp : ().
    pageUpExtend : ().
    pageUpRectExtend : ().
    pageDown : ().
    pageDownExtend : ().
    pageDownRectExtend : ().
    stutteredPageUp : ().
    stutteredPageUpExtend : ().
    stutteredPageDown : ().
    stutteredPageDownExtend : ().
    deleteBack : ().
    deleteBackNotLine : ().
    delWordLeft : ().
    delWordRight : ().
    delWordRightEnd : ().
    delLineLeft : ().
    delLineRight : ().
    lineDelete : ().
    lineCut : ().
    lineCopy : ().
    lineTranspose : ().
    lineDuplicate : ().
    lowerCase : ().
    upperCase : ().
    cancel : ().
    editToggleOvertype : ().
    newline : ().
    formfeed : ().
    tab : ().
    tab_vip : ().
    backTab : ().
    backTab_vip : ().
    selectionDuplicate : ().
    verticalCentreCaret : ().

% The SCI_*EXTEND messages extend the selection.
%
% The SCI_*RECTEXTEND messages extend the rectangular selection (and convert regular selection to rectangular one, if
% any).
%
% The SCI_WORDPART* commands are used to move between word segments marked by capitalisation (aCamelCaseIdentifier) or
% underscores (an_under_bar_ident).
%
% The SCI_HOME* commands move the caret to the start of the line, while the SCI_VCHOME* commands move the caret to the
% first non-blank character of the line (ie. just after the indentation) unless it is already there; in this case, it acts
% as SCI_HOME*.
%
% The SCI_[HOME|LINEEND]DISPLAY* commands are used when in line wrap mode to allow movement to the start or end of
% display lines as opposed to the normal SCI_[HOME|LINEEND] commands which move to the start or end of document lines.
%
% The SCI_[[VC]HOME|LINEEND]WRAP* commands are like their namesakes SCI_[[VC]HOME|LINEEND]* except they behave
% differently when word-wrap is enabled: They go first to the start / end of the display line, like
% SCI_[HOME|LINEEND]DISPLAY*, but if the cursor is already at the point, it goes on to the start or end of the document
% line, as appropriate for SCI_[[VC]HOME|LINEEND]*.
%
% The SCI_SCROLLTO[START|END] commands scroll the document to the start or end without changing the selection. These
% commands match OS X platform conventions for the behaviour of the home and end keys. Scintilla can be made to match OS X
% applications by binding the home and end keys to these commands.
%
% The SCI_CANCEL command cancels autocompletion and calltip display and drops any additional selections.
%
% Key bindings
%
% There is a default binding of keys to commands that is defined in the Scintilla source in the file KeyMap.cxx by the
% constant KeyMap::MapDefault[]. This table maps key definitions to SCI_* messages with no parameters (mostly the keyboard
% commands discussed above, but any Scintilla command that has no arguments can be mapped). You can change the mapping to
% suit your own requirements.
%
% keyDefinition
% A key definition contains the key code in the low 16-bits and the key modifiers in the high 16-bits. To combine keyCode
% and keyMod set:
%
% keyDefinition = keyCode + (keyMod << 16)
%
% The key code is a visible or control character or a key from the SCK_* enumeration, which contains:
% SCK_ADD, SCK_BACK, SCK_DELETE, SCK_DIVIDE, SCK_DOWN, SCK_END, SCK_ESCAPE, SCK_HOME, SCK_INSERT, SCK_LEFT, SCK_MENU,
% SCK_NEXT (Page Down), SCK_PRIOR (Page Up), SCK_RETURN, SCK_RIGHT, SCK_RWIN, SCK_SUBTRACT, SCK_TAB, SCK_UP, and SCK_WIN.
%
% The modifiers are a combination of zero or more of SCMOD_ALT, SCMOD_CTRL, SCMOD_SHIFT, and SCMOD_META. On OS X, the
% Command key is mapped to SCMOD_CTRL and the Control key to SCMOD_META. If you are building a table, you might want to
constants
    key_A = 0x41.
    key_B = 0x42.
    key_C = 0x43.
    key_D = 0x44.
    key_E = 0x45.
    key_F = 0x46.
    key_G = 0x47.
    key_H = 0x48.
    key_I = 0x49.
    key_J = 0x4A.
    key_K = 0x4B.
    key_L = 0x4C.
    key_M = 0x4D.
    key_N = 0x4E.
    key_O = 0x4F.
    key_P = 0x50.
    key_Q = 0x51.
    key_R = 0x52.
    key_S = 0x53.
    key_T = 0x54.
    key_U = 0x55.
    key_V = 0x56.
    key_W = 0x57.
    key_X = 0x58.
    key_Y = 0x59.
    key_Z = 0x5A.
    key_down = sciLexer_native::sck_down.
    key_up = sciLexer_native::sck_up.
    key_left = sciLexer_native::sck_left.
    key_right = sciLexer_native::sck_right.
    key_home = sciLexer_native::sck_home.
    key_end = sciLexer_native::sck_end.
    key_prior = sciLexer_native::sck_prior.
    key_next = sciLexer_native::sck_next.
    key_delete = sciLexer_native::sck_delete.
    key_insert = sciLexer_native::sck_insert.
    key_escape = sciLexer_native::sck_escape.
    key_back = sciLexer_native::sck_back.
    key_tab = sciLexer_native::sck_tab.
    key_return = sciLexer_native::sck_return.
    key_add = sciLexer_native::sck_add.
    key_subtract = sciLexer_native::sck_subtract.
    key_divide = sciLexer_native::sck_divide.
    key_win = sciLexer_native::sck_win.
    key_rwin = sciLexer_native::sck_rwin.
    key_menu = sciLexer_native::sck_menu.
    % @short Key value for use with e.g. assignCmdKey
    % @end

constants
    shift = scmod_shift * 2 ^ 16.
    ctrl = scmod_ctrl * 2 ^ 16.
    alt = scmod_alt * 2 ^ 16.
    % @short Key modifers value for use with e.g. assignCmdKey
    % @end

predicates
    assignCmdKey : (unsigned KeyDefinition, integer SciCommand).
    % This assigns the given key definition to a Scintilla command identified by sciCommand. sciCommand can be any SCI_*
    % command that has no arguments.

predicates
    clearCmdKey : (unsigned KeyDefinition).
    % This makes the given key definition do nothing by assigning the action SCI_NULL to it.

predicates
    clearAllCmdKeys : ().
    % This command removes all keyboard command mapping by setting an empty mapping table.

properties
    usePopup : boolean (i).
    % Clicking the wrong button on the mouse pops up a short default editing menu.
    % This may be turned off with SCI_USEPOPUP(0).
    % If you turn it off, context menu commands (in Windows, WM_CONTEXTMENU) will not be handled by Scintilla, so the parent of the Scintilla
    % window will have the opportunity to handle the message.
    %

% Macro recording
%
% Start and stop macro recording mode. In macro recording mode, actions are reported to the container through
% SCN_MACRORECORD notifications. It is then up to the container to record these actions for future replay.
%
predicates
    startRecord : ().

predicates
    stopRecord : ().
    % These two messages turn macro recording on and off.
    %

% Printing
%
% SCI_FORMATRANGE can be used to draw the text onto a display surface which can include a printer display surface.
% Printed output shows text styling as on the screen, but it hides all margins except a line number margin. All special
% marker effects are removed and the selection and caret are hidden.
%
% Different platforms use different display surface ID types to print on. On Windows, these are HDCs., on GTK+ 3.x
% cairo_t *, and on Cocoa CGContextRef is used.
% SCI_FORMATRANGE(bool bDraw, Sci_RangeToFormat *pfr)
% SCI_SETPRINTMAGNIFICATION(int magnification)
% SCI_GETPRINTMAGNIFICATION
% SCI_SETPRINTCOLOURMODE(int mode)
% SCI_GETPRINTCOLOURMODE
% SCI_SETPRINTWRAPMODE
% SCI_GETPRINTWRAPMODE
%
predicates
    formatRange : (boolean BDraw, sci_rangeToFormat Pfr).
    % This call allows Windows users to render a range of text into a device context.
    % If you use this for printing, you will probably want to arrange a page header and footer; Scintilla does not do this for you.
    % See SciTEWin::Print() in SciTEWinDlg.cxx for an example.
    % Each use of this message renders a range of text into a rectangular area and returns the position in the document of the next character to print.
    %

%
% bDraw controls if any output is done. Set this to false if you are paginating (for example, if you use this with MFC
% you will need to paginate in OnBeginPrinting() before you output each page.
% struct Sci_Rectangle { int left; int top; int right; int bottom; };
%
% struct Sci_RangeToFormat {
% Sci_SurfaceID hdc; // The Surface ID we print to
% Sci_SurfaceID hdcTarget; // The Surface ID we use for measuring (may be same as hdc)
% Sci_Rectangle rc; // Rectangle in which to print
% Sci_Rectangle rcPage; // Physically printable page size
% Sci_CharacterRange chrg; // Range of characters to print
% };
%
% On Windows, hdc and hdcTarget should both be set to the device context handle of the output device (usually a printer).
% If you print to a metafile these will not be the same as Windows metafiles (unlike extended metafiles) do not implement
% the full API for returning information. In this case, set hdcTarget to the screen DC.
% rcPage is the rectangle {0, 0, maxX, maxY} where maxX+1 and maxY+1 are the number of physically printable pixels in x
% and y.
% rc is the rectangle to render the text in (which will, of course, fit within the rectangle defined by rcPage).
% chrg.cpMin and chrg.cpMax define the start position and maximum position of characters to output. All of each line
% within this character range is drawn.
%
% On Cocoa, the surface IDs for printing (bDraw=1) should be the graphics port of the current context ((CGContextRef)
% [[NSGraphicsContext currentContext] graphicsPort]) when the view's drawRect method is called. The Surface IDs are not
% really used for measurement (bDraw=0) but can be set to a bitmap context (created with CGBitmapContextCreate) to avoid
% runtime warnings.
%
% On GTK+, the surface IDs to use can be found from the printing context with
% gtk_print_context_get_cairo_context(context).
%
% chrg.cpMin and chrg.cpMax define the start position and maximum position of characters to output. All of each line
% within this character range is drawn.
%
% When printing, the most tedious part is always working out what the margins should be to allow for the non-printable
% area of the paper and printing a header and footer. If you look at the printing code in SciTE, you will find that most
% of it is taken up with this. The loop that causes Scintilla to render text is quite simple if you strip out all the
% margin, non-printable area, header and footer code.
%
properties
    printMagnification : integer.
    % SCI_GETPRINTMAGNIFICATION lets you to print at a different size than the screen font.
    % magnification is the number of points to add to the size of each screen font.
    % A value of -3 or -4 gives reasonably small print.
    % You can get this value with SCI_GETPRINTMAGNIFICATION.

properties
    printColorMode : integer.
    % These two messages set and get the method used to render coloured text on a printer that is probably using white paper.
    % It is especially important to consider the treatment of colour if you use a dark or black screen background. Printing
    % white on black uses up toner and ink very many times faster than the other way around. You can set the mode to one of:
    %
    % Symbol Value
    %
    % Purpose
    %
    % SC_PRINT_NORMAL 0 Print using the current screen colours. This is the default.
    % SC_PRINT_INVERTLIGHT 1 If you use a dark screen background this saves ink by inverting the light value of all colours
    % and printing on a white background.
    % SC_PRINT_BLACKONWHITE 2 Print all text as black on a white background.
    % SC_PRINT_COLOURONWHITE 3 Everything prints in its own colour on a white background.
    % SC_PRINT_COLOURONWHITEDEFAULTBG 4 Everything prints in its own colour on a white background except that line numbers
    % use their own background colour.
    %

properties
    printWrapMode : integer.
    % These two functions get and set the printer wrap mode.
    % wrapMode can be set to sc_wrap_none (0), sc_wrap_word (1) or sc_wrap_char (2).
    % The default is sc_wrap_word, which wraps printed output so that all characters fit into the print rectangle.
    % If you set sc_wrap_none, each line of text generates one line of output and the line is truncated if it is too long to fit into the print area.
    % sc_wrap_word tries to wrap only between words as indicated by white space or style changes although if a word is longer than a line, it will be wrapped before the line end.
    % sc_wrap_char is preferred to sc_wrap_word for Asian languages where there is no white space between words.

% Direct access
%
% On Windows, the message-passing scheme used to communicate between the container and Scintilla is mediated by the
% operating system SendMessage function and can lead to bad performance when calling intensively. To avoid this overhead,
% Scintilla provides messages that allow you to call the Scintilla message function directly. The code to do this in C/C++
% is of the form:
% #include "Scintilla.h"
% SciFnDirect pSciMsg = (SciFnDirect)SendMessage(hSciWnd, SCI_GETDIRECTFUNCTION, 0, 0);
% sptr_t pSciWndData = (sptr_t)SendMessage(hSciWnd, SCI_GETDIRECTPOINTER, 0, 0);
%
% // now a wrapper to call Scintilla directly
% sptr_t CallScintilla(unsigned int iMessage, uptr_t wParam, sptr_t lParam){
% return pSciMsg(pSciWndData, iMessage, wParam, lParam);
% }
%
% SciFnDirect, sptr_t and uptr_t are declared in Scintilla.h. hSciWnd is the window handle returned when you created the
% Scintilla window.
%
% While faster, this direct calling will cause problems if performed from a different thread to the native thread of the
% Scintilla window in which case SendMessage(hSciWnd, SCI_*, wParam, lParam) should be used to synchronize with the
% window's thread.
%
% This feature also works on GTK+ but has no significant impact on speed.
%
% From version 1.47 on Windows, Scintilla exports a function called Scintilla_DirectFunction that can be used the same as
% the function returned by SCI_GETDIRECTFUNCTION. This saves you the call to SCI_GETDIRECTFUNCTION and the need to call
% Scintilla indirectly via the function pointer.
%
% SCI_GETDIRECTFUNCTION
% This message returns the address of the function to call to handle Scintilla messages without the overhead of passing
% through the Windows messaging system. You need only call this once, regardless of the number of Scintilla windows you
% create.
%
predicates
    getCharacterPointer : () -> pointer CharPointer.
    % Move the gap within Scintilla so that the text of the document is stored consecutively and ensure there is a NUL character after the text,
    % then return a pointer to the first character.
    % Applications may then pass this to a function that accepts a character pointer such as a regular expression search or a parser.
    % The pointer should not be written to as that may desynchronize the internal state of Scintilla.
    %
    % Since any runnable in Scintilla may change its internal state this pointer becomes invalid after any call or by allowing user interface
    % activity.
    % The application should reacquire the pointer after making any call to Scintilla or performing any user-interface calls such as modifying a progress indicator.
    % This call takes similar time to inserting a character at the end of the document and this may include moving the document contents.
    % Specifically, all the characters after the document gap are moved to before the gap.
    % This compacted state should persist over calls and user interface actions that do not change the document contents so reacquiring the pointer afterwards is very quick.
    % If this call is used to implement a global replace operation, then each replacement will move the gap so if SCI_GETCHARACTERPOINTER is called after
    % each replacement then the operation will become O(n^2) rather than O(n).
    % Instead, all matches should be found and remembered, then all the replacements performed.

predicates
    getRangePointer : (integer Position, integer RangeLength) -> pointer CharPointer.
    % SCI_GETRANGEPOINTER provides direct access to just the range requested. The gap is not moved unless it is within the
    % requested range so this call can be faster than SCI_GETCHARACTERPOINTER. This can be used by application code that is
    % able to act on blocks of text or ranges of lines.

predicates
    getGapPosition : () -> pointer GabPointer.
    % Grant temporary direct read-only access to the memory used by Scintilla to store the document.
    %
    % SCI_GETCHARACTERPOINTER moves the gap within Scintilla so that the text of the document is stored consecutively and
    % ensure there is a NUL character after the text, then returns a pointer to the first character. Applications may then
    % pass this to a function that accepts a character pointer such as a regular expression search or a parser. The pointer
    % should not be written to as that may desynchronize the internal state of Scintilla.
    %
    % Since any action in Scintilla may change its internal state this pointer becomes invalid after any call or by allowing
    % user interface activity. The application should reacquire the pointer after making any call to Scintilla or performing
    % any user-interface calls such as modifying a progress indicator.
    %
    % This call takes similar time to inserting a character at the end of the document and this may include moving the
    % document contents. Specifically, all the characters after the document gap are moved to before the gap. This compacted
    % state should persist over calls and user interface actions that do not change the document contents so reacquiring the
    % pointer afterwards is very quick. If this call is used to implement a global replace operation, then each replacement
    % will move the gap so if SCI_GETCHARACTERPOINTER is called after each replacement then the operation will become O(n^2)
    % rather than O(n). Instead, all matches should be found and remembered, then all the replacements performed.
    %
    % SCI_GETRANGEPOINTER provides direct access to just the range requested. The gap is not moved unless it is within the
    % requested range so this call can be faster than SCI_GETCHARACTERPOINTER. This can be used by application code that is
    % able to act on blocks of text or ranges of lines.
    %
    % SCI_GETGAPPOSITION returns the current gap position. This is a hint that applications can use to avoid calling
    % SCI_GETRANGEPOINTER with a range that contains the gap and consequent costs of moving the gap.
    %

% Multiple views
%
% A Scintilla window and the document that it displays are separate entities. When you create a new window, you also
% create a new, empty document. Each document has a reference count that is initially set to 1. The document also has a
% list of the Scintilla windows that are linked to it so when any window changes the document, all other windows in which
% it appears are notified to cause them to update. The system is arranged in this way so that you can work with many
% documents in a single Scintilla window and so you can display a single document in multiple windows (for use with
% splitter windows).
%
% Although these messages use document *pDoc, to ensure compatibility with future releases of Scintilla you should treat
% pDoc as an opaque void*. That is, you can use and store the pointer as described in this section but you should not
% dereference it.
% SCI_GETDOCPOINTER
% SCI_SETDOCPOINTER(<unused>, document *pDoc)
% SCI_CREATEDOCUMENT
% SCI_ADDREFDOCUMENT(<unused>, document *pDoc)
% SCI_RELEASEDOCUMENT(<unused>, document *pDoc)
%
properties
    docPointer : document.
    % GetTing
    % This returns a pointer to the document currently in use by the window.
    % It has no other effect.
    % Setting:
    % This message does the following:
    % 1. It removes the current window from the list held by the current document.
    % 2. It reduces the reference count of the current document by 1.
    % 3. If the reference count reaches 0, the document is deleted.
    % 4. pDoc is set as the new document for the window.
    % 5. If pDoc was 0, a new, empty document is created and attached to the window.
    % 6. If pDoc was not 0, its reference count is increased by 1.

predicates
    createDocument : ().
    % This message creates a new, empty document and returns a pointer to it.
    % This document is not selected into the editor and starts with a reference count of 1.
    % This means that you have ownership of it and must either reduce its reference count by 1 after using SCI_SETDOCPOINTER so that the Scintilla window owns it or you must make
    % sure that you reduce the reference count by 1 with
    % SCI_RELEASEDOCUMENT before you close the application to avoid memory leaks.

predicates
    addrefDocument : (document PDoc).
    % This increases the reference count of a document by 1. If you want to replace the current document in the Scintilla
    % window and take ownership of the current document, for example if you are editing many documents in one window, do the
    % following:
    % 1. Use SCI_GETDOCPOINTER to get a pointer to the document, pDoc.
    % 2. Use SCI_ADDREFDOCUMENT(0, pDoc) to increment the reference count.
    % 3. Use SCI_SETDOCPOINTER(0, pNewDoc) to set a different document or SCI_SETDOCPOINTER(0, 0) to set a new, empty
    % document.
    %

predicates
    releaseDocument : (document PDoc).
    % This message reduces the reference count of the document identified by pDoc. pDoc must be the result of
    % SCI_GETDOCPOINTER or SCI_CREATEDOCUMENT and must point at a document that still exists. If you call this on a document
    % with a reference count of 1 that is still attached to a Scintilla window, bad things will happen. To keep the world
    % spinning in its orbit you must balance each call to SCI_CREATEDOCUMENT or SCI_ADDREFDOCUMENT with a call to
    % SCI_RELEASEDOCUMENT.

% Background loading and saving
%
% To ensure a responsive user interface, applications may decide to load and save documents using a separate thread from
% the user interface.
%
% Loading in the background
%
% An application can load all of a file into a buffer it allocates on a background thread and then add the data in that
% buffer into a Scintilla document on the user interface thread. That technique uses extra memory to store a complete copy
% of the file and also means that the time that Scintilla takes to perform initial line end discovery blocks the user
% interface.
%
% To avoid these issues, a loader object may be created and used to load the file. The loader object supports the ILoader
% interface.
%
% SCI_CREATELOADER(int bytes)
% Create an object that supports the ILoader interface which can be used to load data and then be turned into a Scintilla
% document object for attachment to a view object. The bytes argument determines the initial memory allocation for the
% document as it is more efficient to allocate once rather than rely on the buffer growing as data is added. If
% SCI_CREATELOADER fails then 0 is returned.
%
% ILoader
%
% class ILoader {
% public:
% virtual int SCI_METHOD Release() = 0;
% // Returns a status code from SC_STATUS_*
% virtual int SCI_METHOD AddData(char *data, int length) = 0;
% virtual void * SCI_METHOD ConvertToDocument() = 0;
% };
%
% The application should call the AddData method with each block of data read from the file. AddData will return
% SC_STATUS_OK unless a failure, such as memory exhaustion occurs. If a failure occurs in AddData or in a file reading
% call then loading can be abandoned and the loader released with the Release call. When the whole file has been read, the
% ConvertToDocument method should be called to produce a Scintilla document pointer which can be used in the same way as a
% document pointer returned from SCI_CREATEDOCUMENT. There is no need to call Release after ConvertToDocument.
%
% Saving in the background
%
% An application that wants to save in the background should lock the document with SCI_SETREADONLY(1) to prevent
% modifications and retrieve a pointer to the unified document contents with SCI_GETCHARACTERPOINTER. The buffer of a
% locked document will not move so the pointer is valid until the application calls SCI_SETREADONLY(0).
%
% If the user tries to performs a modification while the document is locked then a SCN_MODIFYATTEMPTRO notification is
% sent to the application. The application may then decide to ignore the modification or to terminate the background
% saving thread and reenable modification before returning from the notification.
%
% Folding
%
% The fundamental operation in folding is making lines invisible or visible. Line visibility is a property of the view
% rather than the document so each view may be displaying a different set of lines. From the point of view of the user,
% lines are hidden and displayed using fold points. Generally, the fold points of a document are based on the hierarchical
% structure of the document contents. In Python, the hierarchy is determined by indentation and in C++ by brace
% characters. This hierarchy can be represented within a Scintilla document object by attaching a numeric "fold level" to
% each line. The fold level is most easily set by a lexer, but you can also set it with messages.
%
% It is up to your code to set the connection between user actions and folding and unfolding. The best way to see how
% this is done is to search the SciTE source code for the messages used in this section of the documentation and see how
% they are used. You will also need to use markers and a folding margin to complete your folding implementation. The
% "fold" property should be set to "1" with SCI_SETPROPERTY("fold", "1") to enable folding.
predicates
    visibleFromDocLine : (integer DocLine).
    % When some lines are hidden and/or annotations are displayed, then a particular line in the document may be displayed at
    % a different position to its document position. If no lines are hidden and there are no annotations, this message returns
    % docLine. Otherwise, this returns the display line (counting the very first visible line as 0). The display line of an
    % invisible line is the same as the previous visible line. The display line number of the first line in the document is 0.
    % If lines are hidden and docLine is outside the range of lines in the document, the return value is -1. Lines can occupy
    % more than one display line if they wrap.
    %

predicates
    docLineFromVisible : (integer DisplayLine).
    % When some lines are hidden and/or annotations are displayed, then a particular line in the document may be displayed at
    % a different position to its document position. This message returns the document line number that corresponds to a
    % display line (counting the display line of the first line in the document as 0). If displayLine is less than or equal to
    % 0, the result is 0. If displayLine is greater than or equal to the number of displayed lines, the result is the number
    % of lines in the document.
    %

predicates
    showLines : (lineNumber LineStart, lineNumber LineEnd).
    hideLines : (lineNumber LineStart, lineNumber LineEnd).
    % The first two messages mark a range of lines as visible or invisible and then redraw the display.

predicates
    isLineVisible : (lineNumber Line) determ.
    % The third message reports on the visible state of a line and returns 1 if it is visible and 0 if it is not visible.
    % These messages have no effect on fold levels or fold flags.
    % The first line can not be hidden.

properties
    isAllLinesVisible : boolean (o).
    % returns true if all lines are visible and false if some lines are hidden.
    % It have no effect on fold levels or fold flags. The first line can not be hidden.

predicates
    setFoldLevel : (lineNumber Line, integer Level).

predicates
    getFoldLevel : (lineNumber Line) -> integer Level.
    % These two messages set and get a 32-bit value that contains the fold level of a line and some flags associated with
    % folding. The fold level is a number in the range 0 to SC_FOLDLEVELNUMBERMASK (4095). However, the initial fold level is
    % set to SC_FOLDLEVELBASE (1024) to allow unsigned arithmetic on folding levels. There are two addition flag bits.
    % SC_FOLDLEVELWHITEFLAG indicates that the line is blank and allows it to be treated slightly different then its level may
    % indicate. For example, blank lines should generally not be fold points and will be considered part of the preceding
    % section even though they may have a lesser fold level. SC_FOLDLEVELHEADERFLAG indicates that the line is a header (fold
    % point).
    %
    % Use SCI_GETFOLDLEVEL(line) & SC_FOLDLEVELNUMBERMASK to get the fold level of a line. Likewise, use
    % SCI_GETFOLDLEVEL(line) & SC_FOLDLEVEL*FLAG to get the state of the flags. To set the fold level you must or in the
    % associated flags. For instance, to set the level to thisLevel and mark a line as being a fold point use:
    % SCI_SETFOLDLEVEL(line, thisLevel | SC_FOLDLEVELHEADERFLAG).
    % If you use a lexer, you should not need to use SCI_SETFOLDLEVEL as this is far better handled by the lexer. You will
    % need to use SCI_GETFOLDLEVEL to decide how to handle user folding requests. If you do change the fold levels, the
    % folding margin will update to match your changes.

properties
    foldFlags : integer (i).
    % In addition to showing markers in the folding margin, you can indicate folds to the user by drawing lines in the text
    % area. The lines are drawn in the foreground colour set for STYLE_DEFAULT. Bits set in flags determine where folding
    % lines are drawn:
    %
    % Symbol Value Effect
    %
    % 1 Experimental feature that has been removed.
    % SC_FOLDFLAG_LINEBEFORE_EXPANDED 2 Draw above if expanded
    % SC_FOLDFLAG_LINEBEFORE_CONTRACTED 4 Draw above if not expanded
    % SC_FOLDFLAG_LINEAFTER_EXPANDED 8 Draw below if expanded
    % SC_FOLDFLAG_LINEAFTER_CONTRACTED 16 Draw below if not expanded
    % SC_FOLDFLAG_LEVELNUMBERS 64 display hexadecimal fold levels in line margin to aid debugging of folding. The appearance
    % of this feature may change in the future.
    % SC_FOLDFLAG_LINESTATE 128 display hexadecimal line state in line margin to aid debugging of lexing and folding. May not
    % be used at the same time as SC_FOLDFLAG_LEVELNUMBERS.
    %
    % This message causes the display to redraw.
    %

predicates
    getLastChild : (integer StartLine, integer Level) -> lineNumber LineNumber.
    % This message searches for the next line after startLine, that has a folding level that is less than or equal to level
    % and then returns the previous line number. If you set level to -1, level is set to the folding level of line startLine.
    % If from is a fold point, SCI_GETLASTCHILD(from, -1) returns the last line that would be in made visible or hidden by
    % toggling the fold state.
    %

predicates
    getFoldParent : (integer StartLine) -> lineNumber LineNumber.
    % This message returns the line number of the first line before startLine that is marked as a fold point with
    % SC_FOLDLEVELHEADERFLAG and has a fold level less than the startLine. If no line is found, or if the header flags and
    % fold levels are inconsistent, the return value is -1.
    %

predicates
    toggleFold : (lineNumber Line).
    % Each fold point may be either expanded, displaying all its child lines, or contracted, hiding all the child lines. This
    % message toggles the folding state of the given line as long as it has the SC_FOLDLEVELHEADERFLAG set. This message takes
    % care of folding or expanding all the lines that depend on the line. The display updates after this message.
    %

predicates
    setFoldExpanded : (lineNumber Line, boolean Expanded).

predicates
    getFoldExpanded : (lineNumber Line) -> boolean Expanded.
    % These messages set and get the expanded state of a single line. The set message has no effect on the visible state of
    % the line or any lines that depend on it. It does change the markers in the folding margin. If you ask for the expansion
    % state of a line that is outside the document, the result is false (0).
    %
    % If you just want to toggle the fold state of one line and handle all the lines that are dependent on it, it is much
    % easier to use SCI_TOGGLEFOLD. You would use the SCI_SETFOLDEXPANDED message to process many folds without updating the
    % display until you had finished. See SciTEBase::FoldAll() and SciTEBase::Expand() for examples of the use of these
    % messages.
    %
    % SCI_FOLDLINE(int line, int action)
    % SCI_FOLDCHILDREN(int line, int action)
    % SCI_FOLDALL(int action)
    % These messages provide a higher-level approach to folding instead of setting expanded flags and showing or hiding
    % individual lines.
    %

%
% To affect the entire document call SCI_FOLDALL. With SC_FOLDACTION_TOGGLE the first fold header in the document is
% examined to decide whether to expand or contract.
%
% Symbol Value Effect
%
% SC_FOLDACTION_CONTRACT 0 Contract.
% SC_FOLDACTION_EXPAND 1 Expand.
% SC_FOLDACTION_TOGGLE 2 Toggle between contracted and expanded.
%
% SCI_EXPANDCHILDREN(int line, int level)
% This is used to respond to a change to a line causing its fold level or whether it is a header to change, perhaps when
% adding or removing a '{'.
%
% By the time the container has received the notification that the line has changed, the fold level has already been set,
% so the container has to use the previous level in this call so that any range hidden underneath this line can be shown.
%
% SCI_SETAUTOMATICFOLD(int automaticFold)
% SCI_GETAUTOMATICFOLD
% Instead of implementing all the logic for handling folding in the container, Scintilla can provide behaviour that is
% adequate for many applications. The automaticFold argument is a bit set defining which of the 3 pieces of folding
% implementation should be enabled. Most applications should be able to use the SC_AUTOMATICFOLD_SHOW and
% SC_AUTOMATICFOLD_CHANGE flags unless they wish to implement quite different behaviour such as defining their own fold
% structure. SC_AUTOMATICFOLD_CLICK is more likely to be set off when an application would like to add or change click
% behaviour such as showing method headers only when Shift+Alt is used in conjunction with a click.
%
% Symbol Value Effect
%
% SC_AUTOMATICFOLD_SHOW 1 Automatically show lines as needed. This avoids sending the SCN_NEEDSHOWN notification.
% SC_AUTOMATICFOLD_CLICK 2 Handle clicks in fold margin automatically. This avoids sending the SCN_MARGINCLICK
% notification for folding margins.
% SC_AUTOMATICFOLD_CHANGE 4 Show lines as needed when fold structure is changed. The SCN_MODIFIED notification is still
% sent unless it is disabled by the container.
%
predicates
    contractedFoldNext : (lineNumber LineStart).
    % Search efficiently for lines that are contracted fold headers. This is useful when saving the user's folding when
    % switching documents or saving folding with a file. The search starts at line number lineStart and continues forwards to
    % the end of the file. lineStart is returned if it is a contracted fold header otherwise the next contracted fold header
    % is returned. If there are no more contracted fold headers then -1 is returned.
    %

predicates
    ensureVisible : (lineNumber Line).

predicates
    ensureVisibleEnforcePolicy : (lineNumber Line).
    % A line may be hidden because more than one of its parent lines is contracted. Both these message travels up the fold
    % hierarchy, expanding any contracted folds until they reach the top level. The line will then be visible. If you use
    % SCI_ENSUREVISIBLEENFORCEPOLICY, the vertical caret policy set by SCI_SETVISIBLEPOLICY is then applied.

% Line wrapping
% SCI_SETWRAPMODE(int wrapMode)
% SCI_GETWRAPMODE
% SCI_SETWRAPVISUALFLAGS(int wrapVisualFlags)
% SCI_GETWRAPVISUALFLAGS
% SCI_SETWRAPVISUALFLAGSLOCATION(int wrapVisualFlagsLocation)
% SCI_GETWRAPVISUALFLAGSLOCATION
% SCI_SETWRAPINDENTMODE(int indentMode)
% SCI_GETWRAPINDENTMODE
% SCI_SETWRAPSTARTINDENT(int indent)
% SCI_GETWRAPSTARTINDENT
% SCI_SETLAYOUTCACHE(int cacheMode)
% SCI_GETLAYOUTCACHE
% SCI_SETPOSITIONCACHE(int size)
% SCI_GETPOSITIONCACHE
% SCI_LINESSPLIT(int pixelWidth)
% SCI_LINESJOIN
% SCI_WRAPCOUNT(int docLine)
%
% By default, Scintilla does not wrap lines of text. If you enable line wrapping, lines wider than the window width are
% continued on the following lines. Lines are broken after space or tab characters or between runs of different styles. If
% this is not possible because a word in one style is wider than the window then the break occurs after the last character
% that completely fits on the line. The horizontal scroll bar does not appear when wrap mode is on.
%
% For wrapped lines Scintilla can draw visual flags (little arrows) at end of a a subline of a wrapped line and at begin
% of the next subline. These can be enabled individually, but if Scintilla draws the visual flag at the beginning of the
% next subline this subline will be indented by one char. Independent from drawing a visual flag at the begin the subline
% can have an indention.
%
% Much of the time used by Scintilla is spent on laying out and drawing text. The same text layout calculations may be
% performed many times even when the data used in these calculations does not change. To avoid these unnecessary
% calculations in some circumstances, the line layout cache can store the results of the calculations. The cache is
% invalidated whenever the underlying data, such as the contents or styling of the document changes. Caching the layout of
% the whole document has the most effect, making dynamic line wrap as much as 20 times faster but this requires 7 times
% the memory required by the document contents plus around 80 bytes per line.
%
% Wrapping is not performed immediately there is a change but is delayed until the display is redrawn. This delay
% improves performance by allowing a set of changes to be performed and then wrapped and displayed once. Because of this,
% some operations may not occur as expected. If a file is read and the scroll position moved to a particular line in the
% text, such as occurs when a container tries to restore a previous editing session, then the scroll position will have
% been determined before wrapping so an unexpected range of text will be displayed. To scroll to the position correctly,
% delay the scroll until the wrapping has been performed by waiting for an initial SCN_PAINTED notification.
%
properties
    wrapMode : integer.
    % Set wrapMode to (sciLexer_native::sc_wrap_*):
    % sc_wrap_none (0) to disable line wrapping,
    % sc_wrap_word (1) to enable wrapping on word or style boundaries
    % sc_wrap_char (2) to enable wrapping between any characters, and
    % sc_wrap_whitespace (3) to enable wrapping on whitespace.
    % sc_wrap_char is preferred for Asian languages where there is no white space between words.

properties
    wrapvisualFlags : integer.
    % You can enable the drawing of visual flags to indicate a line is wrapped. Bits set in wrapVisualFlags determine which
    % visual flags are drawn.
    %
    % Symbol Value Effect
    %
    % SC_WRAPVISUALFLAG_NONE 0 No visual flags
    % SC_WRAPVISUALFLAG_END 1 Visual flag at end of subline of a wrapped line.
    % SC_WRAPVISUALFLAG_START 2 Visual flag at begin of subline of a wrapped line.
    % Subline is indented by at least 1 to make room for the flag.
    %
    % SC_WRAPVISUALFLAG_MARGIN 4 Visual flag in line number margin.
    %

properties
    wrapvisualFlagsLocation : integer.
    % You can set whether the visual flags to indicate a line is wrapped are drawn near the border or near the text. Bits set
    % in wrapVisualFlagsLocation set the location to near the text for the corresponding visual flag.
    %
    % Symbol Value Effect
    %
    % SC_WRAPVISUALFLAGLOC_DEFAULT 0 Visual flags drawn near border
    % SC_WRAPVISUALFLAGLOC_END_BY_TEXT 1 Visual flag at end of subline drawn near text
    % SC_WRAPVISUALFLAGLOC_START_BY_TEXT 2 Visual flag at beginning of subline drawn near text
    %

properties
    wrapIndentMode : integer.
    % Wrapped sublines can be indented to the position of their first subline or one more indent level. The default is
    % SC_WRAPINDENT_FIXED. The modes are:
    %
    % Symbol Value Effect
    %
    % SC_WRAPINDENT_FIXED 0 Wrapped sublines aligned to left of window plus amount set by SCI_SETWRAPSTARTINDENT
    % SC_WRAPINDENT_SAME 1 Wrapped sublines are aligned to first subline indent
    % SC_WRAPINDENT_INDENT 2 Wrapped sublines are aligned to first subline indent plus one more level of indentation
    %

properties
    wrapStartIndent : integer.
    % SCI_SETWRAPSTARTINDENT sets the size of indentation of sublines for wrapped lines in terms of the average character
    % width in STYLE_DEFAULT. There are no limits on indent sizes, but values less than 0 or large values may have undesirable
    % effects.
    % The indention of sublines is independent of visual flags, but if SC_WRAPVISUALFLAG_START is set an indent of at least 1
    % is used.
    %

properties
    layoutcache : integer.
    % You can set cacheMode to one of the symbols in the table:
    %
    % Symbol Value
    %
    % Layout cached for these lines
    %
    % SC_CACHE_NONE 0 No lines are cached.
    % SC_CACHE_CARET 1 The line containing the text caret. This is the default.
    % SC_CACHE_PAGE 2 Visible lines plus the line containing the caret.
    % SC_CACHE_DOCUMENT 3 All lines in the document.
    %

properties
    positionCache : integer.
    % The position cache stores position information for short runs of text so that their layout can be determined more
    % quickly if the run recurs. The size in entries of this cache can be set with SCI_SETPOSITIONCACHE.
    %

predicates
    linesSplit : (integer PixelWidth).
    % Split a range of lines indicated by the target into lines that are at most pixelWidth wide. Splitting occurs on word
    % boundaries wherever possible in a similar manner to line wrapping. When pixelWidth is 0 then the width of the window is
    % used.
    %

predicates
    linesJoin : ().
    % Join a range of lines indicated by the target into one line by removing line end characters. Where this would lead to
    % no space between words, an extra space is inserted.
    %

predicates
    wrapCount : (integer DocLine).
    % Document lines can occupy more than one display line if they wrap and this returns the number of display lines needed
    % to wrap a document line.
    %

% Zooming
%
% Scintilla incorporates a "zoom factor" that lets you make all the text in the document larger or smaller in steps of
% one point. The displayed point size never goes below 2, whatever zoom factor you set. You can set zoom factors in the
% range -10 to +20 points.
% SCI_ZOOMIN
% SCI_ZOOMOUT
% SCI_SETZOOM(int zoomInPoints)
% SCI_GETZOOM
%
predicates
    zoomIn : ().

predicates
    zoomOut : ().
    % SCI_ZOOMIN increases the zoom factor by one point if the current zoom factor is less than 20 points. SCI_ZOOMOUT
    % decreases the zoom factor by one point if the current zoom factor is greater than -10 points.
    %

properties
    zoom : integer.
    % These messages let you set and get the zoom factor directly. There is no limit set on the factors you can set, so
    % limiting yourself to -10 to +20 to match the incremental zoom functions is a good idea.
    %

% Long lines
%
% You can choose to mark lines that exceed a given length by drawing a vertical line or by colouring the background of
% characters that exceed the set length.
% SCI_SETEDGEMODE(int mode)
% SCI_GETEDGEMODE
% SCI_SETEDGECOLUMN(int column)
% SCI_GETEDGECOLUMN
% SCI_SETEDGECOLOUR(int colour)
% SCI_GETEDGECOLOUR
%
properties
    edgeMode : integer.
    % SCI_SETEDGEMODE(int edgeMode)

% SCI_GETEDGEMODE
% These two messages set and get the mode used to display long lines. You can set one of the values in the table:
%
% Symbol Value
%
% Long line display mode
%
% EDGE_NONE 0 Long lines are not marked. This is the default state.
% EDGE_LINE 1 A vertical line is drawn at the column number set by SCI_SETEDGECOLUMN. This works well for monospaced
% fonts. The line is drawn at a position based on the width of a space character in STYLE_DEFAULT, so it may not work very
% well if your styles use proportional fonts or if your style have varied font sizes or you use a mixture of bold, italic
% and normal text.
% EDGE_BACKGROUND 2 The background colour of characters after the column limit is changed to the colour set by
% SCI_SETEDGECOLOUR. This is recommended for proportional fonts.
%
properties
    edgeColumn : integer.
    % These messages set and get the column number at which to display the long line marker. When drawing lines, the column
    % sets a position in units of the width of a space character in STYLE_DEFAULT. When setting the background colour, the
    % column is a character count (allowing for tabs) into the line.
    %

properties
    edgeColor : unsigned.
    % These messages set and get the colour of the marker used to show that a line has exceeded the length set by
    % SCI_SETEDGECOLUMN.
    %

% Lexer
%
% If you define the symbol SCI_LEXER when building Scintilla, (this is sometimes called the SciLexer version of
% Scintilla), lexing support for a wide range of programming languages is included and the messages in this section are
% supported. If you want to set styling and fold points for an unsupported language you can either do this in the
% container or better still, write your own lexer following the pattern of one of the existing ones.
%
% Scintilla also supports external lexers. These are DLLs (on Windows) or .so modules (on GTK+/Linux) that export three
% functions: GetLexerCount, GetLexerName, and GetLexerFactory. See externalLexer.cxx for more.
% SCI_SETLEXER(int lexer)
% SCI_GETLEXER
% SCI_SETLEXERLANGUAGE(<unused>, const char8 *name)
% SCI_GETLEXERLANGUAGE(<unused>, char8 *name)
% SCI_LOADLEXERLIBRARY(<unused>, const char8 *path)
% SCI_COLOURISE(int start, int end)
% SCI_CHANGELEXERSTATE(int start, int end)
% SCI_PROPERTYNAMES(<unused>, char8 *names)
% SCI_PROPERTYTYPE(const char8 *name)
% SCI_DESCRIBEPROPERTY(const char8 *name, char8 *description)
% SCI_SETPROPERTY(const char8 *key, const char8 *value)
% SCI_GETPROPERTY(const char8 *key, char8 *value)
% SCI_GETPROPERTYEXPANDED(const char8 *key, char8 *value)
% SCI_GETPROPERTYINT(const char8 *key, int default)
% SCI_DESCRIBEKEYWORDSETS(<unused>, char8 *descriptions)
% SCI_SETKEYWORDS(int keyWordSet, const char8 *keyWordList)
% SCI_GETSUBSTYLEBASES(<unused>, char8 *styles)
% SCI_DISTANCETOSECONDARYSTYLES
% SCI_ALLOCATESUBSTYLES(int styleBase, int numberStyles)
% SCI_FREESUBSTYLES
% SCI_GETSUBSTYLESSTART(int styleBase)
% SCI_GETSUBSTYLESLENGTH(int styleBase)
% SCI_GETSTYLEFROMSUBSTYLE(int subStyle)
% SCI_GETPRIMARYSTYLEFROMSTYLE(int style)
% SCI_SETIDENTIFIERS(int style, const char8 *identifiers)
%
properties
    lexer : integer.
    % You can select the lexer to use with an integer code from the SCLEX_* enumeration in Scintilla.h. There are two codes
    % in this sequence that do not use lexers: SCLEX_NULL to select no lexing action and SCLEX_CONTAINER which sends the
    % SCN_STYLENEEDED notification to the container whenever a range of text needs to be styled. You cannot use the
    % SCLEX_AUTOMATIC value; this identifies additional external lexers that Scintilla assigns unused lexer numbers to.
    %

properties
    lexerLanguage : string.
    % SCI_SETLEXERLANGUAGE lets you select a lexer by name, and is the only method if you are using an external lexer or if
    % you have written a lexer module for a language of your own and do not wish to assign it an explicit lexer number. To
    % select an existing lexer, set name to match the (case sensitive) name given to the module, for example "ada" or
    % "python", not "Ada" or "Python". To locate the name for the built-in lexers, open the relevant Lex*.cxx file and search
    % for LexerModule. The third argument in the LexerModule constructor is the name to use.
    %
    % To test if your lexer assignment worked, use SCI_GETLEXER before and after setting the new lexer to see if the lexer
    % number changed.
    %
    % SCI_GETLEXERLANGUAGE retrieves the name of the lexer.
    %

predicates
    loadLexerLibrary : (string Path).
    % SCI_LOADLEXERLIBRARY(<unused>, const char8 *path)
    % Load a lexer implemented in a shared library. This is a .so file on GTK+/Linux or a .DLL file on Windows.
    %

predicates
    colorise : (integer StartPos, integer EndPos).
    % This requests the current lexer or the container (if the lexer is set to SCLEX_CONTAINER) to style the document between
    % startPos and endPos. If endPos is -1, the document is styled from startPos to the end. If the "fold" property is set to
    % "1" and your lexer or container supports folding, fold levels are also set. This message causes a redraw.
    %

predicates
    changeLexerState : (integer StartPos, integer EndPos).
    % Indicate that the internal state of a lexer has changed over a range and therefore there may be a need to redraw.
    %

properties
    propertyNames : string (o).

predicates
    propertyType : (string Name).

predicates
    describeProperty : (string Name) -> string Description.
    % Information may be retrieved about the properties that can be set for the current lexer. This information is only
    % available for newer lexers. SCI_PROPERTYNAMES returns a string with all of the valid properties separated by "\n". If
    % the lexer does not support this call then an empty string is returned. Properties may be boolean (SC_TYPE_BOOLEAN),
    % integer (SC_TYPE_INTEGER), or string (SC_TYPE_STRING) and this is found with SCI_PROPERTYTYPE. A description of a
    % property in English is returned by SCI_DESCRIBEPROPERTY.
    %

predicates
    setProperty : (string Key, string Value).
    % SCI_SETPROPERTY(const char8 *key, const char8 *value)
    % You can communicate settings to lexers with keyword:value string pairs. There is no limit to the number of keyword
    % pairs you can set, other than available memory. key is a case sensitive keyword, value is a string that is associated
    % with the keyword. If there is already a value string associated with the keyword, it is replaced. If you pass a zero
    % length string, the message does nothing. Both key and value are used without modification; extra spaces at the beginning
    % or end of key are significant.
    %
    % The value string can refer to other keywords. For example, SCI_SETPROPERTY("foldTimes10", "$(fold)0") stores the string
    % "$(fold)0", but when this is accessed, the $(fold) is replaced by the value of the "fold" keyword (or by nothing if this
    % keyword does not exist).
    %
    % Currently the "fold" property is defined for most of the lexers to set the fold structure if set to "1". SCLEX_PYTHON
    % understands "tab.timmy.whinge.level" as a setting that determines how to indicate bad indentation. Most keywords have
    % values that are interpreted as integers. Search the lexer sources for GetPropertyInt to see how properties are used.
    %
    % There is a convention for naming properties used by lexers so that the set of properties can be found by scripts.
    % Property names should start with "lexer.<lexer>." or "fold.<lexer>." when they apply to one lexer or start with "lexer."
    % or "fold." if they apply to multiple lexers.
    %
    % Applications may discover the set of properties used by searching the source code of lexers for lines that contain
    % GetProperty and a double quoted string and extract the value of the double quoted string as the property name. The
    % scintilla/scripts/LexGen.py script does this and can be used as an example. Documentation for the property may be
    % located above the call as a multi-line comment starting with
    % // property <property-name>
    %

predicates
    tryGetProperty : (string Key) -> string Value determ.
    % Lookup a keyword:value pair using the specified key; if found, copy the value to the user-supplied buffer and return
    % the length (not including the terminating 0). If not found, copy an empty string to the buffer and return 0.
    %
    % Note that "keyword replacement" as described in SCI_SETPROPERTY will not be performed.
    %
    % If the value argument is 0 then the length that should be allocated to store the value is returned; again, the
    % terminating 0 is not included.
    %

predicates
    tryGetPropertyExpanded : (string Key) -> string Value determ.
    % Lookup a keyword:value pair using the specified key; if found, copy the value to the user-supplied buffer and return
    % the length (not including the terminating 0). If not found, copy an empty string to the buffer and return 0.
    %
    % Note that "keyword replacement" as described in SCI_SETPROPERTY will be performed.
    %
    % If the value argument is 0 then the length that should be allocated to store the value (including any indicated keyword
    % replacement) is returned; again, the terminating 0 is not included.
    %

predicates
    getPropertyInt : (string Key, integer Default) -> integer Value.
    % Lookup a keyword:value pair using the specified key; if found, interpret the value as an integer and return it. If not
    % found (or the value is an empty string) then return the supplied default. If the keyword:value pair is found but is not
    % a number, then return 0.
    %
    % Note that "keyword replacement" as described in SCI_SETPROPERTY will be performed before any numeric interpretation.
    %

predicates
    setKeywords : (integer KeywordSet, string KeywordList).
    % You can set up to 9 lists of keywords for use by the current lexer. keyWordSet can be 0 to 8 (actually 0 to
    % KEYWORDSET_MAX) and selects which keyword list to replace. keyWordList is a list of keywords separated by spaces, tabs,
    % "\n" or "\r" or any combination of these. It is expected that the keywords will be composed of standard ASCII printing
    % characters, but there is nothing to stop you using any non-separator character codes from 1 to 255 (except common
    % sense).
    %
    % How these keywords are used is entirely up to the lexer. Some languages, such as HTML may contain embedded languages,
    % VBScript and JavaScript are common for HTML. For HTML, key word set 0 is for HTML, 1 is for JavaScript and 2 is for
    % VBScript, 3 is for Python, 4 is for PHP and 5 is for SGML and DTD keywords. Review the lexer code to see examples of
    % keyword list. A fully conforming lexer sets the fourth argument of the LexerModule constructor to be a list of strings
    % that describe the uses of the keyword lists.
    %
    % Alternatively, you might use set 0 for general keywords, set 1 for keywords that cause indentation and set 2 for
    % keywords that cause unindentation. Yet again, you might have a simple lexer that colours keywords and you could change
    % languages by changing the keywords in set 0. There is nothing to stop you building your own keyword lists into the
    % lexer, but this means that the lexer must be rebuilt if more keywords are added.
    %

predicates
    describeKeywordSets : (string Descriptions).
    % A description of all of the keyword sets separated by "\n" is returned by SCI_DESCRIBEKEYWORDSETS.
    %

% Substyles
%
% Lexers may support several different sublanguages and each sublanguage may want to style some number of sets of
% identifiers (or similar lexemes such as documentation keywords) uniquely. Preallocating a large number for each purpose
% would exhaust the number of allowed styles quickly. This is alleviated by substyles which allow the application to
% determine how many sets of identifiers to allocate for each purpose. Lexers have to explicitly support this feature by
% implementing the methods in ILexerWithSubStyles.
%
% SCI_GETSUBSTYLEBASES(<unused>, char8 *styles NUL-terminated)
% Fill styles with a byte for each style that can be split into substyles.
%
% SCI_DISTANCETOSECONDARYSTYLES
% Returns the distance between a primary style and its corresponding secondary style.
%
% SCI_ALLOCATESUBSTYLES(int styleBase, int numberStyles)
% Allocate some number of substyles for a particular base style returning the first substyle number allocated. Substyles
% are allocated contiguously.
%
% SCI_FREESUBSTYLES
% Free all allocated substyles.
%
% SCI_GETSUBSTYLESSTART(int styleBase)
% SCI_GETSUBSTYLESLENGTH(int styleBase)
% Return the start and length of the substyles allocated for a base style.
%
% SCI_GETSTYLEFROMSUBSTYLE(int subStyle)
% For a sub style, return the base style, else return the argument.
%
% SCI_GETPRIMARYSTYLEFROMSTYLE(int style)
% For a secondary style, return the primary style, else return the argument.
%
% SCI_SETIDENTIFIERS(int style, const char8 *identifiers)
% Similar to SCI_SETKEYWORDS but for substyles. The prefix feature available with SCI_SETKEYWORDS is not implemented for
% SCI_SETIDENTIFIERS.
%
% Lexer Objects
%
% Lexers are programmed as objects that implement the ILexer interface and that interact with the document they are
% lexing through the IDocument interface. Previously lexers were defined by providing lexing and folding functions but
% creating an object to handle the interaction of a lexer with a document allows the lexer to store state information that
% can be used during lexing. For example a C++ lexer may store a set of preprocessor definitions or variable declarations
% and style these depending on their role.
%
% A set of helper classes allows older lexers defined by functions to be used in Scintilla.
%
% ILexer
%
% class ILexer {
% public:
% virtual int SCI_METHOD Version() const = 0;
% virtual void SCI_METHOD Release() = 0;
% virtual const char8 * SCI_METHOD PropertyNames() = 0;
% virtual int SCI_METHOD PropertyType(const char8 *name) = 0;
% virtual const char8 * SCI_METHOD DescribeProperty(const char8 *name) = 0;
% virtual Sci_Position SCI_METHOD PropertySet(const char8 *key, const char8 *val) = 0;
% virtual const char8 * SCI_METHOD DescribeWordListSets() = 0;
% virtual Sci_Position SCI_METHOD WordListSet(int n, const char8 *wl) = 0;
% virtual void SCI_METHOD Lex(Sci_PositionU startPos, Sci_Position lengthDoc, int initStyle, IDocument *pAccess) = 0;
% virtual void SCI_METHOD Fold(Sci_PositionU startPos, Sci_Position lengthDoc, int initStyle, IDocument *pAccess) = 0;
% virtual void * SCI_METHOD PrivateCall(int operation, void *pointer) = 0;
% };
%
% The types Sci_Position and Sci_PositionU are used for positions and line numbers in the document. Before release 3.6.0
% the types int and unsigned int were used instead and, for 3.6.0, Sci_Position is defined as int and Sci_PositionU is
% defined as unsigned int. In a future release, 64-bit builds will define these as 64-bit types to allow documents larger
% than 2 GB.
%
% The return values from PropertySet and WordListSet are used to indicate whether the change requires performing lexing
% or folding over any of the document. It is the position at which to restart lexing and folding or -1 if the change does
% not require any extra work on the document. A simple approach is to return 0 if there is any possibility that a change
% requires lexing the document again while an optimisation could be to remember where a setting first affects the document
% and return that position.
%
% Version returns an enumerated value specifying which version of the interface is implemented: lvOriginal for ILexer and
% lvSubStyles for ILexerWithSubStyles.
%
% Release is called to destroy the lexer object.
%
% PrivateCall allows for direct communication between the application and a lexer. An example would be where an
% application maintains a single large data structure containing symbolic information about system headers (like
% Windows.h) and provides this to the lexer where it can be applied to each document. This avoids the costs of
% constructing the system header information for each document. This is invoked with the SCI_PRIVATELEXERCALL API.
%
% Fold is called with the exact range that needs folding. Previously, lexers were called with a range that started one
% line before the range that needs to be folded as this allowed fixing up the last line from the previous folding. The new
% approach allows the lexer to decide whether to backtrack or to handle this more efficiently.
%
% ILexerWithSubStyles
%
% To allow lexers to report which line ends they support, and to support substyles, Ilexer is extended to
% ILexerWithSubStyles.
%
% class ILexerWithSubStyles : public ILexer {
% public:
% virtual int SCI_METHOD LineEndTypesSupported() = 0;
% virtual int SCI_METHOD AllocateSubStyles(int styleBase, int numberStyles) = 0;
% virtual int SCI_METHOD SubStylesStart(int styleBase) = 0;
% virtual int SCI_METHOD SubStylesLength(int styleBase) = 0;
% virtual int SCI_METHOD StyleFromSubStyle(int subStyle) = 0;
% virtual int SCI_METHOD PrimaryStyleFromStyle(int style) = 0;
% virtual void SCI_METHOD FreeSubStyles() = 0;
% virtual void SCI_METHOD SetIdentifiers(int style, const char8 *identifiers) = 0;
% virtual int SCI_METHOD DistanceToSecondaryStyles() = 0;
% virtual const char8 * SCI_METHOD GetSubStyleBases() = 0;
% };
%
% IDocument
%
% class IDocument {
% public:
% virtual int SCI_METHOD Version() const = 0;
% virtual void SCI_METHOD SetErrorStatus(int status) = 0;
% virtual Sci_Position SCI_METHOD Length() const = 0;
% virtual void SCI_METHOD GetCharRange(char *buffer, Sci_Position position, Sci_Position lengthRetrieve) const = 0;
% virtual char8 SCI_METHOD StyleAt(Sci_Position position) const = 0;
% virtual Sci_Position SCI_METHOD LineFromPosition(Sci_Position position) const = 0;
% virtual Sci_Position SCI_METHOD LineStart(Sci_Position line) const = 0;
% virtual int SCI_METHOD GetLevel(Sci_Position line) const = 0;
% virtual int SCI_METHOD SetLevel(Sci_Position line, int level) = 0;
% virtual int SCI_METHOD GetLineState(Sci_Position line) const = 0;
% virtual int SCI_METHOD SetLineState(Sci_Position line, int state) = 0;
% virtual void SCI_METHOD StartStyling(Sci_Position position, char8 mask) = 0;
% virtual bool SCI_METHOD SetStyleFor(Sci_Position length, char8 style) = 0;
% virtual bool SCI_METHOD SetStyles(Sci_Position length, const char8 *styles) = 0;
% virtual void SCI_METHOD DecorationSetCurrentIndicator(int indicator) = 0;
% virtual void SCI_METHOD DecorationFillRange(Sci_Position position, int value, Sci_Position fillLength) = 0;
% virtual void SCI_METHOD ChangeLexerState(Sci_Position start, Sci_Position end) = 0;
% virtual int SCI_METHOD CodePage() const = 0;
% virtual bool SCI_METHOD IsDBCSLeadByte(char ch) const = 0;
% virtual const char8 * SCI_METHOD BufferPointer() = 0;
% virtual int SCI_METHOD GetLineIndentation(Sci_Position line) = 0;
% };
%
% Scintilla tries to minimize the consequences of modifying text to only relex and redraw the line of the change where
% possible. Lexer objects contain their own private extra state which can affect later lines. For example, if the C++
% lexer is greying out inactive code segments then changing the statement #define BEOS 0 to #define BEOS 1 may require
% restyling and redisplaying later parts of the document. The lexer can call ChangeLexerState to signal to the document
% that it should relex and display more.
%
% For StartStyling the mask argument has no effect. It was used in version 3.4.2 and earlier.
%
% SetErrorStatus is used to notify the document of exceptions. Exceptions should not be thrown over build boundaries as
% the two sides may be built with different compilers or incompatible exception options.
%
% IDocumentWithLineEnd
%
% To allow lexers to determine the end position of a line and thus more easily support Unicode line ends IDocument is
% extended to IDocumentWithLineEnd.
%
% GetRelativePosition navigates the document by whole characters, returning INVALID_POSITION for movement beyond the
% start and end of the document.
%
% GetCharacterAndWidth provides a standard conversion from UTF-8 bytes to a UTF-32 character or from DBCS to a 16 bit
% value. Bytes in invalid UTF-8 are reported individually with values 0xDC80+byteValue, which are not valid Unicode code
% points. The pWidth argument can be NULL if the caller does not need to know the number of bytes in the character.
%
% class IDocumentWithLineEnd : public IDocument {
% public:
% virtual Sci_Position SCI_METHOD LineEnd(Sci_Position line) const = 0;
% virtual Sci_Position SCI_METHOD GetRelativePosition(Sci_Position positionStart, Sci_Position characterOffset) const =
% 0;
% virtual int SCI_METHOD GetCharacterAndWidth(Sci_Position position, Sci_Position *pWidth) const = 0;
% };
%
% The ILexer, ILexerWithSubStyles, IDocument, and IDocumentWithLineEnd interfaces may be expanded in the future with
% extended versions (ILexer2...). The Version method indicates which interface is implemented and thus which methods may
% be called.
%
% Notifications
%
% Notifications are sent (fired) from the Scintilla control to its container when an event has occurred that may interest
% the container.
%
% Notifications are sent using the WM_NOTIFY message on Windows.
%
% On GTK+, the "sci-notify" signal is sent and the signal handler should have the signature handler(GtkWidget *, gint,
% SCNotification *notification, gpointer userData).
%
% On Cocoa, a delegate implementing the ScintillaNotificationProtocol may be set to receive notifications or the
% ScintillaView class may be subclassed and the notification: method overridden. Overriding notification: allows the
% subclass to control whether default handling is performed.
%
% The container is passed a SCNotification structure containing information about the event.
% struct Sci_NotifyHeader { // This matches the Win32 NMHDR structure
% void *hwndFrom; // environment specific window handle/pointer
% uptr_t idFrom; // CtrlID of the window issuing the notification
% unsigned int code; // The SCN_* notification code
% };
%
% struct SCNotification {
% struct Sci_NotifyHeader nmhdr;
% Sci_Position position;
% /* SCN_STYLENEEDED, SCN_DOUBLECLICK, SCN_MODIFIED, SCN_MARGINCLICK, */
% /* SCN_NEEDSHOWN, SCN_DWELLSTART, SCN_DWELLEND, SCN_CALLTIPCLICK, */
% /* SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, SCN_HOTSPOTRELEASECLICK, */
% /* SCN_INDICATORCLICK, SCN_INDICATORRELEASE, */
% /* SCN_USERLISTSELECTION, SCN_AUTOCSELECTION */
%
% int ch;
% /* SCN_CHARADDED, SCN_KEY, SCN_AUTOCCOMPLETE, SCN_AUTOCSELECTION, */
% /* SCN_USERLISTSELECTION */
% int modifiers;
% /* SCN_KEY, SCN_DOUBLECLICK, SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, */
% /* SCN_HOTSPOTRELEASECLICK, SCN_INDICATORCLICK, SCN_INDICATORRELEASE, */
%
% int modificationType; /* SCN_MODIFIED */
% const char8 *text;
% /* SCN_MODIFIED, SCN_USERLISTSELECTION, SCN_AUTOCSELECTION, SCN_URIDROPPED */
%
% Sci_Position length; /* SCN_MODIFIED */
% Sci_Position linesAdded; /* SCN_MODIFIED */
% int message; /* SCN_MACRORECORD */
% uptr_t wParam; /* SCN_MACRORECORD */
% sptr_t lParam; /* SCN_MACRORECORD */
% Sci_Position line; /* SCN_MODIFIED */
% int foldLevelNow; /* SCN_MODIFIED */
% int foldLevelPrev; /* SCN_MODIFIED */
% int margin; /* SCN_MARGINCLICK */
% int listType; /* SCN_USERLISTSELECTION */
% int x; /* SCN_DWELLSTART, SCN_DWELLEND */
% int y; /* SCN_DWELLSTART, SCN_DWELLEND */
% int token; /* SCN_MODIFIED with SC_MOD_CONTAINER */
% int annotationLinesAdded; /* SCN_MODIFIED with SC_MOD_CHANGEANNOTATION */
% int updated; /* SCN_UPDATEUI */
% int listCompletionMethod;
% /* SCN_AUTOCSELECTION, SCN_AUTOCCOMPLETED, SCN_USERLISTSELECTION */
%
% };
%
% The notification messages that your container can choose to handle and the messages associated with them are:
% SCN_STYLENEEDED
% SCN_CHARADDED
% SCN_SAVEPOINTREACHED
% SCN_SAVEPOINTLEFT
% SCN_MODIFYATTEMPTRO
% SCN_KEY
% SCN_DOUBLECLICK
% SCN_UPDATEUI
% SCN_MODIFIED
% SCN_MACRORECORD
% SCN_MARGINCLICK
% SCN_NEEDSHOWN
% SCN_PAINTED
% SCN_USERLISTSELECTION
% SCN_URIDROPPED
% SCN_DWELLSTART
% SCN_DWELLEND
% SCN_ZOOM
% SCN_HOTSPOTCLICK
% SCN_HOTSPOTDOUBLECLICK
% SCN_HOTSPOTRELEASECLICK
% SCN_INDICATORCLICK
% SCN_INDICATORRELEASE
% SCN_CALLTIPCLICK
% SCN_AUTOCSELECTION
% SCN_AUTOCCANCELLED
% SCN_AUTOCCHARDELETED
% SCN_FOCUSIN
% SCN_FOCUSOUT
% SCN_AUTOCCOMPLETED
%
% The following SCI_* messages are associated with these notifications:
% SCI_SETMODEVENTMASK(int eventMask)
% SCI_GETMODEVENTMASK
% SCI_SETMOUSEDWELLTIME(int milliseconds)
% SCI_GETMOUSEDWELLTIME
% SCI_SETIDENTIFIER(int identifier)
% SCI_GETIDENTIFIER
%
% The following additional notifications are sent using a secondary "command" method and should be avoided in new code as
% the primary "notification" method provides all the same events with richer information. The WM_COMMAND message is used
% on Windows. This emulates the Windows Edit control. Only the lower 16 bits of the control's ID is passed in these
% notifications.
%
% On GTK+, the "command" signal is sent and the signal handler should have the signature handler(GtkWidget *, gint
% wParam, gpointer lParam, gpointer userData).
% SCEN_CHANGE
% SCEN_SETFOCUS
% SCEN_KILLFOCUS
%
% SCI_GETIDENTIFIER
% These two messages set and get the identifier of the Scintilla instance which is included in notifications as the
% idFrom field. When an application creates multiple Scintilla widgets, this allows the source of each notification to be
% found. On Windows, this value is initialised in the CreateWindow call and stored as the GWLP_ID attribute of the window.
% The value should be small, preferably less than 16 bits, rather than a pointer as some of the functions will only
% transmit 16 or 32 bits.
%
properties
    styleNeededEvent : event2{integer StartPos, integer EndPos}.
    % SCN_STYLENEEDED
    % If you used SCI_SETLEXER(SCLEX_CONTAINER) to make the container act as the lexer, you will receive this notification
    % when Scintilla is about to display or print text that requires styling. You are required to style the text from the line
    % that contains the position returned by SCI_GETENDSTYLED up to the position passed in SCNotification.position.
    % Symbolically, you need code of the form:
    % startPos = SCI_GETENDSTYLED()
    % lineNumber = SCI_LINEFROMPOSITION(startPos);
    % startPos = SCI_POSITIONFROMLINE(lineNumber);
    % MyStyleRoutine(startPos, SCNotification.position);
    %

properties
    charAddedEvent : event1{integer Char}.
    % SCN_CHARADDED
    % This is sent when the user types an ordinary text character (as opposed to a command character) that is entered into
    % the text. The container can use this to decide to display a call tip or an auto completion list. The character is in
    % SCNotification.ch. This notification is sent before the character has been styled so processing that depends on styling
    % should instead be performed in the SCN_UPDATEUI notification.
    %

domains
    savepointAction = savepointReached; savepointLeft.

properties
    savepointEvent : event1{savepointAction SavepointAction}.
    % Sent to the container when the save point is entered or left, allowing the container to display a "document dirty"
    % indicator and change its menus.
    % See also: SCI_SETSAVEPOINT, SCI_GETMODIFY
    %

properties
    modifyAttemptROEvent : event0.
    % SCN_MODIFYATTEMPTRO
    % When in read-only mode, this notification is sent to the container if the user tries to change the text. This can be
    % used to check the document out of a version control system. You can set the read-only state of a document with
    % SCI_SETREADONLY.
    %

properties
    doubleClickEvent : event3{integer Position, lineNumber Line, integer Modifiers}.
    % SCN_DOUBLECLICK
    % The mouse button was double clicked in editor. The position field is set to the text position of the double click, the
    % line field is set to the line of the double click, and the modifiers field is set to the key modifiers held down in a
    % similar manner to SCN_KEY.
    %

properties
    updateUIEvent : event1{unsigned UpdatedFlag} (o).
    % SCN_UPDATEUI
    % Either the text or styling of the document has changed or the selection range or scroll position has changed. Now would
    % be a good time to update any container UI elements that depend on document or view state. The updated field is set to
    % the bit set of things changed since the previous notification.
    %
    % Symbol Value Meaning
    %
    % SC_UPDATE_CONTENT 0x01 Contents, styling or markers have been changed.
    % SC_UPDATE_SELECTION 0x02 Selection has been changed.
    % SC_UPDATE_V_SCROLL 0x04 Scrolled vertically.
    % SC_UPDATE_H_SCROLL 0x08 Scrolled horizontally.
    %

domains
    modifyAction =
        insertText(integer Position, string8 Text8, integer Length, integer LinesAdded, boolean IsTyped);
        deleteText(integer Position, string8 Text8, integer Length, integer LinesAdded);
        changeStyle(integer Position, integer Length);
        changeFold(lineNumber Line, integer FoldLevelNow, integer FoldLevelPrev);
        changeMarker(lineNumber Line);
        beforeInsert(integer Position, string8 Text8, integer Length);
        beforeDelete(integer Position, integer Length);
        changeIndicator(integer Position, integer Length);
        changeLineState(lineNumber Line);
        lexerState(integer Position, integer Length);
        changeMargin(lineNumber Line);
        changeAnnotation(lineNumber Line);
        container(integer Token).

domains
    modifyPerformed = performedNone; performedUser; performedUndo; performedRedo.

properties
    modifiedEvent :
        event5{modifyAction Action, modifyPerformed Performed, boolean MultiStepUndoRedo, boolean LastStepInUndoRedo, boolean StartAction} (o).
    % This notification is sent when the text or styling of the document changes or is about to change. You can set a mask
    % for the notifications that are sent to the container with SCI_SETMODEVENTMASK. The notification structure contains
    % information about what changed, how the change occurred and whether this changed the number of lines in the document. No
    % modifications may be performed while in a SCN_MODIFIED event. The SCNotification fields used are:
    %
    % Field Usage
    %
    % modificationType A set of flags that identify the change(s) made. See the next table.
    % position Start position of a text or styling change. Set to 0 if not used.
    % length Length of the change in bytes when the text or styling changes. Set to 0 if not used.
    % linesAdded Number of added lines. If negative, the number of deleted lines. Set to 0 if not used or no lines added or
    % deleted.
    % text Valid for text changes, not for style changes. If we are collecting undo information this holds a pointer to the
    % text that is handed to the Undo system, otherwise it is zero. For user performed SC_MOD_BEFOREDELETE the text field is
    % 0.
    % line The line number at which a fold level or marker change occurred. This is 0 if unused and may be -1 if more than
    % one line changed.
    % foldLevelNow The new fold level applied to the line or 0 if this field is unused.
    % foldLevelPrev The previous folding level of the line or 0 if this field is unused.
    %
    % The SCNotification.modificationType field has bits set to tell you what has been done. The SC_MOD_* bits correspond to
    % actions. The SC_PERFORMED_* bits tell you if the action was done by the user, or the result of Undo or Redo of a
    % previous action.
    %
    % Symbol Value Meaning
    %
    % SCNotification fields
    %
    % SC_MOD_INSERTTEXT 0x01 Text has been inserted into the document. position, length, text, linesAdded
    % SC_MOD_DELETETEXT 0x02 Text has been removed from the document. position, length, text, linesAdded
    % SC_MOD_CHANGESTYLE 0x04 A style change has occurred. position, length
    % SC_MOD_CHANGEFOLD 0x08 A folding change has occurred. line, foldLevelNow, foldLevelPrev
    % SC_PERFORMED_USER 0x10 Information: the operation was done by the user. None
    % SC_PERFORMED_UNDO 0x20 Information: this was the result of an Undo. None
    % SC_PERFORMED_REDO 0x40 Information: this was the result of a Redo. None
    % SC_MULTISTEPUNDOREDO 0x80 This is part of a multi-step Undo or Redo transaction. None
    % SC_LASTSTEPINUNDOREDO 0x100 This is the final step in an Undo or Redo transaction. None
    % SC_MOD_CHANGEMARKER 0x200 One or more markers has changed in a line. line
    % SC_MOD_BEFOREINSERT 0x400 Text is about to be inserted into the document. position, if performed by user then text in
    % bytes, length in bytes
    % SC_MOD_BEFOREDELETE 0x800 Text is about to be deleted from the document. position, length
    % SC_MOD_CHANGEINDICATOR 0x4000 An indicator has been added or removed from a range of text. position, length
    % SC_MOD_CHANGELINESTATE 0x8000 A line state has changed because SCI_SETLINESTATE was called. line
    % SC_MOD_CHANGETABSTOPS 0x200000 The explicit tab stops on a line have changed because SCI_CLEARTABSTOPS or
    % SCI_ADDTABSTOP was called. line
    % SC_MOD_LEXERSTATE 0x80000 The internal state of a lexer has changed over a range. position, length
    % SC_MOD_CHANGEMARGIN 0x10000 A text margin has changed. line
    % SC_MOD_CHANGEANNOTATION 0x20000 An annotation has changed. line
    % SC_MOD_INSERTCHECK 0x100000 Text is about to be inserted. The handler may change the text being inserted by calling
    % SCI_CHANGEINSERTION. No other modifications may be made in this handler. position, length, text
    % SC_MULTILINEUNDOREDO 0x1000 This is part of an Undo or Redo with multi-line changes. None
    % SC_STARTACTION 0x2000 This is set on a SC_PERFORMED_USER action when it is the first or only step in an undo
    % transaction. This can be used to integrate the Scintilla undo stack with an undo stack in the container application by
    % adding a Scintilla action to the container's stack for the currently opened container transaction or to open a new
    % container transaction if there is no open container transaction. None
    % SC_MOD_CONTAINER 0x40000 This is set on for actions that the container stored into the undo stack with
    % SCI_ADDUNDOACTION. token
    % SC_MODEVENTMASKALL 0x1FFFFF This is a mask for all valid flags. This is the default mask state set by
    % SCI_SETMODEVENTMASK. None
    %
    % SCEN_CHANGE
    % SCEN_CHANGE (768) is fired when the text (not the style) of the document changes. This notification is sent using the
    % WM_COMMAND message on Windows and the "command" signal on GTK+ as this is the behaviour of the standard Edit control
    % (SCEN_CHANGE has the same value as the Windows Edit control EN_CHANGE). No other information is sent. If you need more
    % detailed information use SCN_MODIFIED. You can filter the types of changes you are notified about with
    % SCI_SETMODEVENTMASK.
    %

properties
    modEventMask : integer.
    % These messages set and get an event mask that determines which document change events are notified to the container
    % with SCN_MODIFIED and SCEN_CHANGE. For example, a container may decide to see only notifications about changes to text
    % and not styling changes by calling SCI_SETMODEVENTMASK(SC_MOD_INSERTTEXT|SC_MOD_DELETETEXT).
    %
    % The possible notification types are the same as the modificationType bit flags used by SCN_MODIFIED: SC_MOD_INSERTTEXT,
    % SC_MOD_DELETETEXT, SC_MOD_CHANGESTYLE, SC_MOD_CHANGEFOLD, SC_PERFORMED_USER, SC_PERFORMED_UNDO, SC_PERFORMED_REDO,
    % SC_MULTISTEPUNDOREDO, SC_LASTSTEPINUNDOREDO, SC_MOD_CHANGEMARKER, SC_MOD_BEFOREINSERT, SC_MOD_BEFOREDELETE,
    % SC_MULTILINEUNDOREDO, and SC_MODEVENTMASKALL.
    %

properties
    setFocusEvent : event0 (o).
    killFocusEvent : event0 (o).
    % SCEN_SETFOCUS
    % SCEN_KILLFOCUS
    % SCEN_SETFOCUS (512) is fired when Scintilla receives focus and SCEN_KILLFOCUS (256) when it loses focus. These
    % notifications are sent using the WM_COMMAND message on Windows and the "command" signal on GTK+ as this is the behaviour
    % of the standard Edit control. Unfortunately, these codes do not match the Windows Edit notification codes EN_SETFOCUS
    % (256) and EN_KILLFOCUS (512). It is now too late to change the Scintilla codes as clients depend on the current values.
    %

properties
    macroRecordEvent : event3{unsigned Message, gui_native::wParam WParam, gui_native::lParam LParam} (o).
    % SCN_MACRORECORD
    % The SCI_STARTRECORD and SCI_STOPRECORD messages enable and disable macro recording. When enabled, each time a
    % recordable change occurs, the SCN_MACRORECORD notification is sent to the container. It is up to the container to record
    % the action. To see the complete list of SCI_* messages that are recordable, search the Scintilla source Editor.cxx for
    % Editor::NotifyMacroRecord. The fields of SCNotification set in this notification are:
    %
    % Field Usage
    %
    % message The SCI_* message that caused the notification.
    % wParam The value of wParam in the SCI_* message.
    % lParam The value of lParam in the SCI_* message.
    %

properties
    marginClickEvent : event3{integer Position, integer Modifiers, integer Margin} (o).
    % SCN_MARGINCLICK
    % This notification tells the container that the mouse was clicked inside a margin that was marked as sensitive (see
    % SCI_SETMARGINSENSITIVEN). This can be used to perform folding or to place breakpoints. The following SCNotification
    % fields are used:
    %
    % Field Usage
    %
    % modifiers The appropriate combination of SCI_SHIFT, SCI_CTRL and SCI_ALT to indicate the keys that were held down at
    % the time of the margin click.
    % position The position of the start of the line in the document that corresponds to the margin click.
    % margin The margin number that was clicked.
    %

properties
    needShownEvent : event2{integer Position, integer Length} (o).
    % SCN_NEEDSHOWN
    % Scintilla has determined that a range of lines that is currently invisible should be made visible. An example of where
    % this may be needed is if the end of line of a contracted fold point is deleted. This message is sent to the container in
    % case it wants to make the line visible in some unusual way such as making the whole document visible. Most containers
    % will just ensure each line in the range is visible by calling SCI_ENSUREVISIBLE. The position and length fields of
    % SCNotification indicate the range of the document that should be made visible. The container code will be similar to the
    % following code skeleton:
    % firstLine = SCI_LINEFROMPOSITION(scn.position)
    % lastLine = SCI_LINEFROMPOSITION(scn.position+scn.length-1)
    % for line = lineStart to lineEnd do SCI_ENSUREVISIBLE(line) next
    %

properties
    paintedEvent : event0 (o).
    % SCN_PAINTED
    % Painting has just been done. Useful when you want to update some other widgets based on a change in Scintilla, but want
    % to have the paint occur first to appear more responsive. There is no other information in SCNotification.
    %

properties
    userListSelectionEvent : event2{string8 Text8, gui_native::wParam WParam} (o).
    % SCN_USERLISTSELECTION
    % The user has selected an item in a user list. The SCNotification fields used are:
    %
    % Field Usage
    %
    % listType This is set to the listType parameter from the SCI_USERLISTSHOW message that initiated the list.
    % text The text of the selection.
    % position The position the list was displayed at.
    % ch If a fillup character was the method of selection, the used character, otherwise 0.
    % listCompletionMethod A value indicating the way in which the completion occurred. See the table below.
    %
    % See the SCN_AUTOCCOMPLETED notification for the possible values for listCompletionMethod.
    % SCN_URIDROPPED
    % Only on the GTK+ version. Indicates that the user has dragged a URI such as a file name or Web address onto Scintilla.
    % The container could interpret this as a request to open the file. The text field of SCNotification points at the URI
    % text.

domains
    dwellAction = dwellStart; dwellEnd.

properties
    dwellEvent : event4{dwellAction DwellAction, integer Position, integer X, integer Y} (o).
    % SCN_DWELLSTART is generated when the user keeps the mouse in one position for the dwell period (see
    % SCI_SETMOUSEDWELLTIME). SCN_DWELLEND is generated after a SCN_DWELLSTART and the mouse is moved or other activity such
    % as key press indicates the dwell is over. Both notifications set the same fields in SCNotification:
    %
    % Field Usage
    %
    % position This is the nearest position in the document to the position where the mouse pointer was lingering.
    % x, y Where the pointer lingered. The position field is set to SCI_POSITIONFROMPOINTCLOSE(x, y).
    %

properties
    mouseDwellTime : integer.
    % These two messages set and get the time the mouse must sit still, in milliseconds, to generate a SCN_DWELLSTART
    % notification. If set to SC_TIME_FOREVER, the default, no dwell events are generated.
    %

properties
    zoomEvent : event0 (o).
    % SCN_ZOOM
    % This notification is generated when the user zooms the display using the keyboard or the SCI_SETZOOM method is called.
    % This notification can be used to recalculate positions, such as the width of the line number margin to maintain sizes in
    % terms of characters rather than pixels. SCNotification has no additional information.
    %

domains
    hotspotAction =
        hotspotClick(integer Position, integer Modifier);
        hotspotDoubleClick(integer Position, integer Modifier);
        hotspotReleaseClick(integer Modifier).

properties
    hotspotActionEvent : event1{hotspotAction HotspotAction} (o).
    % SCN_HOTSPOTCLICK
    % SCN_HOTSPOTDOUBLECLICK
    % SCN_HOTSPOTRELEASECLICK
    % These notifications are generated when the user clicks or double clicks on text that is in a style with the hotspot
    % attribute set. This notification can be used to link to variable definitions or web pages. In the notification handler,
    % you should avoid calling any function that modifies the current selection or caret position. The position field is set
    % the text position of the click or double click and the modifiers field set to the key modifiers held down in a similar
    % manner to SCN_KEY. Only the state of the Ctrl key is reported for SCN_HOTSPOTRELEASECLICK.
    %

domains
    indicatorAction =
        indicatorClick(integer Position, integer Modifier);
        indicatorRelease(integer Modifier).

properties
    indicatorActionEvent : event1{indicatorAction IndicatorAction} (o).
    % SCN_INDICATORCLICK
    % SCN_INDICATORRELEASE
    % These notifications are generated when the user clicks or releases the mouse on text that has an indicator. The
    % position field is set the text position of the click or double click and the modifiers field set to the key modifiers
    % held down in a similar manner to SCN_KEY.
    %

properties
    callTipClickEvent : event1{integer Arrow} (o).
    % SCN_CALLTIPCLICK
    % This notification is generated when the user clicks on a calltip. This notification can be used to display the next
    % function prototype when a function name is overloaded with different arguments. The position field is set to 1 if the
    % click is in an up arrow, 2 if in a down arrow, and 0 if elsewhere.
    %

domains
    autoCAction =
        autoCSelection(gui_native::lParam LParam);
        autoCCancelled;
        autoCCharDeleted.

properties
    autoCEvent : event1{autoCAction AutoCAction} (o).
    % SCN_AUTOCSELECTION
    % The user has selected an item in an autocompletion list. The notification is sent before the selection is inserted.
    % Automatic insertion can be cancelled by sending a SCI_AUTOCCANCEL message before returning from the notification. The
    % SCNotification fields used are:
    %
    % Field Usage
    %
    % position The start position of the word being completed.
    % text The text of the selection.
    % ch If a fillup character was the method of selection, the used character, otherwise 0.
    % listCompletionMethod A value indicating the way in which the completion occurred. See the table below.
    %
    % Symbol Value Meaning
    %
    % SC_AC_FILLUP 0x01 A fillup character triggered the completion. The character used is in ch.
    % SC_AC_DOUBLECLICK 0x02 A double-click triggered the completion. ch is 0.
    % SC_AC_TAB 0x04 The tab key or SCI_TAB triggered the completion. ch is 0.
    % SC_AC_NEWLINE 0x08 A new line or SCI_NEWLINE triggered the completion. ch is 0.
    % SC_AC_COMMAND 0x10 The SCI_AUTOCSELECT message triggered the completion. ch is 0.
    %
    % SCN_AUTOCCANCELLED
    % The user has cancelled an autocompletion list. There is no other information in SCNotification.
    %
    % SCN_AUTOCCHARDELETED
    % The user deleted a character while autocompletion list was active. There is no other information in SCNotification.
    %
    % SCN_FOCUSIN
    % SCN_FOCUSOUT
    % SCN_FOCUSIN (2028) is fired when Scintilla receives focus and SCN_FOCUSOUT (2029) when it loses focus.
    %
    % SCN_AUTOCCOMPLETED
    % This notification is generated after an autocompletion has inserted its text. The fields are identical to the
    % SCN_AUTOCSELECTION notification.
    %

% Images
%
% Two formats are supported for images used in margin markers and autocompletion lists, RGBA and XPM.
%
% RGBA
%
% The RGBA format allows translucency with an alpha value for each pixel. It is simpler than XPM and more capable.
%
% The data is a sequence of 4 byte pixel values starting with the pixels for the top line, with the leftmost pixel first,
% then continuing with the pixels for subsequent lines. There is no gap between lines for alignment reasons.
%
% Each pixel consists of, in order, a red byte, a green byte, a blue byte and an alpha byte. The colour bytes are not
% premultiplied by the alpha value. That is, a fully red pixel that is 25% opaque will be [FF, 00, 00, 3F]
%
% Since the RGBA pixel data does not include any size information the width and height must previously been set with the
% SCI_RGBAIMAGESETWIDTH and SCI_RGBAIMAGESETHEIGHT messages.
%
% GUI platforms often include functions for reading image file formats like PNG into memory in the RGBA form or a similar
% form. If there is no suitable platform support, the LodePNG and picoPNG libraries are small libraries for loading and
% decoding PNG files available under a BSD-style license.
%
% RGBA format is supported on Windows, GTK+ and OS X Cocoa.
%
% XPM
%
% The XPM format is described here. Scintilla is only able to handle XPM pixmaps that use one character per pixel with no
% named colours. There may be a completely transparent colour named "None".
%
% There are two forms of data structure used for XPM images, the first "lines form" format is well suited to embedding an
% image inside C source code and the "text form" is suited to reading from a file. In the lines form, an array of strings
% is used with the first string indicating the dimensions and number of colours used. This is followed by a string for
% each colour and that section is followed by the image with one string per line. The text form contains the same data as
% one null terminated block formatted as C source code starting with a "/* XPM */" comment to mark the format.
%
% Either format may be used with Scintilla APIs with the bytes at the location pointed to examined to determine which
% format: if the bytes start with "/* XPM */" then it is treated as text form, otherwise it is treated as lines form.
%
% XPM format is supported on on all platforms.
%
% GTK+
%
% On GTK+, the following functions create a Scintilla widget, communicate with it and allow resources to be released
% after all Scintilla widgets have been destroyed.
% GtkWidget *scintilla_new()
% void scintilla_set_id(ScintillaObject *sci, uptr_t id)
% sptr_t scintilla_send_message(ScintillaObject *sci,unsigned int iMessage, uptr_t wParam, sptr_t lParam)
% void scintilla_release_resources()
%
% GtkWidget *scintilla_new()
% Create a new Scintilla widget. The returned pointer can be added to a container and displayed in the same way as other
% widgets.
%
% void scintilla_set_id(ScintillaObject *sci, uptr_t id)
% Set the control ID which will be used in the idFrom field of the NotifyHeader structure of all notifications for this
% instance. This is equivalent to SCI_SETIDENTIFIER.
%
% sptr_t scintilla_send_message(ScintillaObject *sci,unsigned int iMessage, uptr_t wParam, sptr_t lParam)
% The main entry point allows sending any of the messages described in this document.
%
% void scintilla_release_resources()
% Call this to free any remaining resources after all the Scintilla widgets have been destroyed.
%
% Provisional messages
%
% Complex new features may be added as 'provisional' to allow further changes to the API. Provisional features may even
% be removed if experience shows they are a mistake.
%
% Provisional features are displayed in this document with a distinctive background colour.
%
% There are currently no provisional messages. The SC_TECHNOLOGY_DIRECTWRITERETAIN and SC_TECHNOLOGY_DIRECTWRITEDC values
% for SCI_SETTECHNOLOGY are provisional.
%
% Using C++11 <regex> is provisional.
%
% Some developers may want to only use features that are stable and have graduated from provisional status. To avoid
% using provisional messages compile with the symbol SCI_DISABLE_PROVISIONAL defined.
%
/***************************************************************************************************************

predicates
    addStyledText : (string StyledText).
    % @short
    % This behaves like addText, but inserts styled text.  In styled text each second character is a style.
    % @end

predicates
    getCharAt : (integer Pos) -> char8 Char.
    % SCI_GETCHARAT(int Pos)
    % This returns the character at pos in the document or 0 if pos is negative or past the end of the document.

predicates
    targetAsUtf8 : (string8 S).
    % SCI_TARGETASUTF8(<unused>, char *s)
    % This method retrieves the value of the target encoded as UTF-8 which is the default encoding of GTK+ so is useful for retrieving text for
    % use in other parts of the user interface, such as find and replace dialogs.
    % The length of the encoded text in bytes is returned.

predicates
    encodedFromUtf8 : (string8 Utf8, string8 Encoded).
    % SCI_ENCODEDFROMUTF8(const char *utf8, char *encoded)

predicates
    setLengthforEncode : (integer Bytes).
    % SCI_ENCODEDFROMUTF8 converts a UTF-8 string into the document's encoding which is useful for taking the results of a find dialog, for
    % example, and receiving a string of bytes that can be searched for in the document.
    % Since the text can contain nul bytes, the SCI_SETLENGTHFORENCODE method can be used to set the length that will be converted.
    % If set to -1, the length is determined by finding a nul byte.
    % The length of the converted string is returned.
*/
%
%
%
predicates
    displayFindAndReplace_modal : () -> topLevelContainerWindow FindReplaceDialog.
    % @short Display the search dialog for the message control
    % @end

predicates
    displayIncrementalFind_modal : () -> sciLexerSupport\incrementalFindDialog Dialog.
    % @short Display the search dialog for the message control
    % @end

predicates
    displayGotoLine_modal : () -> boolean Went.
    % @short Display the dialog for going to a line.  #Went is false if the user cancelled the dialog.
    % @end

end interface sciLexerBase
