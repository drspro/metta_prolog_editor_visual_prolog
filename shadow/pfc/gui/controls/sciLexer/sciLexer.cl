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

class sciLexer : sciLexer

constructors
    new : ().

constructors
    new : (containerWindow Parent).

predicates
    displayFindAndReplace_modeless : (sciLexer Parent) -> topLevelContainerWindow SearchReplaceDialog.
    displayFindAndReplace_modeless : (sciLexer Parent, boolean AsDialog) -> topLevelContainerWindow SearchReplaceDialog.
    % @short Display the search/replace dialog for the editor control
    % @end

predicates
    findNext : ().
    findPrevious : ().
    % @short Perform find next/previous (in the last editor)
    % @end

predicates
    tryGetFindAndReplace_modeless : () -> topLevelContainerWindow SearchReplaceDialog determ.
    % @short Get the FindAndReplace modeles dialog if existing
    % @end

predicates
    getInVisitOrder_nd : () -> sciLexer SciLexer nondeterm.

predicates
    tryGetEditor : () -> sciLexer SciLexer determ.

properties
    fileExtension_visualProlog : string*.
    fileExtension_vipGrammar : string*.
    fileExtension_xml : string*.
    fileExtension_cpp : string*.
    fileExtension_html : string*.
    % @short
    % A list of extensions that will use the corresponding lexer when calling setLexer_extension.  The extensions must be in lower case.
    % @end

end class sciLexer
