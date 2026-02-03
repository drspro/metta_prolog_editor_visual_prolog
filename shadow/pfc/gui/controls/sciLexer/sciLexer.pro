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

implement sciLexer inherits commonControlSupport
    supports controlSupportSite
    open core, vpiDomains, gui_native, sciLexer_native

constants
    string8_defaultLength : byteCount = 1000.

resolve
    setFocus from commonControlSupport

clauses
    new() :-
        commonControlSupport::new(This, This, sciLexerWindowClass),
        usePopup := true.

clauses
    new(Container) :-
        new(),
        setContainer(Container).

facts
    native : scintillaObject := erroneous.

constants
    yCaretPolicy_default = sciLexer_native::caret_slop ++ sciLexer_native::caret_strict ++ sciLexer_native::caret_even.
    yCaretSlop_default = 5.

clauses
    show() :-
        HWnd = createWindowEx(),
        native := sciLexer_api::getScintillaObject(HWnd),
        scrollWidth := 500,
        scrollWidthTracking := true,
        codePage := utf8,
        setFont(getContainer():getFont()),
        setDefaultLexerStyle_postAction(),
        eolMode := sc_eol_lf,
% 13-12-2025        probeer de 2 kleuren anders te zetten
%        setSelBack(true, 0xF1CDA8),
%        setAdditionalSelBack(color_LightSalmon),

%        setSelBack( true , 0xF1CDA8 ) ,
        setSelBack( true ,0x727272 ) ,
%  0x7E7E7E

%       setAdditionalSelBack( color_LightSalmon ) ,
       setAdditionalSelBack( 0x616161 ) ,

        addNativeMessageHandler(onNative),
        charAddedEvent:addListener(onCharAdded),
        tabWidth := 4,
        tabIndents := true,
        backSpaceUnIndents := true,
        useTabs := false,
        pasteConvertEndings := true,
% 1-11-2025
%        caretFore := color_Gray40,
        caretFore := color_yellow,
        caretWidth := 2,
        setYCaretPolicy(yCaretPolicy_default, yCaretSlop_default),
        caretLineVisible := true,
        caretLineBack := color_Black,
% 31-10-2025
%        caretLineBackAlpha := 20,
%         caretLineBackAlpha := 0,

        %  You can choose to make the background colour of the line containing the caret different with
        % these messages. To do this, set the
        %  desired background colour with SCI_SETCARETLINEBACK,
        % then use SCI_SETCARETLINEVISIBLE(true) to enable the effect. You can
        %  cancel the effect with SCI_SETCARETLINEVISIBLE(false).
        % The two SCI_GETCARET* functions return the state and the colour. This form
        %  of background colouring has highest priority when a line has markers that would otherwise
        % change the background colour. The caret
        %  line may also be drawn translucently which allows other background colours to show through.
        % This is done by setTing the alpha
        %  (translucency) value by calling SCI_SETCARETLINEBACKALPHA.
        % When the alpha is not SC_ALPHA_NOALPHA, the caret line is drawn
        %  after all other features so will affect the colour of all other features.
% setstyle  STYLE_LINENUMBER
%     sc_margin_back = 2.
%    sc_margin_fore = 3.
% alles staat in program file Pfc \ windows_api \ scilexer_api
%     sc_max_margin = 4.
%    sc_margin_symbol = 0.
%    sc_margin_number = 1.
%    sc_margin_back = 2.
%    sc_margin_fore = 3.
%    sc_margin_text = 4.
%    sc_margin_rText = 5.
%    sci_setMarginTypeN = 2240.
%    sci_getMarginTypeN = 2241.

        setMarginTypeN(1, 1) ,
        setMarginWidthN(1, 50),

% 1-11-2025  Hiermee zet je een linkermargin in de tekst
% is nog onduidelijk wat het nu eigenlijk is
% want het is bedoeld voor symbols  volgens    de files
        setMarginTypeN(2, 2) ,        setMarginWidthN(2, 10),

%        setMarginTypeN(sc_margin_back, 0x000000 ) ,

%        setMarginTypeN(2, 0) ,        setMarginWidthN(2, 10),

        indent := 20,
%         marginSetStyle(Line, Style)
%           marginSetStyle(1,  0xC08080  ) ,
                 % doesnt work

        foreach K = std::fromTo(0x20, 0x60) do
            clearCmdKey(ctrl ++ K),
            clearCmdKey(ctrl ++ shift ++ K)
        end foreach,
        % popup is implemented here (to get HTML)
        sciLexer_api::usePopup(native, b_false),
        % handled in onNative to get old Visual Prolog behaviour
        clearCmdKey(key_tab),
        clearCmdKey(shift ++ key_tab),
        % select cut/copy/paste
        % handled in onNative (to get HTML)
        clearCmdKey(ctrl ++ key_C),
        clearCmdKey(ctrl ++ key_insert),
        clearCmdKey(ctrl ++ key_X),
        assignCmdKey(ctrl ++ key_V, sci_paste),
        % undo/redo
        assignCmdKey(ctrl ++ key_Z, sci_undo),
        assignCmdKey(ctrl ++ key_Y, sci_redo),
        % common
        assignCmdKey(ctrl ++ key_A, sci_selectAll),
        assignCmdKey(ctrl ++ shift ++ key_L, sci_lineDelete),
        assignCmdKey(ctrl ++ alt ++ key_R, sci_selectionDuplicate),
        assignCmdKey(ctrl ++ shift ++ key_T, sci_lineTranspose),
        assignCmdKey(alt ++ key_up, sci_moveSelectedLinesUp),
        assignCmdKey(alt ++ key_down, sci_moveSelectedLinesDown),
        assignCmdKey(ctrl ++ shift ++ key_left, sci_wordLeftExtend),
        assignCmdKey(ctrl ++ shift ++ key_right, sci_wordRightEndExtend),
        assignCmdKey(ctrl ++ shift ++ key_U, sci_upperCase),
        % Scintilla
        assignCmdKey(ctrl ++ key_U, sci_lowerCase),
        % Visual Prolog
        assignCmdKey(ctrl ++ key_L, sci_lowerCase),
        highlightSelectedWords := highlightSelectedWords_sameStyle,
        attachNativeCreate(HWnd).

constants
    logPixelsY = 90.

clauses
    setFont(Font) :-
        commonControlSupport::setFont(Font),
        logfont(Height, _Width, _Escapement, _Orientation, Weight, Italic, Underline, _StrikeOut, _CharSet, _OutPrecision, _ClipPrecision, _Quality,
                _PitchAndFamily, FaceName)
            = vpiDomains::fromFont_logfont(Font),
        ParentWin = getContainer():getVpiWindow(),
        HDC = gui_api::getDC(ParentWin),
        LogY = getDeviceCaps(HDC, logPixelsY),
        gui_api::releaseDC(ParentWin, HDC),
        PointSize = (-Height * 72 + 36) div LogY,
        IsItalic = toBoolean(b_false <> Italic),
        IsUnderline = toBoolean(b_false <> Underline),
        foreach Style = std::fromTo(0, style_max) do
            styleSetFont(Style, FaceName),
            styleSetSize(Style, PointSize),
            styleSetWeight(Style, Weight),
            styleSetItalic(Style, IsItalic),
            styleSetUnderline(Style, IsUnderline)
        end foreach.

clauses
    usePopup(false) :-
        resetContextMenuResponder().
    usePopup(true) :-
        setContextMenuResponder(onContextMenu).

constants
    id_edit_undo = 30000.
    id_edit_redo = id_edit_undo + 1.
    id_edit_copy = id_edit_undo + 2.
    id_edit_cut = id_edit_undo + 3.
    id_edit_paste = id_edit_undo + 4.

predicates
    onContextMenu : contextMenuResponder.
clauses
    onContextMenu(_, _) = contextMenuHandled :-
        Menu =
            dynMenu(
                [
                    txt(id_edit_undo, "Undo", noAccelerator(), boolean::toBooleanInt(canUndo), b_false, []),
                    txt(id_edit_redo, "Redo", noAccelerator(), boolean::toBooleanInt(canRedo), b_false, []),
                    separator,
                    txt(id_edit_cut, "Cut", noAccelerator(), boolean::toBooleanInt(canCut), b_false, []),
                    txt(id_edit_copy, "Copy", noAccelerator(), boolean::toBooleanInt(canCopy), b_false, []),
                    txt(id_edit_paste, "Paste", noAccelerator(), boolean::toBooleanInt(canPaste), b_false, [])
                ]),
        vpi::menuPopUp(getVpiWindow(), Menu, cursorGetPos(), align_Left).

predicates
    tryOnContextMenuItem : (menuTag Tag) determ.
clauses
    tryOnContextMenuItem(id_edit_copy) :-
        copy().
    tryOnContextMenuItem(id_edit_cut) :-
        cut().
    tryOnContextMenuItem(id_edit_paste) :-
        paste().
    tryOnContextMenuItem(id_edit_undo) :-
        undo().
    tryOnContextMenuItem(id_edit_redo) :-
        redo().

class facts
    visitOrder_fact : (sciLexer Editor).

predicates
    onNative : nativeMessageHandler.
clauses
    onNative(_Window, wm_getdlgcode, _WParam, _LParam) = nativeResult(gui_api::mkR(dlgc_wantallkeys)) :-
        !.

    onNative(_Window, wm_setfocus, _WParam, _LParam) = defaultNativeHandling :-
        !,
        retractAll(visitOrder_fact(This)),
        asserta(visitOrder_fact(This)).

    onNative(_Window, wm_destroy, _WParam, _LParam) = defaultNativeHandling :-
        !,
        retractAll(visitOrder_fact(This)).

    onNative(_, wm_keydown, WParam, _LParam) = defaultNativeHandling :-
        Action = tryGetKeydownAction(getKeyModifier(), gui_api::getInteger(WParam)),
        !,
        multipleSelection := false,
        postAction(Action).

    onNative(_, wm_command, WParam, _LParam) = nativeResult_null :-
        C = gui_api::getUnsigned(WParam),
        0 = 0xFFFF0000 ** C,
        tryOnContextMenuItem(uncheckedConvert(menuTag, C)),
        !.

    onNative(_Source, wm_mousemove, WParam, _LParam) = _ :-
        true = multipleSelection,
        bit::isSet(gui_api::getUnsigned(WParam), mk_lbutton),
        multipleSelection := false,
        fail.

    onNative(_Window, _Msg, _WParam, _LParam) = defaultNativeHandling.

class predicates
    getKeyModifier2 : (integer VirtKey, vpiDomains::keyModifier KeyModifier) -> vpiDomains::keyModifier Modifier.
clauses
    getKeyModifier2(VirtKey, M1) = if bit::isSet(uncheckedConvert(unsigned16, getKeyState(VirtKey)), keyShifted) then M1 else 0 end if.

class predicates
    getKeyModifier : () -> vpiDomains::keyModifier Modifier.
clauses
    getKeyModifier() = getKeyModifier2(vk_shift, c_Shift) ++ getKeyModifier2(vk_control, c_Control) ++ getKeyModifier2(vk_menu, c_Alt).

predicates
    tryGetKeydownAction : (vpiDomains::keyModifier Modifier, integer VKey) -> runnable Action determ.
clauses
    tryGetKeydownAction(c_Control, vk_Ckey) = copy.
    tryGetKeydownAction(c_Control, vk_insert) = copy.
    tryGetKeydownAction(c_Control, vk_Xkey) = cut.
    tryGetKeydownAction(c_Shift, vk_delete) = cut.
    tryGetKeydownAction(c_Nothing, vk_tab) = tab_vip.
    tryGetKeydownAction(c_Shift, vk_tab) = backTab_vip.

clauses
    tab_vip() :-
        changeIndent(true).

clauses
    backtab_vip() :-
        changeIndent(false).

predicates
    changeIndent : (boolean Forward).
clauses
    changeIndent(Forward) :-
        beginUndoAction(),
        try
            IndentStep = indentStep,
            IndentIncrease = if true = Forward then IndentStep else -IndentStep end if,
            LineStart = lineFromPosition(selectionStart),
            LineEnd1 = lineFromPosition(selectionEnd),
            LineEnd = if LineStart < LineEnd1 and selectionEnd = positionFromLine(LineEnd1) then LineEnd1 - 1 else LineEnd1 end if,
            foreach Line = std::fromTo(LineStart, LineEnd) do
                if Line = LineStart or b_false = sciLexer_api::getReadOnly(native) then
                    changeIndentLine(Line, IndentIncrease)
                end if
            end foreach,
            if currentPos = anchor and 1 = getColumn(currentPos) then
                gotoPos(getLineIndentPosition(LineStart))
            end if
        finally
            endUndoAction()
        end try.

predicates
    changeIndentLine : (lineNumber Line, integer IndentIncrease).
clauses
    changeIndentLine(Line, IndentIncrease) :-
        Indent1 = getLineIndentation(Line),
        Indent2 = Indent1 - Indent1 rem IndentIncrease,
        NewIndent = tryConvert(core::linePosition, Indent2 + IndentIncrease) otherwise 0,
        setLineIndentation(Line, NewIndent).

clauses
    indentStep() = E :-
        I = indent,
        E = if 0 = I then tabWidth else I end if.

clauses
    getInVisitOrder_nd() = SciLexer :-
        visitOrder_fact(SciLexer).

clauses
    tryGetEditor() = SciLexer :-
        visitOrder_fact(SciLexer),
        !.

facts
    maintainIndentation : boolean := true.

predicates
    onCharLF : ().
clauses
    onCharLF() :-
        if true = maintainIndentation and CurLine = lineFromPosition(currentPos) and CurLine >= 2 and Indent = getLineIndentation(CurLine - 1)
            and Indent > 0
        then
            setLineIndentation(CurLine, Indent),
            gotoPos(getLineIndentPosition(CurLine))
        end if.

constants
    charLF = 10.

predicates
    onCharAdded : (integer CharInt).
clauses
    onCharAdded(charLF) :-
        !,
        onCharLF().

    onCharAdded(_CharInt).

clauses
    extraLineSeparation() = extraAscent + extraDescent.

clauses
    extraLineSeparation(V) :-
        A = V div 2,
        extraAscent := A,
        extraDescent := V - A.

facts
    highlightSelectedWords_fact : integer := highlightSelectedWords_none.
    isHighlightSelectedWords : boolean := false.

clauses
    highlightSelectedWords() = highlightSelectedWords_fact.

clauses
    highlightSelectedWords(V) :-
        if highlightSelectedWords_none = highlightSelectedWords_fact and highlightSelectedWords_none <> V then
            updateUIEvent:addListener(onUpdateUI_highlightSelectedWords),
            onUpdateUI_highlightSelectedWords(sciLexer_native::sc_update_selection)
        elseif highlightSelectedWords_none <> highlightSelectedWords_fact and highlightSelectedWords_none = V then
            updateUIEvent:removeListener(onUpdateUI_highlightSelectedWords),
            multipleSelection := false,
            isHighlightSelectedWords := false
        end if,
        highlightSelectedWords_fact := V.

predicates
    onUpdateUI_highlightSelectedWords : (unsigned UpdatedFlag).
clauses
    onUpdateUI_highlightSelectedWords(UpdateFlags) :-
        if bit::isSet(UpdateFlags, sciLexer_native::sc_update_selection) then
            if Word = isWordSelected() then
                Style = getStyleAt(selectionStart),
                multipleSelection := true,
                isHighlightSelectedWords := true,
                additionalCaretsVisible := false,
                highlightSelectedWords(Style, Word)
            elseif true = isHighlightSelectedWords then
                isHighlightSelectedWords := false,
                multipleSelection := false
            end if
        end if.

predicates
    isWordSelected : () -> string Word determ.
clauses
    isWordSelected() = Word :-
        S = selectionStart,
        E = selectionEnd,
        S <> E,
        S = wordStartPosition(E),
        E = wordEndPosition(S),
        Word = selText,
        "_" <> Word.

predicates
    highlightSelectedWords : (sci_style Style, string Word).
clauses
    highlightSelectedWords(Style, Word) :-
        highlightSelectedWords2(Style, Word, selectionEnd, lastPos),
        highlightSelectedWords2(Style, Word, 0, selectionStart).

predicates
    highlightSelectedWords2 : (sci_style Style, string Word, integer Start, integer End).
clauses
    highlightSelectedWords2(Style, Word, S, E) :-
        if sci_characterRange(WS, WE) = tryFindText(scfind_wholeWord, sci_characterRange(S, E), Word) then
            if highlightSelectedWords_all = highlightSelectedWords_fact or Style = getStyleAt(WS) then
                addSelection(WS, WE),
                mainSelection := 0
            end if,
            highlightSelectedWords2(Style, Word, WE, E)
        end if.

clauses
    setDefaultLexerStyle() :-
        postAction(setDefaultLexerStyle_postAction).

clauses
    lexerDefault_visualProlog() :-
        postAction(lexerDefault_visualProlog_postAction).
clauses
    lexerDefault_Metta() :-
        postAction(lexerDefault_Metta_postAction).


clauses
    lexerDefault_vipGrammar() :-
        postAction(lexerDefault_vipGrammar_postAction).

clauses
    lexerDefault_cpp() :-
        postAction(lexerDefault_cpp_postAction).

clauses
    lexerDefault_html() :-
        postAction(lexerDefault_html_postAction).

clauses
    lexerDefault_xml() :-
        postAction(lexerDefault_xml_postAction).

clauses
    lexerDefault_markdown() :-
        postAction(lexerDefault_markdown_postAction).

predicates
    setDefaultLexerStyle_postAction : ().
clauses
    setDefaultLexerStyle_postAction() :-
        styleSetFont(style_default, "Verdana"),
        styleSetSize(style_default, styleGetSize(0)),

%31-10-2025
%        styleSetFore(style_default, 0x000000),

% dit was  best
%        styleSetFore(style_default, 0x505656),
% dit bepaald o.a. kleur in the message window
        styleSetFore(style_default, 0xB4B4B4 ),
% hierdoor is niet meer leesbaar  in de margin window met de nummers



%31-10-2025
%        styleSetBack(style_default, 0xFFFFFF),
%        styleSetBack(style_default, 0x646464 ),
% dit was best
       styleSetBack(style_default, 0x3F3F3F ),

%                 styleSetBack( sc_margin_back , 0x000000 ),

%        styleSetBack(style_default, 0x8F8F8F ),
%        styleSetBack(style_default, 0x757575 ),


%        styleSetBack(style_default, 0x000000 ),

%0x6A6725

        styleSetBold(style_default, false),
        styleSetItalic(style_default, false),
        styleSetUnderline(style_default, false),
        styleClearAll(),
        styleSetFore(style_braceLight, braceLight_styleFore),
        styleSetBack(style_braceLight, braceLight_styleBack),
        styleSetBold(style_braceLight, true).


predicates
    lexerDefault_Metta_postAction : ().
clauses
    lexerDefault_Metta_postAction() :-
        lexerDefault_Metta_style(),
        setKeywords(0,
            "car_atom car_atom let* $"
            "car_atom"),
% 9-11-2025
        setKeywords(1,
            "car_atom car_atom $"
            " car_atom"),
        setKeywords(2,
            "car_atom $"
            " car_atom $"),
        setKeywords(3, "car_atom $").



predicates
    lexerDefault_visualProlog_postAction : ().
clauses
    lexerDefault_visualProlog_postAction() :-
        lexerDefault_visualProlog_style(),
        setKeywords(0,
%            "car_atom cdr_atom goal namespace interface class implement where open inherits supports resolve delegate monitor constants domains predicates"
            "car_atom cdr_atom match match_all cons_atom let_star let komma println"
            " constructors properties clauses facts"),
        setKeywords(1,
%            "guard language stdcall apicall c thiscall prolog digits if then elseif else foreach do try catch finally erroneous failure procedure"
            "case and if not remove_atom remove_all_atoms add_atom add_atom_a add_atom_z size_atom"
            " determ multi nondeterm anyflow and or externally from div mod rem quot in orelse otherwise"),
        setKeywords(2,
            "include bininclude stringinclude requires orrequires if then else elseif endif error message export externally options"
            " grammarinclude grammargenerate"),
        setKeywords(3, "short detail end exception withdomain noformat").

predicates
    lexerDefault_vipGrammar_postAction : ().
clauses
    lexerDefault_vipGrammar_postAction() :-
        lexerDefault_visualProlog_style(),
        setKeywords(0, "namespace open grammar terminals nonterminals startsymbols precedence rules"),
        setKeywords(1, "left right nonassoc if then elseif else"),
        setKeywords(2, ""),
        setKeywords(3, "short detail end exception withdomain").

predicates
    lexerDefault_visualProlog_style : ().
clauses
    lexerDefault_visualProlog_style() :-
        lexer := sclex_visualProlog,
%        lexer := sclex_lisp,
%               lexer := sclex_phpscript,
%          sclex_lisp
%          sclex_phpscript

%31-10-2025
        styleSetFore(sce_visualProlog_default, default_styleFore),
%        styleSetFore(sce_visualProlog_default, 0x505656 ),

        styleSetFore(sce_visualProlog_key_major, key_major_styleFore),
        styleSetFore(sce_visualProlog_key_minor, key_minor_styleFore),
        styleSetFore(sce_visualProlog_key_directive, key_directive_styleFore),
        styleSetFore(sce_visualProlog_identifier, identifier_styleFore),

        styleSetFore(sce_visualProlog_variable, variable_styleFore),
%        styleSetFore(sce_visualProlog_variable, 0x505656 ),
        styleSetFore(sce_visualProlog_anonymous, anonymous_styleFore),
        styleSetFore(sce_visualProlog_number, number_styleFore),
        styleSetFore(sce_visualProlog_operator, operator_styleFore),
        % comment styles
        styleSetFore(sce_visualprolog_comment_block, comment_block_styleFore),
        styleSetFore(sce_visualProlog_comment_line, comment_line_styleFore),


%                IsItalic = toBoolean(b_false <> Italic),
 %       IsUnderline = toBoolean(b_false <> Underline),
  %      foreach Style = std::fromTo(0, style_max) do
   %%        styleSetSize(Style, PointSize),
     %       styleSetWeight(Style, Weight),
% 31-10-2025
            styleSetItalic(sce_visualProlog_comment_line, true ),


        styleSetFore(sce_visualProlog_comment_key, comment_key_styleFore),
        styleSetFore(sce_visualProlog_comment_key_error, comment_key_error_styleFore),
        % string styles
        styleSetFore(sce_visualProlog_string, string_styleFore),
        styleSetFore(sce_visualProlog_string_escape, string_escape_styleFore),
        % verbatim string styles
        styleSetFore(sce_visualProlog_string_verbatim, string_verbatim_styleFore),
        styleSetFore(sce_visualProlog_string_verbatim_special, string_verbatim_special_styleFore),
        styleSetBack(sce_visualProlog_string_verbatim_eol, string_verbatim_eol_styleBack),
        % character & string error styles
        styleSetFore(sce_visualProlog_string_eol_open, string_eol_open_styleFore),
        styleSetBack(sce_visualProlog_string_eol_open, string_eol_open_styleBack),
        styleSetFore(sce_visualProlog_string_escape_error, string_escape_error_styleFore),
        styleSetBack(sce_visualProlog_string_escape_error, string_escape_error_styleBack),
        % embeddes syntax and placeholders
        styleSetFore(sce_visualprolog_embedded, embedded_styleFore),
        styleSetFore(sce_visualprolog_placeholder, placeholder_styleFore).


predicates
    lexerDefault_Metta_style : ().
clauses
    lexerDefault_Metta_style() :-
%        lexer := sclex_visualProlog,

%    lexer := sclex_perl,    styleSetFore( sce_p_word, vpiDomains::color_Red ).


% Bij PHP doet hij wel  de dollar vars goed alleen de keywords werken niet
% keyword
%    lexer := sclex_phpscript,
%    styleSetFore( sce_hphp_variable, vpiDomains::color_Red ),
%    styleSetFore( sce_hphp_complex_variable, vpiDomains::color_Black ),
%    styleSetFore( sce_hphp_commentline, vpiDomains::color_White ),
%    styleSetFore(   sce_hphp_default  ,vpiDomains::color_Green ),
%    styleSetFore( sce_hphp_hstring ,0xFF0080 ),
%    styleSetFore( sce_hphp_simplestring ,vpiDomains::color_MediumGreen ),
%    styleSetFore( sce_hphp_word ,0xFFFF80 ),
%    styleSetFore( sce_hphp_number,0x4080FF ),
%    styleSetFore( sce_hphp_hstring_variable ,vpiDomains::color_LtGray ),
%    styleSetFore( sce_hphp_operator ,0x80FF ).




% sclex_perl   sce_p_word
% LISP
        lexer := sclex_lisp,
  styleSetFore( sce_lisp_default, vpiDomains::color_Red ),
   styleSetFore( sce_lisp_comment, vpiDomains::color_Blue ),
   styleSetFore( sce_lisp_number, vpiDomains::color_green ),
    styleSetFore( sce_lisp_keyword, vpiDomains::color_greenYellow ),
  styleSetFore( sce_lisp_keyword_kw, vpiDomains::color_blueViolet ),
  styleSetFore( sce_lisp_symbol, vpiDomains::color_yellowGreen ),
    styleSetFore( sce_lisp_string, vpiDomains::color_orchid ),
  styleSetFore( sce_lisp_stringeol, vpiDomains::color_orange ),
   styleSetFore( sce_lisp_identifier, vpiDomains::color_violetRed ),
    styleSetFore( sce_lisp_operator, vpiDomains::color_darkCyan ),
  styleSetFore( sce_lisp_special, vpiDomains::color_chocolate ),
   styleSetFore( sce_lisp_multi_comment, vpiDomains::color_coral ).




constants
    cppJavaIdlCsharp =
        "__declspec __int3264 __int64 abstract after aggregatable allocate and and_eq appobject array arrays as asm assert async async_uuid at attribute auto auto_handle base before bind bindable bitand bitor bool boolean bound break broadcast byte byte_count call_as callback case catch char checked class coclass code comm_status compl complex ComplexInf ComplexNaN const const_cast context_handle context_handle_noserialize context_handle_serialize continue control cpp_quote custom debugger decimal decode def default defaultbind defaultcollelem defaultvalue defaultvtable delegate delete dispinterface displaybind dllname do double dual dynamic_cast else enable_allocate encode endpoint entry enum error_status_t event exclusive explicit explicit_handle export extends extern false fault_status final finally first_is fixed float for foreach friend from function goto handle_t heap helpcontext helpfile helpstring helpstringcontext helpstringdll hidden hyper id idempotent if ignore iid_as iid_is immediatebind implements implicit implicit_handle import importlib in in_line include indexof Inf inline insert instanceof int interface internal is last_is lazy lcid length_is library licensed local lock long max_is maybe message methods midl_pragma midl_user_allocate midl_user_free min_is mod module ms_union mutable namespace NaN native ncacn_at_dsp ncacn_dnet_nsp ncacn_http ncacn_ip_tcp ncacn_nb_ipx ncacn_nb_nb ncacn_nb_tcp ncacn_np ncacn_spx ncacn_vns_spp ncadg_ip_udp ncadg_ipx ncadg_mq ncalrpc new nocode nonbrowsable noncreatable nonextensible not not_eq notify null object odl oleautomation operator optimize optional or or_eq out out_of_line override package params pipe pointer_default pragma private properties propget propput propputref protected ptr public public-init public-read range readonly ref register reinterpret_cast represent_as requestedit restrict restricted return retval reverse sbyte sealed shape short signed size_is size_t sizeof small source stackalloc static static_cast strict_context_handle string string_t struct super switch switch_is switch_type synchronized template then this throw throws transient transmit_as true try typedef typeid typename typeof uidefault uint ulong unchecked union unique unsafe unsigned user_marshal usesgetlasterror ushort using uuid v1_enum var vararg version virtual void volatile wchar_t while wire_marshal with xor xor_eq".

predicates
    lexerDefault_cpp_postAction : ().
clauses
    lexerDefault_cpp_postAction() :-
        lexer := sclex_cpp,
        setKeywords(0, cppJavaIdlCsharp),
        setKeywords(1, ""),
        styleSetFore_cpp(sce_c_default, default_styleFore),
        styleSetFore_cpp(sce_c_word, cpp_keyword_styleFore),
        styleSetFore_cpp(sce_c_word2, cpp_keyword2_styleFore),
        styleSetFore_cpp(sce_c_preprocessor, key_directive_styleFore),
        styleSetFore_cpp(sce_c_comment, comment_block_styleFore),
        styleSetFore_cpp(sce_c_commentline, comment_line_styleFore),
        styleSetFore_cpp(sce_c_commentdoc, comment_doc_styleFore),
        styleSetFore_cpp(sce_c_commentlinedoc, comment_doc_line_styleFore),
        styleSetFore_cpp(sce_c_commentdockeyword, comment_key_styleFore),
        styleSetFore_cpp(sce_c_commentdockeyworderror, comment_key_error_styleFore),
        styleSetFore_cpp(sce_c_identifier, identifier_styleFore),
        styleSetFore_cpp(sce_c_number, number_styleFore),
        styleSetFore_cpp(sce_c_string, string_styleFore),
        styleSetFore_cpp(sce_c_verbatim, string_verbatim_styleFore),
        styleSetBack(sce_c_stringeol, string_eol_open_styleBack),
        styleSetFore_cpp(sce_c_character, string_styleFore),
        styleSetFore_cpp(sce_c_operator, operator_styleFore),
        styleSetFore_cpp(sce_c_uuid, uuid_styleFore),
        styleSetFore_cpp(sce_c_regex, regex_styleFore).

predicates
    styleSetFore_cpp : (sci_style StyleNumber, unsigned Color).
clauses
    styleSetFore_cpp(StyleNumber, Color) :-
        styleSetFore(StyleNumber, Color),
        styleSetFore(StyleNumber + 64, Color).

constants
    html =
        "a abbr acronym address applet area b base basefont bdo big blockquote body br button caption center cite code col colgroup dd del dfn dir div dl dt em fieldset font form frame frameset h1 h2 h3 h4 h5 h6 head hr html i iframe img input ins isindex kbd label legend li link map menu meta noframes noscript object ol optgroup option p param pre q s samp script select small span strike strong style sub sup table tbody td textarea tfoot th thead title tr tt u ul var xml xmlns abbr accept  accept-charset accesskey action align alink alt archive axis background bgcolor border cellpadding cellspacing char charoff charset checkbox checked cite class classid clear codebase codetype color cols colspan compact content coords data datafld dataformatas datapagesize datasrc datetime declare defer dir disabled enctype event face file for frame frameborder headers height hidden href hreflang hspace http-equiv id image ismap label lang language leftmargin link longdesc marginheight marginwidth maxlength media method multiple name nohref noresize noshade nowrap object onblur onchange onclick ondblclick onfocus onkeydown onkeypress onkeyup onload onmousedown onmousemove onmouseout onmouseover onmouseup onreset onselect onsubmit onunload password profile prompt radio readonly rel reset rev rows rowspan rules scheme scope selected shape size span src standby start style submit summary tabindex target text title topmargin type usemap valign value valuetype version vlink vspace width address article aside audio base canvas command datalist details embed figcaption figure footer header hgroup keygen mark menu meter nav output progress rp rt ruby section source time video wbr async attributes autocomplete autofocus contenteditable contextmenu draggable form formaction formenctype formmethod formnovalidate formtarget list manifest max min novalidate pattern placeholder required reversed role sandbox scoped seamless sizes spellcheck srcdoc step" % attributes
        % html5 elements
        % html5
        .

constants
    javascript =
        "abstract boolean break byte case catch char class const continue debugger default delete do double else enum export extends final finally float for function goto if implements import in instanceof int interface long native new package private protected public return short static super switch synchronized this throw throws transient try typeof var void volatile while with".

constants
    vbscript =
        " Empty IsEmpty Nothing Is Nothing Null IsNull True False CDate Date DateAdd DateDiff DatePart DateSerial DateValue Day FormatDateTime Hour IsDate Minute Month MonthName Now Second Time Timer TimeSerial TimeValue Weekday WeekdayName Year Asc CBool CByte CCur CDate CDbl Chr CInt CLng CSng CStr Function Hex Oct  FormatCurrency FormatDateTime FormatNumber FormatPercent Function  Abs Atn Cos Exp Fix Hex Int Log Oct Rnd Sgn Sin Sqr Tan  Array Filter IsArray Join LBound Split UBound   Function InStr InStrRev LCase Left Len LTrim Mid Replace Right RTrim Space StrComp String StrReverse Trim UCase  CreateObject Eval GetLocale GetObject GetRef InputBox IsEmpty IsNull IsNumeric IsObject LoadPicture MsgBox RGB Round ScriptEngine ScriptEngineBuildVersion ScriptEngineMajorVersion ScriptEngineMinorVersion SetLocale TypeName VarType" % Date/Time Functions
        % Conversion Functions
        % Format Functions
        % Math Functions
        % Array Functions
        % String Functions
        % Other Functions
        .

constants
    python =
        "and as assert break class continue def del elif else except exec finally for from global if import in is lambda None not or pass print raise return try while with yield".

constants
    php =
        "and array as bool boolean break case cfunction class const continue declare default die directory do double echo else elseif empty enddeclare endfor endforeach endif endswitch endwhile eval exit extends false float for foreach function global goto if include include_once int integer isset list namespace new null object old_function or parent print real require require_once resource return static stdclass string switch true unset use var while xor abstract catch clone exception final implements interface php_user_filter private protected public this throw try __class__ __dir__ __file__ __function__ __line__ __method__ __namespace__ __sleep __wakeup".

constants
    sgml_dtd = "ELEMENT DOCTYPE ATTLIST ENTITY NOTATION".

predicates
    lexerDefault_html_postAction : ().
clauses
    lexerDefault_html_postAction() :-
        lexer := sclex_html,
        setKeywords(0, html),
        setKeywords(1, javascript),
        setKeywords(2, vbscript),
        setKeywords(3, python),
        setKeywords(4, php),
        setKeywords(5, sgml_dtd),
        xmlHtmlDefault(0xFF0000),
        % XML identifier start '<?'
        styleSetFore(sce_h_xmlstart, 0x0000FF),
        % XML identifier end '?>'
        styleSetFore(sce_h_xmlend, 0x0000FF),
        %  SCRIPT
        styleSetFore(sce_h_script, 0x000080),
        % ASP <% ... %>
        styleSetBack(sce_h_asp, 0xFFFF00),
        % ASP <% ... %>
        styleSetBack(sce_h_aspat, 0xFFDF00),
        % CDATA
        styleSetBack(sce_h_cdata, 0xFFDF00),
        % PHP
        styleSetFore(sce_h_question, 0x0000FF),
        styleSetBack(sce_h_question, 0xFFEFBF),
        % Unquoted values
        styleSetFore(sce_h_value, 0xFF00FF),
        styleSetBack(sce_h_value, 0xFFEFFF),
        % JSP Comment <%-- ... --%>
        styleSetFore(sce_h_xccomment, 0x000000),
        styleSetBack(sce_h_xccomment, 0xFFFFD0),
        % Some reasonable background colours found in the default Windows palette
        % Matched Operators
        styleSetFore(31, 0x0000FF),
        %32=fore:0xFF0000,notbold
        styleSetFore(32, 0xFF0000),
        % Embedded Javascript
        % JS Start - allows eol filled background to not start on same line as SCRIPT tag
        styleSetFore(sce_hj_start, 0x7F7F00),
        % JS Default
        styleSetFore(sce_hj_default, 0x000000),
        styleSetBack(sce_hj_default, embedJavaScript_styleBack),
        styleSetEolFilled(sce_hj_default, true),
        % JS Comment
        styleSetFore(sce_hj_comment, comment_block_styleFore),
        styleSetBack(sce_hj_comment, embedJavaScript_styleBack),
        styleSetEolFilled(sce_hj_comment, true),
        % JS Line Comment
        styleSetFore(sce_hj_commentline, comment_line_styleFore),
        styleSetBack(sce_hj_commentline, embedJavaScript_styleBack),
        % JS Doc comment
        styleSetFore(sce_hj_commentdoc, comment_key_styleFore),
        %styleSetBold(sce_hj_commentdoc, true),
        styleSetBack(sce_hj_commentdoc, embedJavaScript_styleBack),
        styleSetEolFilled(sce_hj_commentdoc, true),
        % JS Number
        styleSetFore(sce_hj_number, number_styleFore),
        styleSetBack(sce_hj_number, embedJavaScript_styleBack),
        % JS Word
        styleSetFore(sce_hj_word, 0x000000),
        styleSetBack(sce_hj_word, embedJavaScript_styleBack),
        % JS Keyword
        styleSetFore(sce_hj_keyword, cpp_keyword_styleFore),
        %styleSetBold(sce_hj_keyword, true),
        styleSetBack(sce_hj_keyword, embedJavaScript_styleBack),
        % JS Double quoted string
        styleSetFore(sce_hj_doublestring, string_styleFore),
        styleSetBack(sce_hj_doublestring, embedJavaScript_styleBack),
        % JS Single quoted string
        styleSetFore(sce_hj_singlestring, string_styleFore),
        styleSetBack(sce_hj_singlestring, embedJavaScript_styleBack),
        % JS Symbols
        styleSetFore(sce_hj_symbols, operator_styleFore),
        %styleSetBold(sce_hj_symbols, true),
        styleSetBack(sce_hj_symbols, embedJavaScript_styleBack),
        % JavaScript EOL
        styleSetBack(sce_hj_stringeol, 0xBFBBB0),
        styleSetEolFilled(sce_hj_stringeol, true),
        % JavaScript RegEx
        styleSetBack(sce_hj_regex, 0xFFBBB0),
        % ASP Javascript
        % JS Start - allows eol filled background to not start on same line as SCRIPT tag
        styleSetFore(sce_hja_start, 0x7F7F00),
        % JS Default
        styleSetFore(sce_hja_default, 0x000000),
        %styleSetBold(sce_hja_default, true),
        %sce_hja_default=font:javascomment_block_styleForecript_font
        styleSetBack(sce_hja_default, aspJavaScript_styleBack),
        styleSetEolFilled(sce_hja_default, true),
        % JS Comment
        styleSetFore(sce_hja_comment, 0x007F00),
        %sce_hja_comment=font:javascript_font
        styleSetBack(sce_hja_comment, aspJavaScript_styleBack),
        styleSetEolFilled(sce_hja_comment, true),
        % JS Line Comment
        styleSetFore(sce_hja_commentline, 0x007F00),
        %sce_hja_commentline=font:javascript_font
        styleSetBack(sce_hja_commentline, aspJavaScript_styleBack),
        % JS Doc comment
        styleSetFore(sce_hja_commentdoc, 0x7F7F7F),
        %styleSetBold(sce_hja_commentdoc, true),
        %sce_hja_commentdoc=font:javascript_font
        styleSetBack(sce_hja_commentdoc, aspJavaScript_styleBack),
        styleSetEolFilled(sce_hja_commentdoc, true),
        % JS Number
        styleSetFore(sce_hja_number, 0x007F7F),
        %sce_hja_number=font:javascript_font
        styleSetBack(sce_hja_number, aspJavaScript_styleBack),
        % JS Word
        styleSetFore(sce_hja_word, 0x000000),
        %sce_hja_word=font:javascript_font
        styleSetBack(sce_hja_word, aspJavaScript_styleBack),
        % JS Keyword
        styleSetFore(sce_hja_keyword, 0x00007F),
        %styleSetBold(sce_hja_keyword, true),
        %sce_hja_keyword=font:javascript_font
        styleSetBack(sce_hja_keyword, aspJavaScript_styleBack),
        % JS Double quoted string
        styleSetFore(sce_hja_doublestring, 0x7F007F),
        %sce_hja_doublestring=font:javascript_font
        styleSetBack(sce_hja_doublestring, aspJavaScript_styleBack),
        % JS Single quoted string
        styleSetFore(sce_hja_singlestring, 0x7F007F),
        %sce_hja_singlestring=font:javascript_font
        styleSetBack(sce_hja_singlestring, aspJavaScript_styleBack),
        % JS Symbols
        styleSetFore(sce_hja_symbols, 0x000000),
        %styleSetBold(sce_hja_symbols, true),
        %sce_hja_symbols=font:javascript_font
        styleSetBack(sce_hja_symbols, aspJavaScript_styleBack),
        % JavaScript EOL
        styleSetBack(sce_hja_stringeol, 0xBFBBB0),
        styleSetEolFilled(sce_hja_stringeol, true),
        % JavaScript RegEx
        styleSetBack(sce_hja_regex, 0xFFBBB0),
        % Embedded VBS
        % Default
        %sce_hb_default=font:vbscript_font
        styleSetBack(sce_hb_default, embedVbScript_styleBack),
        styleSetFore(sce_hb_default, 0x000000),
        styleSetEolFilled(sce_hb_default, true),
        % Comment
        %sce_hb_commentline=font:comment_font
        styleSetBack(sce_hb_commentline, embedVbScript_styleBack),
        styleSetFore(sce_hb_commentline, 0x008000),
        styleSetEolFilled(sce_hb_commentline, true),
        % Number
        %sce_hb_number=font:vbscript_font
        styleSetBack(sce_hb_number, embedVbScript_styleBack),
        styleSetFore(sce_hb_number, 0x008080),
        styleSetEolFilled(sce_hb_number, true),
        % KeyWord
        %sce_hb_word=font:vbscript_font
        styleSetBack(sce_hb_word, embedVbScript_styleBack),
        styleSetFore(sce_hb_word, 0x000080),
        %styleSetBold(sce_hb_word, true),
        styleSetEolFilled(sce_hb_word, true),
        % String
        %sce_hb_string=font:vbscript_font
        styleSetBack(sce_hb_string, embedVbScript_styleBack),
        styleSetFore(sce_hb_string, 0x800080),
        styleSetEolFilled(sce_hb_string, true),
        % Identifier
        %sce_hb_identifier=font:vbscript_font
        styleSetBack(sce_hb_identifier, embedVbScript_styleBack),
        styleSetFore(sce_hb_identifier, 0x000080),
        styleSetEolFilled(sce_hb_identifier, true),
        % Unterminated string
        %sce_hb_stringeol=font:vbscript_font
        styleSetBack(sce_hb_stringeol, 0x7F7FFF),
        styleSetFore(sce_hb_stringeol, 0x000080),
        styleSetEolFilled(sce_hb_stringeol, true),
        % ASP VBS
        % Start
        % Default
        %sce_hba_default=font:vbscript_font
        styleSetBack(sce_hba_default, aspVbScript_styleBack),
        styleSetFore(sce_hba_default, 0x000000),
        styleSetEolFilled(sce_hba_default, true),
        % Comment
        %sce_hba_commentline=font:comment_font
        styleSetBack(sce_hba_commentline, aspVbScript_styleBack),
        styleSetFore(sce_hba_commentline, 0x008000),
        styleSetEolFilled(sce_hba_commentline, true),
        % Number
        %sce_hba_number=font:vbscript_font
        styleSetBack(sce_hba_number, aspVbScript_styleBack),
        styleSetFore(sce_hba_number, 0x008080),
        styleSetEolFilled(sce_hba_number, true),
        % KeyWord
        %sce_hba_word=font:vbscript_font
        styleSetBack(sce_hba_word, aspVbScript_styleBack),
        styleSetFore(sce_hba_word, 0x000080),
        %styleSetBold(sce_hba_word, true),
        styleSetEolFilled(sce_hba_word, true),
        % String
        %sce_hba_string=font:vbscript_font
        styleSetBack(sce_hba_string, aspVbScript_styleBack),
        styleSetFore(sce_hba_string, 0x800080),
        styleSetEolFilled(sce_hba_string, true),
        % Identifier
        %sce_hba_identifier=font:vbscript_font
        styleSetBack(sce_hba_identifier, aspVbScript_styleBack),
        styleSetFore(sce_hba_identifier, 0x000080),
        styleSetEolFilled(sce_hba_identifier, true),
        % Unterminated string
        %sce_hba_stringeol=font:vbscript_font
        styleSetBack(sce_hba_stringeol, 0x7F7FBF),
        styleSetFore(sce_hba_stringeol, 0x000080),
        styleSetEolFilled(sce_hba_stringeol, true),
        % Embedded Python
        styleSetFore(sce_hp_start, 0x808080),
        styleSetFore(sce_hp_default, 0x808080),
        styleSetBack(sce_hp_default, embedPython_styleBack),
        styleSetEolFilled(sce_hp_default, true),
        % Comment
        styleSetFore(sce_hp_commentline, 0x007F00),
        %sce_hp_commentline=font:comment_font
        styleSetBack(sce_hp_commentline, embedPython_styleBack),
        styleSetEolFilled(sce_hp_commentline, true),
        % Number
        styleSetFore(sce_hp_number, 0x007F7F),
        styleSetBack(sce_hp_number, embedPython_styleBack),
        styleSetEolFilled(sce_hp_number, true),
        % String
        styleSetFore(sce_hp_string, 0x7F007F),
        %sce_hp_string=font:monospace_font
        styleSetBack(sce_hp_string, embedPython_styleBack),
        styleSetEolFilled(sce_hp_string, true),
        % Single quoted string
        styleSetFore(sce_hp_character, 0x7F007F),
        %sce_hp_character=font:monospace_font
        styleSetBack(sce_hp_character, embedPython_styleBack),
        styleSetEolFilled(sce_hp_character, true),
        % Keyword
        styleSetFore(sce_hp_word, 0x00007F),
        %styleSetBold(sce_hp_word, true),
        styleSetBack(sce_hp_word, embedPython_styleBack),
        styleSetEolFilled(sce_hp_word, true),
        % Triple quotes
        styleSetFore(sce_hp_triple, 0x7F0000),
        styleSetBack(sce_hp_triple, embedPython_styleBack),
        styleSetEolFilled(sce_hp_triple, true),
        % Triple double quotes
        styleSetFore(sce_hp_tripledouble, 0x7F0000),
        styleSetBack(sce_hp_tripledouble, embedPython_styleBack),
        styleSetEolFilled(sce_hp_tripledouble, true),
        % Class name definition
        styleSetFore(sce_hp_classname, 0x0000FF),
        %styleSetBold(sce_hp_classname, true),
        styleSetBack(sce_hp_classname, embedPython_styleBack),
        styleSetEolFilled(sce_hp_classname, true),
        % Function or method name definition
        styleSetFore(sce_hp_defname, 0x007F7F),
        %styleSetBold(sce_hp_defname, true),
        styleSetBack(sce_hp_defname, embedPython_styleBack),
        styleSetEolFilled(sce_hp_defname, true),
        % Operators
        %styleSetBold(sce_hp_operator, true),
        styleSetBack(sce_hp_operator, embedPython_styleBack),
        styleSetEolFilled(sce_hp_operator, true),
        % Identifiers
        styleSetBack(sce_hp_identifier, embedPython_styleBack),
        styleSetEolFilled(sce_hp_identifier, true),
        % PHP complex variable
        styleSetFore(sce_hphp_complex_variable, 0x007F00),
        styleSetItalic(sce_hphp_complex_variable, true),
        styleSetBack(sce_hphp_complex_variable, embedPHP_styleBack),
        % ASP Python
        styleSetFore(sce_hpa_default, 0x808080),
        styleSetBack(sce_hpa_default, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_default, true),
        % Comment
        styleSetFore(sce_hpa_commentline, 0x007F00),
        %sce_hpa_commentline=font:comment_font
        styleSetBack(sce_hpa_commentline, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_commentline, true),
        % Number
        styleSetFore(sce_hpa_number, 0x007F7F),
        styleSetBack(sce_hpa_number, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_number, true),
        % String
        styleSetFore(sce_hpa_string, 0x7F007F),
        %sce_hpa_string=font:monospace_font
        styleSetBack(sce_hpa_string, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_string, true),
        % Single quoted string
        styleSetFore(sce_hpa_character, 0x7F007F),
        %sce_hpa_character=font:monospace_font
        styleSetBack(sce_hpa_character, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_character, true),
        % Keyword
        styleSetFore(sce_hpa_word, 0x00007F),
        %styleSetBold(sce_hpa_word, true),
        styleSetBack(sce_hpa_word, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_word, true),
        % Triple quotes
        styleSetFore(sce_hpa_triple, 0x7F0000),
        styleSetBack(sce_hpa_triple, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_triple, true),
        % Triple double quotes
        styleSetFore(sce_hpa_tripledouble, 0x7F0000),
        styleSetBack(sce_hpa_tripledouble, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_tripledouble, true),
        % Class name definition
        styleSetFore(sce_hpa_classname, 0x0000FF),
        %styleSetBold(sce_hpa_classname, true),
        styleSetBack(sce_hpa_classname, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_classname, true),
        % Function or method name definition
        styleSetFore(sce_hpa_defname, 0x007F7F),
        %styleSetBold(sce_hpa_defname, true),
        styleSetBack(sce_hpa_defname, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_defname, true),
        % Operators
        %styleSetBold(sce_hpa_operator, true),
        styleSetBack(sce_hpa_operator, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_operator, true),
        % Identifiers
        styleSetBack(sce_hpa_identifier, aspPython_styleBack),
        styleSetEolFilled(sce_hpa_identifier, true),
        % PHP
        % Default
        styleSetFore(sce_hphp_default, 0x000033),
        styleSetBack(sce_hphp_default, embedPHP_styleBack),
        styleSetEolFilled(sce_hphp_default, true),
        % Double quoted String
        styleSetFore(sce_hphp_hstring, 0x007F00),
        styleSetBack(sce_hphp_hstring, embedPHP_styleBack),
        % Single quoted string
        styleSetFore(sce_hphp_simplestring, 0x009F00),
        styleSetBack(sce_hphp_simplestring, embedPHP_styleBack),
        % Keyword
        styleSetFore(sce_hphp_word, 0x7F007F),
        styleSetItalic(sce_hphp_word, true),
        styleSetBack(sce_hphp_word, embedPHP_styleBack),
        % Number
        styleSetFore(sce_hphp_number, 0xCC9900),
        styleSetBack(sce_hphp_number, embedPHP_styleBack),
        % Variable
        styleSetFore(sce_hphp_variable, 0x00007F),
        styleSetItalic(sce_hphp_variable, true),
        styleSetBack(sce_hphp_variable, embedPHP_styleBack),
        % Comment
        styleSetFore(sce_hphp_comment, 0x999999),
        %sce_hphp_comment=font:comment_font
        styleSetBack(sce_hphp_comment, embedPHP_styleBack),
        % One line comment
        styleSetFore(sce_hphp_commentline, 0x666666),
        styleSetItalic(sce_hphp_commentline, true),
        %sce_hphp_commentline=font:comment_font
        styleSetBack(sce_hphp_commentline, embedPHP_styleBack),
        % PHP variable in double quoted string
        styleSetFore(sce_hphp_hstring_variable, 0x007F00),
        styleSetItalic(sce_hphp_hstring_variable, true),
        styleSetBack(sce_hphp_hstring_variable, embedPHP_styleBack),
        % PHP operator
        styleSetFore(sce_hphp_operator, operator_styleFore).

predicates
    lexerDefault_xml_postAction : ().
clauses
    lexerDefault_xml_postAction() :-
        lexer := sclex_xml,
        xmlHtmlDefault(0x000080),
        styleSetFore(sce_h_xmlstart, 0x800080),
        %styleSetBold(sce_h_xmlstart, true),
        % XML identifier end '?>'
        styleSetFore(sce_h_xmlend, 0x800080),
        %styleSetBold(sce_h_xmlend, true),
        % CDATA
        styleSetFore(sce_h_cdata, 0x800000),
        styleSetBack(sce_h_cdata, 0xFFF0F0),
        styleSetEolFilled(sce_h_cdata, true),
        % Question
        styleSetFore(sce_h_question, 0x800000),
        % Unquoted Value
        styleSetFore(sce_h_value, 0x608060).

predicates
    xmlHtmlDefault : (unsigned UnknownColor).
clauses
    xmlHtmlDefault(UnknownColor) :-
        % Text
        styleSetFore(sce_h_default, 0x000000),
        % Tags
        styleSetFore(sce_h_tag, key_minor_styleFore),
        % Unknown Tags
        styleSetFore(sce_h_tagunknown, UnknownColor),
        % Attributes
        styleSetFore(sce_h_attribute, variable_styleFore),
        % Unknown Attributes
        styleSetFore(sce_h_attributeunknown, UnknownColor),
        % Numbers
        styleSetFore(sce_h_number, number_styleFore),
        % Double quoted strings
        styleSetFore(sce_h_doublestring, string_styleFore),
        % Single quoted strings
        styleSetFore(sce_h_singlestring, string_styleFore),
        % Other inside tag
        styleSetFore(sce_h_other, 0x800080),
        % Comment
        styleSetFore(sce_h_comment, comment_line_styleFore),
        % Entities
        styleSetFore(sce_h_entity, 0x800080),
        % XML style tag ends '/>'
        styleSetFore(sce_h_tagend, 0x000080),
        % SGML tags <! ... >
        styleSetFore(sce_h_sgml_default, 0x000080),
        styleSetBack(sce_h_sgml_default, embedSGML_styleBack),
        % SGML command
        styleSetFore(sce_h_sgml_command, 0x000080),
        styleSetBack(sce_h_sgml_command, embedSGML_styleBack),
        %styleSetBold(sce_h_sgml_command, true),
        % SGML 1st param
        styleSetFore(sce_h_sgml_1st_param, 0x006600),
        styleSetBack(sce_h_sgml_1st_param, embedSGML_styleBack),
        % SGML double string
        styleSetFore(sce_h_sgml_doublestring, 0x800000),
        styleSetBack(sce_h_sgml_doublestring, embedSGML_styleBack),
        % SGML single string
        styleSetFore(sce_h_sgml_simplestring, 0x993300),
        styleSetBack(sce_h_sgml_simplestring, embedSGML_styleBack),
        % SGML error
        styleSetFore(sce_h_sgml_error, 0x800000),
        styleSetBack(sce_h_sgml_error, 0xFF6666),
        % SGML special (0xxxxx type)
        styleSetFore(sce_h_sgml_special, 0x3366FF),
        styleSetBack(sce_h_sgml_special, embedSGML_styleBack),
        % SGML entity
        styleSetFore(sce_h_sgml_entity, 0x333333),
        styleSetBack(sce_h_sgml_entity, embedSGML_styleBack),
        % SGML comment
        styleSetFore(sce_h_sgml_comment, 0x808000),
        styleSetBack(sce_h_sgml_comment, embedSGML_styleBack),
        %
        % 30:sce_=h_sgml_1st_param_comment
        % SGML block
        styleSetFore(sce_h_sgml_block_default, 0x000066),
        styleSetBack(sce_h_sgml_block_default, 0xCCCCE0).

predicates
    lexerDefault_markdown_postAction : ().
clauses
    lexerDefault_markdown_postAction() :-
        lexer := sclex_markdown,
        styleSetFore(sce_markdown_default, 0x000000),
        styleSetFore(sce_markdown_line_begin, 0x000000),
        styleSetFore(sce_markdown_strong1, 0x224466),
        styleSetBold(sce_markdown_strong1, true),
        styleSetFore(sce_markdown_strong2, 0x224466),
        styleSetBold(sce_markdown_strong2, true),
        styleSetFore(sce_markdown_em1, 0x663300),
        styleSetItalic(sce_markdown_em1, true),
        styleSetFore(sce_markdown_em2, 0x663300),
        styleSetItalic(sce_markdown_em2, true),
        HeaderStyles =
            [sce_markdown_header1, sce_markdown_header2, sce_markdown_header3, sce_markdown_header4, sce_markdown_header5, sce_markdown_header6],
        foreach HS in HeaderStyles do
            styleSetFore(HS, 0x5183C4)
        end foreach,
        styleSetFore(sce_markdown_prechar, 0x000000),
        styleSetBack(sce_markdown_prechar, 0xEEEEAA),
        styleSetFore(sce_markdown_ulist_item, 0x555555),
        styleSetFore(sce_markdown_olist_item, 0x555555),
        styleSetFore(sce_markdown_blockquote, 0x000088),
        styleSetFore(sce_markdown_strikeout, 0x18453B),
        styleSetBack(sce_markdown_strikeout, 0xA9BA9D),
        styleSetFore(sce_markdown_hrule, 0x555555),
        styleSetFore(sce_markdown_link, 0x0000AA),
        styleSetUnderline(sce_markdown_link, true),
        foreach C in [sce_markdown_code, sce_markdown_code2, sce_markdown_codebk] do
            styleSetFore(C, 0x000088),
            styleSetBack(C, 0xEEEEEE)
        end foreach,
        foreach Strong in [sce_markdown_strong1, sce_markdown_strong2, sce_markdown_hrule] or Strong in HeaderStyles do
            styleSetBold(Strong, true)
        end foreach.

clauses
    lexerDefault_extension(Ext) :-
        ExtLow = string::toLowerCase(Ext),
        extension_lexer(Lexer, ExtensionList),
        ExtLow in ExtensionList,
        !,
        Lexer().

    lexerDefault_extension(_Ext).

class facts
    fileExtension_visualProlog : string* := ["pro", "i", "cl", "ph", "pack", "win", "mnu", "dlg", "frm", "tb", "prc"].
    fileExtension_vipGrammar : string* := ["vipgrm"].
    fileExtension_xml : string* := ["vipprj", "version", "manifest", "xml", "xsl", "svg", "xul", "xsd", "dtd", "xslt", "axl", "xrc", "rdf"].
    fileExtension_cpp : string* := ["c", "cpp", "cxx", "h", "hpp", "hxx", "js", "java", "cs", "idl", "odl"].
    fileExtension_html : string* :=
        ["htm", "html", "asp", "shtml", "htd", "js", "jsp", "php", "php3", "php5", "phtml", "htt", "cfm", "tpl", "dtd", "hta"].
    fileExtension_markdown : string* := ["md", "markdown"] [constant].

predicates
    extension_lexer : (runnable Lexer [out], string* Extension [out]) multi.
clauses
    extension_lexer(lexerDefault_visualProlog, fileExtension_visualProlog).
    extension_lexer(lexerDefault_vipGrammar, fileExtension_vipGrammar).
    extension_lexer(lexerDefault_xml, fileExtension_xml).
    extension_lexer(lexerDefault_cpp, fileExtension_cpp).
    extension_lexer(lexerDefault_html, fileExtension_html).
    extension_lexer(lexerDefault_markdown, fileExtension_markdown).

clauses
    textUtf8() = string8::empty :-
        isErroneous(native),
        !.

    textUtf8() = Text8 :-
        L = lastPos,
        Text8 = string8::create(L),
        sciLexer_api::getText(native, L + 1, Text8).

clauses
    textUtf8(Text8) :-
        sciLexer_api::setText(native, Text8).

constants
    bufferSize : byteCount = 8 * 1024.

clauses
    textSegmented_utf8(Seg) :-
        beginUndoAction(),
        textUtf8 := string8::empty,
        foreach Text8 = segmented_utf8::getAll_nd(Seg) do
            appendText_utf8(Text8)
        end foreach,
        endUndoAction().

clauses
    textSegmented_utf16(Seg) :-
        beginUndoAction(),
        textUtf8 := string8::empty,
        foreach Text16 = segmented_utf16::getAll_nd(Seg) do
            appendText(Text16)
        end foreach,
        endUndoAction().

clauses
    textSegmented_utf8() = textSegmented(segmented_utf8::support, getTextRange_utf8, 0, lastPos).

clauses
    textSegmented_utf16() = segmented::s16(textSegmented(segmented_utf16::support, getTextRange, 0, lastPos)).

class predicates
    textSegmented : (segmentedSupport{Type} SegSupport, function{integer CpFrom, integer CpTo, Type Text} GetTextRange, integer CpMin,
        integer CpLast) -> segmented::segmented{Type} Segmented.
clauses
    textSegmented(SegSupport, GetTextRange, CpMin, CpLast) = S :-
        CpNext = CpMin + bufferSize,
        A = SegSupport:mkSegment(GetTextRange(CpMin, CpNext)),
        if CpNext <= CpLast then
            S = SegSupport:concat(A, textSegmented(SegSupport, GetTextRange, CpNext, CpLast))
        else
            S = A
        end if.

clauses
    readFromStreamAndClose(Stream) :-
        try
            beginUndoAction(),
            textUtf8 := string8::empty,
            foreach Stream:repeatToEndOfStream() do
                appendText(Stream:readString(bufferSize div 2))
            end foreach,
            endUndoAction()
        finally
            Stream:close()
        end try.

clauses
    writeToStreamAndClose(Stream) :-
        try
            writeToStreamAndClose2(Stream, 0, lastPos)
        finally
            Stream:close()
        end try.

predicates
    writeToStreamAndClose2 : (outputStream Stream, integer CpMin, integer CpLast).
clauses
    writeToStreamAndClose2(Stream, CpMin, CpLast) :-
        CpNext = CpMin + bufferSize,
        Stream:write(getTextRange(CpMin, CpNext)),
        if CpNext <= CpLast then
            writeToStreamAndClose2(Stream, CpNext, CpLast)
        end if.

clauses
    text() = string8::fromUtf8(textUtf8).

clauses
    text(Text) :-
        textUtf8 := string8::toUtf8(Text).

clauses
    setSavepoint() :-
        sciLexer_api::setSavepoint(native).

clauses
    getLine(Line) = string8::fromUtf8(Text8) :-
        L = sciLexer_api::lineLength(native, fromLineNumber(Line)),
        Text8 = string8::create(L),
        sciLexer_api::getLine(native, fromLineNumber(Line), Text8).

clauses
    replaceSel(Text) :-
        replaceSel_utf8(string8::toUtf8(Text)).

clauses
    replaceSel_utf8(Text8) :-
        sciLexer_api::replaceSel(native, Text8).

clauses
    readOnly() = boolean::fromBooleanInt(sciLexer_api::getReadOnly(native)).

clauses
    readOnly(ReadOnly) :-
        whenCreated({  :- sciLexer_api::setReadOnly(native, boolean::toBooleanInt(ReadOnly)) }).

clauses
    lastPos() = sciLexer_api::getTextLength(native).

clauses
    getTextRange(CpMin, CpMax) = string8::fromUtf8(getTextRange_utf8(CpMin, CpMax)).

clauses
    getTextRange_utf8(CpMin, -1) = getTextRange_utf8(CpMin, lastPos) :-
        !.
    getTextRange_utf8(CpMin1, CpMax1) = Text8 :-
        CpMin2 = adjustPositionNext(CpMin1),
        CpMax2 = adjustPositionNext(CpMax1),
        L = CpMax2 - CpMin2,
        Text8 = string8::create(L),
        _L = sciLexer_api::getTextRange(native, sci_textRange(sci_characterRange(CpMin2, CpMax2), Text8)).

clauses
    allocate(Size) :-
        sciLexer_api::allocate(native, Size).

clauses
    appendText(Text) :-
        appendText_utf8(string8::toUtf8(Text)).

clauses
    appendText_utf8(Text8) :-
        L = string8::length(Text8),
        sciLexer_api::appendText(native, L, Text8).

clauses
    insertText(Text) :-
        insertText(-1, Text).

clauses
    insertText_utf8(Text8) :-
        insertText_utf8(-1, Text8).

clauses
    insertText(Pos, Text) :-
        insertText_utf8(Pos, string8::toUtf8(Text)).

clauses
    insertText_utf8(Pos, Text8) :-
        sciLexer_api::insertText(native, Pos, Text8).

clauses
    addText(Text) :-
        addText_utf8(string8::toUtf8(Text)).

clauses
    addText_utf8(Text8) :-
        L = string8::length(Text8),
        sciLexer_api::addText(native, L, Text8).

clauses
    clearAll() :-
        sciLexer_api::clearAll(native).

clauses
    deleteRange(Pos, DeleteLength) :-
        sciLexer_api::deleteRange(native, Pos, DeleteLength).

clauses
    clearDocumentStyle() :-
        sciLexer_api::clearDocumentStyle(native).

clauses
    tryGetChar8At(Pos) = Char8 :-
        Char8 = sciLexer_api::getCharAt(native, Pos),
        0 <> Char8.

clauses
    tryGetCharAt(Pos) = Char :-
        U8 = tryGetChar8At(Pos),
        if U8 < 0x80 then
            Char = uncheckedConvert(char, convert(unsigned16, U8))
        else
            PosAfter = tryPositionAfter(Pos),
            PosStart = tryPositionBefore(PosAfter),
            L = PosAfter - PosStart,
            MultibyteChar8 = string8::create(L),
            _L = sciLexer_api::getTextRange(native, sci_textRange(sci_characterRange(PosStart, PosAfter), MultibyteChar8)),
            CharString = string8::fromUtf8(MultibyteChar8),
            Char = memory::getChar(uncheckedConvert(pointer, CharString))
        end if.

clauses
    getStyleAt(Pos) = sciLexer_api::getStyleAt(native, Pos).

% Search and Replace
clauses
    tryFindText(SearchFlags, SearchRange, Text) = tryFindText_utf8(SearchFlags, SearchRange, string8::toUtf8(Text)).

clauses
    tryFindText_utf8(SearchFlags, SearchRange, Text8) = MatchCharRange :-
        TTF = sci_textToFind(SearchRange, Text8, sci_characterRange(0, 0)),
        -1 <> sciLexer_api::findText(native, SearchFlags, TTF),
        sci_textToFind(_, _, MatchCharRange) = TTF.

clauses
    searchAnchor() :-
        sciLexer_api::searchAnchor(native).

clauses
    trySearchNext(SearchFlags, Text) = trySearchNext_utf8(SearchFlags, string8::toUtf8(Text)).

clauses
    trySearchNext_utf8(SearchFlags, Text8) = FoundPos :-
        FoundPos = sciLexer_api::searchNext(native, SearchFlags, Text8),
        -1 <> FoundPos.

clauses
    trySearchPrev(SearchFlags, Text) = trySearchPrev_utf8(SearchFlags, string8::toUtf8(Text)).

clauses
    trySearchPrev_utf8(SearchFlags, Text8) = FoundPos :-
        FoundPos = sciLexer_api::searchPrev(native, SearchFlags, Text8),
        -1 <> FoundPos.

% Search and replace using the target
clauses
    targetStart(Pos) :-
        sciLexer_api::setTargetStart(native, Pos).

clauses
    targetStart() = sciLexer_api::getTargetStart(native).

clauses
    targetEnd(Pos) :-
        sciLexer_api::setTargetEnd(native, Pos).

clauses
    targetEnd() = sciLexer_api::getTargetEnd(native).

clauses
    targetFromSelection() :-
        sciLexer_api::targetFromSelection(native).

clauses
    searchFlags(Bits) :-
        sciLexer_api::setSearchFlags(native, Bits).

clauses
    searchFlags() = sciLexer_api::getSearchFlags(native).

clauses
    trySearchInTarget(Text) = trySearchInTarget_utf8(string8::toUtf8(Text)).

clauses
    trySearchInTarget_utf8(Text8) = MatchPos :-
        Length = string8::length(Text8),
        MatchPos = sciLexer_api::searchInTarget(native, Length, Text8),
        -1 <> MatchPos.

clauses
    replaceTarget(Text) :-
        _ = replaceTarget(Text).

clauses
    replaceTarget_utf8(Text8) :-
        _ = replaceTarget_utf8(Text8).

clauses
    replaceTarget(Text) = replaceTarget_utf8(string8::toUtf8(Text)).

clauses
    replaceTarget_utf8(Text8) = sciLexer_api::replaceTarget(native, -1, Text8).

clauses
    replaceTargetRE(Text) :-
        _ = replaceTargetRE(Text).

clauses
    replaceTargetRE_utf8(Text8) :-
        _ = replaceTargetRE_utf8(Text8).

clauses
    replaceTargetRE(Text) = replaceTargetRE_utf8(string8::toUtf8(Text)).

clauses
    replaceTargetRE_utf8(Text8) = sciLexer_api::replaceTargetRE(native, -1, Text8).

clauses
    getTag(TagNumber) = string8::fromUtf8(TagValue8) :-
        Length = sciLexer_api::getTag(native, TagNumber, uncheckedConvert(string8, null)),
        TagValue8 = string8::create(Length),
        _ = sciLexer_api::getTag(native, TagNumber, TagValue8),
        _ = toString(Length).

clauses
    removeTrailingSpaces() :-
        targetStart := 0,
        SearchFlags = searchFlags,
        searchFlags := sciLexer_native::scfind_regExp,
        beginUndoAction(),
        removeTrailingSpaces2(),
        endUndoAction(),
        searchFlags := SearchFlags.

predicates
    removeTrailingSpaces2 : ().
clauses
    removeTrailingSpaces2() :-
        targetEnd := lastPos,
        if _ = trySearchInTarget(@"[ \t]+$") then
            replaceTargetRE(""),
            removeTrailingSpaces2()
        end if.

% Overtype
clauses
    overType(OverType) :-
        sciLexer_api::setOverType(native, boolean::toBooleanInt(OverType)).

clauses
    overType() = boolean::fromBooleanInt(sciLexer_api::getOverType(native)).

%  Cut, copy and paste
clauses
    cut() :-
        if true = canCut and true = readOnly then
            modifyAttemptROEvent:notify()
        end if,
        if false = readOnly then
            copy(),
            replaceSel("")
        end if.

clauses
    copy() :-
        if true = canCopy and clipboard_api::clipboardIsAvailable(nullHandle) then
            clipboard::putHtml(This, selText, getHTML(selectionStart, selectionEnd))
        end if.

predicates
    getHTML : (integer From, integer To) -> string HTML.
clauses
    getHTML(From, To) = S:getString() :-
        S = outputStream_string::new(),
        if From < To then
            Font = styleGetFont(style_default),
            if "" <> Font then
                S:writef("<span style=\"font-family:'%s'\">", Font)
            end if,
            writeHTML(S, noStyle, From, To),
            if "" <> Font then
                S:write("</span>")
            end if
        end if.

constants
    noStyle = 0.

predicates
    writeHTML : (outputStream S, sci_style Style, integer From, integer To).
clauses
    writeHTML(S, StylePre, From, To) :-
        From < To,
        Char = tryGetCharAt(From),
        !,
        Style = getStyleAt(From),
        if StylePre <> Style then
            if noStyle <> StylePre then
                S:write("</span>")
            end if,
            if noStyle <> Style then
                Color = styleGetFore(Style),
                B = Color mod 0x100,
                G = Color div 0x100 mod 0x100,
                R = Color div 0x10000 mod 0x100,
                S:writef("<span style=\"color:#%02x%02x%02x\">", B, G, R)
            end if
        end if,
        if Encoded = mustEncode(Char) then
            S:write(Encoded)
        else
            S:write(Char)
        end if,
        if Next = tryPositionAfter(From) then
            writeHTML(S, Style, Next, To)
        end if.

    writeHTML(_S, _Style, _From, _To).

class predicates
    mustEncode : (char Char) -> string Encoded determ.
clauses
    mustEncode(' ') = "&nbsp;".
    mustEncode('<') = "&lt;".
    mustEncode('>') = "&gt;".
    mustEncode('\"') = "&quot;".
    mustEncode('\'') = "&#39;".
    mustEncode('&') = "&amp;".
    mustEncode('\n') = "<br />\n".

clauses
    paste() :-
        sciLexer_api::paste(native).

clauses
    clear() :-
        sciLexer_api::clear(native).

clauses
    canPaste() = if isErroneous(native) then false else boolean::fromBooleanInt(sciLexer_api::canPaste(native)) end if.

clauses
    canCopy() = toBoolean(selectionStart <> selectionEnd).

facts
    allowReadOnlyCut : boolean := true.

clauses
    canCut() = if isErroneous(native) then false else canCopy ** (allowReadOnlyCut ++ ~~readOnly) end if.

clauses
    copyAllowLine() :-
        sciLexer_api::copyAllowLine(native).

clauses
    copyRange(Start, End) :-
        sciLexer_api::copyRange(native, Start, End).

clauses
    copyText(Text) :-
        copyText_utf8(string8::toUtf8(Text)).

clauses
    copyText_utf8(Text8) :-
        Length = string8::length(Text8),
        sciLexer_api::copyText(native, Length, Text8).

clauses
    pasteConvertEndings() = boolean::fromBooleanInt(sciLexer_api::getPasteConvertEndings(native)).

clauses
    pasteConvertEndings(V) :-
        sciLexer_api::setPasteConvertEndings(native, boolean::toBooleanInt(V)).

%  Error handling
clauses
    status() = sciLexer_api::getStatus(native).

clauses
    status(V) :-
        sciLexer_api::setStatus(native, V).

%  Undo and Redo
clauses
    undo() :-
        sciLexer_api::undo(native).

clauses
    redo() :-
        sciLexer_api::redo(native).

clauses
    canUndo() = if isErroneous(native) then false else boolean::fromBooleanInt(sciLexer_api::canUndo(native)) end if.

clauses
    canRedo() = if isErroneous(native) then false else boolean::fromBooleanInt(sciLexer_api::canRedo(native)) end if.

clauses
    emptyUndoBuffer() :-
        sciLexer_api::emptyUndoBuffer(native).

clauses
    undoCollection() = boolean::fromBooleanInt(sciLexer_api::getUndoCollection(native)).

clauses
    undoCollection(V) :-
        whenCreated(
            {  :-
                sciLexer_api::setUndoCollection(native, boolean::toBooleanInt(V)),
                if false = V then
                    emptyUndoBuffer()
                end if
            }).

clauses
    beginUndoAction() :-
        sciLexer_api::beginUndoAction(native).

clauses
    endUndoAction() :-
        sciLexer_api::endUndoAction(native).

clauses
    addUndoAction(Token, Flags) :-
        sciLexer_api::addUndoAction(native, Token, Flags).

%  Selection and information
clauses
    lineCount() = sciLexer_api::getLineCount(native).

clauses
    firstVisibleLine(LineDisplay) :-
        sciLexer_api::setFirstVisibleLine(native, fromLineNumber(LineDisplay)).

clauses
    firstVisibleLine() = toLineNumber(sciLexer_api::getFirstVisibleLine(native)).

clauses
    linesOnScreen() = sciLexer_api::linesOnScreen(native).

clauses
    isModified() :-
        b_false <> sciLexer_api::getModify(native).

clauses
    setSel(AnchorPos, CurrentPos) :-
        sciLexer_api::setSel(native, AnchorPos, CurrentPos).

clauses
    gotoPos(Pos) :-
        sciLexer_api::gotoPos(native, Pos).

clauses
    gotoLine(Line) :-
        sciLexer_api::gotoLine(native, fromLineNumber(Line)).

clauses
    currentPos(Pos) :-
        sciLexer_api::setCurrentPos(native, Pos).

clauses
    currentPos() = sciLexer_api::getCurrentPos(native).

clauses
    anchor(Pos) :-
        sciLexer_api::setAnchor(native, Pos).

clauses
    anchor() = sciLexer_api::getAnchor(native).

clauses
    selectionStart(Pos) :-
        sciLexer_api::setSelectionStart(native, Pos).

clauses
    selectionStart() = if isErroneous(native) then 0 else sciLexer_api::getSelectionStart(native) end if.

clauses
    selectionEnd(Pos) :-
        sciLexer_api::setSelectionEnd(native, Pos).

clauses
    selectionEnd() = if isErroneous(native) then 0 else sciLexer_api::getSelectionEnd(native) end if.

clauses
    setEmptySelection(Pos) :-
        sciLexer_api::setEmptySelection(native, Pos).

clauses
    selectAll() :-
        sciLexer_api::selectAll(native).

clauses
    lineFromPosition(Pos) = toLineNumber(sciLexer_api::lineFromPosition(native, Pos)).

clauses
    positionFromLine(Line) = sciLexer_api::positionFromLine(native, fromLineNumber(Line)).

clauses
    getLineEndPosition(Line) = sciLexer_api::getLineEndPosition(native, fromLineNumber(Line)).

clauses
    lineLength(Line) = sciLexer_api::lineLength(native, fromLineNumber(Line)).

clauses
    selText() = getTextRange(selectionStart, selectionEnd).

clauses
    selTextMultiple() = string8::fromUtf8(Text8) :-
        L = sciLexer_api::getSelText(native, string8::null),
        Text8 = string8::create(L),
        _ = sciLexer_api::getSelText(native, Text8).

clauses
    curLine() = string8::fromUtf8(Text8) :-
        L = sciLexer_api::getCurLine(native, 0, string8::null),
        Text8 = string8::create(L),
        _ = sciLexer_api::getCurLine(native, L, Text8).

clauses
    selectionIsRectangle() :-
        b_false <> sciLexer_api::selectionIsRectangle(native).

clauses
    selectionMode(Mode) :-
        sciLexer_api::setSelectionMode(native, Mode).

clauses
    selectionMode() = sciLexer_api::getSelectionMode(native).

clauses
    tryGetLineSelStartPosition(Line) = Pos :-
        Pos = sciLexer_api::getLineSelStartPosition(native, fromLineNumber(Line)),
        sciLexer_native::invalid_position <> Pos.

clauses
    tryGetLineSelEndPosition(Line) = Pos :-
        Pos = sciLexer_api::getLineSelEndPosition(native, fromLineNumber(Line)),
        sciLexer_native::invalid_position <> Pos.

clauses
    moveCaretInsideView() :-
        sciLexer_api::moveCaretInsideView(native).

clauses
    wordEndPosition(Position, OnlyWordCharacters) = sciLexer_api::wordEndPosition(native, Position, boolean::toBooleanInt(OnlyWordCharacters)).

clauses
    wordStartPosition(Position, OnlyWordCharacters) = sciLexer_api::wordStartPosition(native, Position, boolean::toBooleanInt(OnlyWordCharacters)).

clauses
    adjustPositionStart(Position) = sciLexer_api::positionBefore(native, Position + 1).

clauses
    adjustPositionNext(Position) = if Position = 0 then 0 else sciLexer_api::positionAfter(native, Position - 1) end if.

clauses
    tryPositionBefore(Position) = sciLexer_api::positionBefore(native, Position) :-
        0 < Position.

clauses
    tryPositionAfter(Position) = After :-
        Position >= 0,
        After = sciLexer_api::positionAfter(native, Position),
        After > Position.

clauses
    positionRelative(Position, Offset) = sciLexer_api::positionRelative(native, Position, Offset).

clauses
    countCharacters(StartPos, EndPos) = sciLexer_api::countCharacters(native, StartPos, EndPos).

clauses
    textWidth(StyleNumber, Text) = textWidth_utf8(StyleNumber, string8::toUtf8(Text)).

clauses
    textWidth_utf8(StyleNumber, Text8) = sciLexer_api::textWidth(native, StyleNumber, Text8).

clauses
    textHeight(Line) = sciLexer_api::textHeight(native, fromLineNumber(Line)).

clauses
    getColumn(Pos) = toLinePosition(sciLexer_api::getColumn(native, Pos)).

clauses
    findColumn(Line, Column) = sciLexer_api::findColumn(native, fromLineNumber(Line), fromLinePosition(Column)).

clauses
    positionFromPoint(X, Y) = sciLexer_api::positionFromPoint(native, X, Y).

clauses
    tryPositionFromPointClose(X, Y) = Pos :-
        Pos = sciLexer_api::positionFromPointClose(native, X, Y),
        -1 <> Pos.

clauses
    charPositionFromPoint(X, Y) = sciLexer_api::charPositionFromPoint(native, X, Y).

clauses
    tryCharPositionFromPointClose(X, Y) = Pos :-
        Pos = sciLexer_api::charPositionFromPointClose(native, X, Y),
        -1 <> Pos.

clauses
    pointXFromPosition(Pos) = sciLexer_api::pointXFromPosition(native, Pos).

clauses
    pointYFromPosition(Pos) = sciLexer_api::pointYFromPosition(native, Pos).

clauses
    hideSelection(Hide) :-
        sciLexer_api::hideSelection(native, boolean::toBooleanInt(Hide)).
        %  The normal state is to make the selection visible by drawing it as set by SCI_SETSELFORE and SCI_SETSELBACK. However, if you hide
        %  the selection, it is drawn as normal text.

clauses
    chooseCaretX() :-
        sciLexer_api::chooseCaretX(native).
        %  Scintilla remembers the x value of the last position horizontally moved to explicitly by the user and this value is then used when moving
        %  vertically such as by using the up and down keys. This message setS the current x position of the caret as the remembered value.

clauses
    moveSelectedLinesUp() :-
        sciLexer_api::moveSelectedLinesUp(native).
        %  SCI_MOVESELECTEDLINESUP
        %  Move the selected lines up one line, shifting the line above after the selection. The selection will be automatically extended to the
        %  beginning of the selection's first line and the end of the seletion's last line. If nothing was selected, the line the cursor is currently at will
        %  be selected.

clauses
    moveSelectedLinesDown() :-
        sciLexer_api::moveSelectedLinesDown(native).
        %  SCI_MOVESELECTEDLINESDOWN
        %  Move the selected lines down one line, shifting the line below before the selection. The selection will be automatically extended to the
        %  beginning of the selection's first line and the end of the seletion's last line. If nothing was selected, the line the cursor is currently at will
        %  be selected.

%  Multiple Selection and Virtual Space
%  There may be multiple selections active at one time. More selections are made by holding down the Ctrl key while dragging with the
%  mouse. The most recent selection is the main selection and determines which part of the document is shown automatically. Any
%  selection apart from the main selection is called an additional selection. The calls in the previous section operate on the main selection.
%  There is always at least one selection.
%  Rectangular selections are handled as multiple selections although the original rectangular range is remembered so that subsequent
%  operations may be handled differently for rectangular selections. For example, pasting a rectangular selection places each piece in a
%  vertical column.
%  Virtual space is space beyond the end of each line. The caret may be moved into virtual space but no real space will be added to the
%  document until there is some text typed or some other text insertion command is useD.
%  When discontiguous selections are copied to the clipboard, each selection is added to the clipboard text in order with no delimiting
%  characters. For rectangular selections the document's line end is added after each line's text. Rectangular selections are always copied
%  from top line to bottom, not in the in order of selection.Virtual space is not copied.
clauses
    multipleSelection(MultipleSelection) :-
        C = currentPos,
        A = anchor,
        sciLexer_api::setMultipleSelection(native, boolean::toBooleanInt(MultipleSelection)),
        setSelection(C, A).

clauses
    multipleSelection() = boolean::fromBooleanInt(sciLexer_api::getMultipleSelection(native)).
        %  Enable or disable multiple selection.

clauses
    additionalSelectionTyping(AdditionalSelectionTyping) :-
        sciLexer_api::setAdditionalSelectionTyping(native, boolean::toBooleanInt(AdditionalSelectionTyping)).

clauses
    additionalSelectionTyping() = boolean::fromBooleanInt(sciLexer_api::getAdditionalSelectionTyping(native)).
        %  Whether typing, backspace, or delete works with multiple selections simultaneously.

clauses
    multiPaste(MultiPaste) :-
        sciLexer_api::setMultiPaste(native, MultiPaste).

clauses
    multiPaste() = sciLexer_api::getMultiPaste(native).
        %  When pasting into multiple selections, the pasted text can go into just the main selection with SC_MULTIPASTE_ONCE=0 or into each
        %  selection with SC_MULTIPASTE_EACH=1. SC_MULTIPASTE_ONCE is the default.

clauses
    virtualSpaceOptions(VirtualSpace) :-
        sciLexer_api::setVirtualSpaceOptions(native, VirtualSpace).

clauses
    virtualSpaceOptions() = sciLexer_api::getVirtualSpaceOptions(native).
        %  Virtual space can be enabled or disabled for rectangular selections or in other circumstances or in both. There are two bit flags
        %  SCVS_RECTANGULARSELECTION=1 and SCVS_USERACCESSIBLE=2 which can be set independently. SCVS_NONE=0, the default,
        %  disables all use of virtual space.

clauses
    selections() = sciLexer_api::getSelections(native).
        %  Return the number of selections currently active.

clauses
    clearSelections() :-
        sciLexer_api::clearSelections(native).
        %  Set a single empty selection at 0 as the only selection.

clauses
    setSelection(Caret, Anchor) :-
        sciLexer_api::setSelection(native, Caret, Anchor).
        %  Set a single selection from anchor to caret as the only selection.

clauses
    addSelection(Caret, Anchor) :-
        sciLexer_api::addSelection(native, Caret, Anchor).
        %  Add a new selection from anchor to caret as the main selection retaining all other selections as additional selections. Since there is
        %  always at least one selection, to set a list of selections, the first selection should be added with SCI_SETSELECTION and later selections
        %  added with SCI_ADDSELECTION

clauses
    mainSelection(Selection) :-
        sciLexer_api::setMainSelection(native, Selection).

clauses
    mainSelection() = sciLexer_api::getMainSelection(native).
        %  One of the selections is the main selection which is used to determine what range of text is automatically visible. The main selection
        %  may be displayed in different colours or with a differently styled caret. Only an already existing selection can be made main.

clauses
    setSelectionNCaret(Selection, Pos) :-
        sciLexer_api::setSelectionNCaret(native, Selection, Pos).

clauses
    getSelectionNCaret(Selection) = sciLexer_api::getSelectionNCaret(native, Selection).

clauses
    setSelectionNCaretVirtualSpace(Selection, Space) :-
        sciLexer_api::setSelectionNCaretVirtualSpace(native, Selection, Space).

clauses
    getSelectionNCaretVirtualSpace(Selection) = sciLexer_api::getSelectionNCaretVirtualSpace(native, Selection).

clauses
    setSelectionNAnchor(Selection, PosAnchor) :-
        sciLexer_api::setSelectionNAnchor(native, Selection, PosAnchor).

clauses
    getSelectionNAnchor(Selection) = sciLexer_api::getSelectionNAnchor(native, Selection).

clauses
    setSelectionNAnchorVirtualSpace(Selection, Space) :-
        sciLexer_api::setSelectionNAnchorVirtualSpace(native, Selection, Space).

clauses
    getSelectionNAnchorVirtualSpace(Selection) = sciLexer_api::getSelectionNAnchorVirtualSpace(native, Selection).
        %  Set or query the position and amount of virtual space for the caret and anchor of each already existing selection.

clauses
    setSelectionNStart(Selection, Pos) :-
        sciLexer_api::setSelectionNStart(native, Selection, Pos).

clauses
    getSelectionNStart(Selection) = sciLexer_api::getSelectionNStart(native, Selection).

clauses
    setSelectionNEnd(Selection, Pos) :-
        sciLexer_api::setSelectionNEnd(native, Selection, Pos).

clauses
    getSelectionNEnd(Selection) = sciLexer_api::getSelectionNEnd(native, Selection).
        %  Set or query the start and end position of each already existing selection. Mostly of use to query each range for its text.

clauses
    rectangularSelectionCaret(Pos) :-
        sciLexer_api::setRectangularSelectionCaret(native, Pos).

clauses
    rectangularSelectionCaret() = sciLexer_api::getRectangularSelectionCaret(native).

clauses
    rectangularSelectionCaretVirtualSpace(Space) :-
        sciLexer_api::setRectangularSelectionCaretVirtualSpace(native, Space).

clauses
    rectangularSelectionCaretVirtualSpace() = sciLexer_api::getRectangularSelectionCaretVirtualSpace(native).

clauses
    rectangularSelectionAnchor(PosAnchor) :-
        sciLexer_api::setRectangularSelectionAnchor(native, PosAnchor).

clauses
    rectangularSelectionAnchor() = sciLexer_api::getRectangularSelectionAnchor(native).

clauses
    rectangularSelectionAnchorVirtualSpace(Space) :-
        sciLexer_api::setRectangularSelectionAnchorVirtualSpace(native, Space).

clauses
    rectangularSelectionAnchorVirtualSpace() = sciLexer_api::getRectangularSelectionAnchorVirtualSpace(native).
        %  Set or query the position and amount of virtual space for the caret and anchor of the rectangular selection. After setTing the rectangular
        %  selection, this is broken down into multiple selections, one for each line.

clauses
    additionalSelAlpha(Alpha) :-
        sciLexer_api::setAdditionalSelAlpha(native, Alpha).

clauses
    additionalSelAlpha() = sciLexer_api::getAdditionalSelAlpha(native).

clauses
    setAdditionalSelFore(Color) :-
        sciLexer_api::setAdditionalSelFore(native, Color).

clauses
    setAdditionalSelBack(Color) :-
        sciLexer_api::setAdditionalSelBack(native, Color).
        %  Modify the appearence of additional selections so that they can be differentiated from the main selection which has its appearence set
        %  with SCI_SETSELALPHA, SCI_GETSELALPHA, SCI_SETSELFORE, and SCI_SETSELBACK.

clauses
    additionalCaretFore(Color) :-
        sciLexer_api::setAdditionalCaretFore(native, Color).

clauses
    additionalCaretFore() = sciLexer_api::getAdditionalCaretFore(native).

clauses
    additionalCaretsBlink(AdditionalCaretsBlink) :-
        sciLexer_api::setAdditionalCaretsBlink(native, boolean::toBooleanInt(AdditionalCaretsBlink)).

clauses
    additionalCaretsBlink() = boolean::fromBooleanInt(sciLexer_api::getAdditionalCaretsBlink(native)).
        %  Modify the appearence of additional carets so that they can be differentiated from the main caret which has its appearence set with
        %  SCI_SETCARETFORE, SCI_GETCARETFORE, SCI_SETCARETPERIOD, and SCI_GETCARETPERIOD.

clauses
    additionalCaretsVisible(AdditionalCaretsVisible) :-
        sciLexer_api::setAdditionalCaretsVisible(native, boolean::toBooleanInt(AdditionalCaretsVisible)).

clauses
    additionalCaretsVisible() = boolean::fromBooleanInt(sciLexer_api::getAdditionalCaretsVisible(native)).
        %  Determine whether to show additional carets (defaults to true).

clauses
    swapMainAnchorCaret() :-
        sciLexer_api::swapMainAnchorCaret(native).

clauses
    rotateSelection() :-
        sciLexer_api::rotateSelection(native).
        %  These commands may be assignEd to keys to make it possible to manipulate multiple selections. SCI_SWAPMAINANCHORCARET
        %  moves the caret to the opposite end of the main selection. SCI_ROTATESELECTION makes the next selection be the main selection.

%  Scrolling and automatic scrolling
clauses
    lineScroll(Column, Line) :-
        sciLexer_api::lineScroll(native, fromLinePosition(Column), fromLineNumber(Line)).
        %  This will attempt to scroll the display by the number of columns and lines that you specify. Positive line values increase the line number
        %  at the top of the screen (i.e. they move the text upwards as far as the user is concerned), Negative line values do the reverse.

%  The column measure is the width of a space in the default style. Positive values increase the column at the left edge of the view (i.e.
%  they move the text leftwards as far as the user is concerned). Negative values do the reverse.
%  See also: SCI_SETXOFFSET
clauses
    scrollCaret() :-
        sciLexer_api::scrollCaret(native).
        %  If the current position (this is the caret if there is no selection) is not visible, the view is scrolled to make it visible according to the
        %  current caret policy.

clauses
    setXCaretPolicy(CaretPolicy, CaretSlop) :-
        sciLexer_api::setXCaretPolicy(native, CaretPolicy, CaretSlop).

clauses
    setYCaretPolicy(CaretPolicy, CaretSlop) :-
        sciLexer_api::setYCaretPolicy(native, CaretPolicy, CaretSlop).
        %  These set the caret policy. The value of caretPolicy is a combination of CARET_SLOP, CARET_STRICT, CARET_JUMPS and
        %  CARET_EVEN.

%  CARET_SLOP If set, we can define a slop value: caretSlop. This value defines an unwanted zone (UZ) where the caret is... unwanted.
%  This zone is defined as a number of pixels near the vertical margins, and as a number of lines near the horizontal margins. By keeping
%  the caret away from the edges, it is seen within its context. This makes it likely that the identifier that the caret is on can be completely
%  seen, and that the current line is seen with some of the lines following it, which are often dependent on that line.
%  CARET_STRICT If set, the policy set by CARET_SLOP is enforced... strictly. The caret is centred on the display if caretSlop is not set,
%  and cannot go in the UZ if caretSlop is set.
%  CARET_JUMPS If set, the display is moved more energetically so the caret can move in the same direction longer before the policy is
%  applied again. '3UZ' notation is used to indicate three time the size of the UZ as a distance to the margin.
%  CARET_EVEN If not set, instead of having symmetrical UZs, the left and bottom UZs are extended up to right and top UZs respectively.
%  This way, we favour the displaying of useFul information: the beginning of lines, where most code reside, and the lines after the caret,
%  for example, the body of a function.
%  slop strict jumps even Caret can go to the margin On reaching limit (going out of visibility
%  or going into the UZ) display is...
%  0 0 0 0 Yes moved to put caret on top/on right
%  0 0 0 1 Yes moved by one position
%  0 0 1 0 Yes moved to put caret on top/on right
%  0 0 1 1 Yes centred on the caret
%  0 1 - 0 Caret is always on top/on right of display -
%  0 1 - 1 No, caret is always centred -
%  1 0 0 0 Yes moved to put caret out of the asymmetrical UZ
%  1 0 0 1 Yes moved to put caret out of the UZ
%  1 0 1 0 Yes moved to put caret at 3UZ of the top or right margin
%  1 0 1 1 Yes moved to put caret at 3UZ of the margin
%  1 1 - 0 Caret is always at UZ of top/right margin -
%  1 1 0 1 No, kept out of UZ moved by one position
%  1 1 1 0 No, kept out of UZ moved to put caret at 3UZ of the margin
clauses
    setVisiblePolicy(CaretPolicy, CaretSlop) :-
        sciLexer_api::setVisiblePolicy(native, CaretPolicy, CaretSlop).
        %  This determines how the vertical positioning is determined when SCI_ENSUREVISIBLEENFORCEPOLICY is called. It takes VISIBLE_SLOP
        %  and VISIBLE_STRICT flags for the policy parameter. It is similar in operation to SCI_SETYCARETPOLICY(int caretPolicy, int caretSlop).

clauses
    hScrollbar(Visible) :-
        sciLexer_api::setHScrollbar(native, boolean::toBooleanInt(Visible)).

clauses
    hScrollbar() = boolean::fromBooleanInt(sciLexer_api::getHScrollbar(native)).
        %  The horizontal scroll bar is only displayed if it is needed for the assumed width. If you never wish to see it, call
        %  SCI_SETHSCROLLBAR(0). Use SCI_SETHSCROLLBAR(1) to enable it again. SCI_GETHSCROLLBAR returns the current state. The
        %  default state is to display it when needed.

%  See also: SCI_SETSCROLLWIDTH.
clauses
    vScrollbar(Visible) :-
        sciLexer_api::setVScrollbar(native, boolean::toBooleanInt(Visible)).

clauses
    vScrollbar() = boolean::fromBooleanInt(sciLexer_api::getVScrollbar(native)).
        %  By default, the vertical scroll bar is always displayed when required. You can choose to hide or show it with SCI_SETVSCROLLBAR and
        %  get the current state with SCI_GETVSCROLLBAR.

clauses
    xOffset(XOffset) :-
        sciLexer_api::setXOffset(native, XOffset).

clauses
    xOffset() = sciLexer_api::getXOffset(native).
        %  The xOffset is the horizontal scroll position in pixels of the start of the text view. A value of 0 is the normal position with the first text
        %  column visible at the left of the view.

%  See also: SCI_LINESCROLL
clauses
    scrollWidth(PixelWidth) :-
        sciLexer_api::setScrollWidth(native, PixelWidth).

clauses
    scrollWidth() = sciLexer_api::getScrollWidth(native).
        %  For performance, Scintilla does not measure the display width of the document to determine the properties of the horizontal scroll bar.
        %  Instead, an assumed width is useD. These messages set and get the document width in pixels assumed by Scintilla. The default value is
        %  2000. To ensure the width of the currently visible lines can be scrolled use SCI_SETSCROLLWIDTHTRACKING

clauses
    scrollWidthTracking(Tracking) :-
        sciLexer_api::setScrollWidthTracking(native, boolean::toBooleanInt(Tracking)).

clauses
    scrollWidthTracking() = boolean::fromBooleanInt(sciLexer_api::getScrollWidthTracking(native)).
        %  If scroll width tracking is enabled then the scroll width is adjusted to ensure that all of the lines currently displayed can be completely
        %  scrolled. This mode never adjusts the scroll width to be narrower.

clauses
    endatLastLine(EndAtLastLine) :-
        sciLexer_api::setEndatLastLine(native, boolean::toBooleanInt(EndAtLastLine)).

clauses
    endatLastLine() = boolean::fromBooleanInt(sciLexer_api::getEndatLastLine(native)).
        %  SCI_SETENDATLASTLINE setS the scroll range so that maximum scroll position has the last line at the bottom of the view (default).
        %  SetTing this to false allows scrolling one page below the last line.

%  White space
clauses
    viewWs(WsMode) :-
        sciLexer_api::setViewWs(native, WsMode).

clauses
    viewWs() = sciLexer_api::getViewWs(native).
        %  White space can be made visible which may be useFul for languages in which white space is significant, such as Python. Space
        %  characters appear as small centred dots and tab characters as light arrows pointing to the right. There are also ways to control the
        %  display of end of line characters. The two messages set and get the white space display mode. The wsMode argument can be one of:

%  SCWS_INVISIBLE 0 The normal display mode with white space displayed as an empty background colour.
%  SCWS_VISIBLEALWAYS 1 White space characters are drawn as dots and arrows,
%  SCWS_VISIBLEAFTERINDENT 2 White space used for indentation is displayed normally but after the first visible character, it is shown
%  as dots and arrows.
%  The effect of using any other wsMode value is undefined.
clauses
    setWhiteSpacefore(UseWhiteSpaceforeColor, Color) :-
        sciLexer_api::setWhiteSpacefore(native, boolean::toBooleanInt(UseWhiteSpaceforeColor), Color).

clauses
    setWhiteSpaceBack(UseWhiteSpaceBackColor, Color) :-
        sciLexer_api::setWhiteSpaceBack(native, boolean::toBooleanInt(UseWhiteSpaceBackColor), Color).
        %  By default, the colour of visible white space is determined by the lexer in use. The foreground and/or background colour of all visible
        %  white space can be set globally, overriding the lexer's colours with SCI_SETWHITESPACefore and SCI_SETWHITESPACEBACK.

clauses
    whiteSpaceSize(Size) :-
        sciLexer_api::setWhiteSpaceSize(native, Size).

clauses
    whiteSpaceSize() = sciLexer_api::getWhiteSpaceSize(native).
        %  SCI_SETWHITESPACESIZE setS the size of the dots used for mark space characters. The SCI_GETWHITESPACESIZE message
        %  retrieves the current size.

clauses
    extraAscent(ExtraAscent) :-
        sciLexer_api::setExtraAscent(native, ExtraAscent).

clauses
    extraAscent() = sciLexer_api::getExtraAscent(native).

clauses
    extraDescent(ExtraDescent) :-
        sciLexer_api::setExtraDescent(native, ExtraDescent).

clauses
    extraDescent() = sciLexer_api::getExtraDescent(native).
        %  Text is drawn with the base of each character on a 'baseline'. The height of a line is found from the maximum that any style extends
        %  above the baseline (its 'ascent'), added to the maximum that any style extends below the baseline (its 'descent'). Space may be added
        %  to the maximum ascent (SCI_SETEXTRAASCENT) and the maximum descent (SCI_SETEXTRADESCENT) to allow for more space
        %  between lines. This may done to make the text easier to read or to accomodate underlines or highLights.

%  Cursor
clauses
    cursor(CurType) :-
        sciLexer_api::setCursor(native, CurType).

clauses
    cursor() = sciLexer_api::getCursor(native).
        %  The cursor is normally chosen in a context sensitive way, so it will be different over the margin than when over the text. When
        %  performing a slow action, you may wish to change to a wait cursor. You set the cursor type with SCI_SETCURSOR. The curType
        %  argument can be:

%  SC_CURSORNORMAL -1 The normal cursor is displayed.
%  SC_CURSORWAIT  4 The wait cursor is displayed when the mouse is over or owned by the Scintilla window.
%  Cursor values 1 through 7 have defined cursors, but only SC_CURSORWAIT is useFully controllable. Other values of curType cause a
%  pointer to be displayed. The SCI_GETCURSOR message returns the last cursor type you set, or SC_CURSORNORMAL (-1) if you have
%  not set a cursor type.
%  Mouse capture
clauses
    mouseDownCaptures(Captures) :-
        sciLexer_api::setMouseDownCaptures(native, boolean::toBooleanInt(Captures)).

clauses
    mouseDownCaptures() = boolean::fromBooleanInt(sciLexer_api::getMouseDownCaptures(native)).
        %  When the mouse is pressed inside Scintilla, it is captured so future mouse movement events are sent to Scintilla. This behavior may be
        %  turned off with SCI_SETMOUSEDOWNCAPTURES(0).

%  Line endings
%  Scintilla can interpret any of the three major line end conventions, Macintosh (\r), Unix (\n) and CP/M / DOS / Windows (\r\n). When
%  the user presses the Enter key, one of these line end strings is inserted into the buffer. The default is \r\n in Windows and \n in Unix,
%  but this can be changed with the SCI_SET_eolMODE message. You can also convert the entire document to one of these line endings
%  with SCI_CONVERT_eolS. Finally, you can choose to display the line endings with SCI_SETVIEW_eol.
clauses
    eolMode(EolMode) :-
        sciLexer_api::setEolMode(native, EolMode).

clauses
    eolMode() = sciLexer_api::getEolMode(native).
        %  SCI_SET_eolMODE setS the characters that are added into the document when the user presses the Enter key. You can set eolMode to
        %  one of SC__eol_CRLF (0), SC__eol_CR (1), or SC__eol_LF (2). The SCI_GET_eolMODE message retrieves the current state.

clauses
    convertEols(EolMode) :-
        sciLexer_api::convertEols(native, EolMode).
        %  This message changes all the end of line characters in the document to match eolMode. Valid values are: SC__eol_CRLF (0),
        %  SC__eol_CR (1), or SC__eol_LF (2).

clauses
    viewEol(Visible) :-
        sciLexer_api::setViewEol(native, boolean::toBooleanInt(Visible)).

clauses
    viewEol() = boolean::fromBooleanInt(sciLexer_api::getViewEol(native)).
        %  Normally, the end of line characters are hidden, but SCI_SETVIEW_eol allows you to display (or hide) them by setTing visible true (or
        %  false). The visible rendering of the end of line characters is similar to (CR), (LF), or (CR)(LF). SCI_GETVIEW_eol returns the current
        %  state.

%  Styling
%  The styling messages allow you to assign styles to text. The standard Scintilla setTings divide the 8 style bits available for each
%  character into 5 bits (0 to 4 = sciLexer_api::styles 0 to 31) that set a style and three bits (5 to 7) that define indicators. You can change the balance
%  between styles and indicators with SCI_SETSTYLEBITS. If your styling needs can be met by one of the standard lexers, or if you can
%  write your own, then a lexer is probably the easiest way to style your document. If you choose to use the container to do the styling
%  you can use the SCI_SETLEXER command to select SCLEX_CONTAINER, in which case the container is sent a SCN_STYLENEEDED
%  notification each time text needs styling for display. As another alternative, you might use idle time to style the document. Even if you
%  use a lexer, you might use the styling commands to mark errors detected by a compiler. The following commands can be useD.
clauses
    endStyled() = sciLexer_api::getEndStyled(native).
        %  Scintilla keeps a record of the last character that is likely to be styled correctly. This is moved forwards when characters after it are
        %  styled and moved backwards if changes are made to the text of the document before it. Before drawing text, this position is checked to
        %  see if any styling is needed and, if so, a SCN_STYLENEEDED notification message is sent to the container. The container can send
        %  SCI_GETENDSTYLED to work out where it needs to start styling. Scintilla will always ask to style whole lines.

clauses
    startStyling(Pos, Mask) :-
        sciLexer_api::startStyling(native, Pos, Mask).
        %  This prepares for styling by setTing the styling position pos to start at and a mask indicating which bits of the style bytes can be set. The
        %  mask allows styling to occur over several passes, with, for example, basic styling done on an initial pass to ensure that the text of the
        %  code is seen quickly and correctly, and then a second slower pass, detecting syntax errors and using indicators to show where these
        %  are. For example, with the standard setTings of 5 style bits and 3 indicator bits, you would use a mask value of 31 (0x1f) if you were
        %  setTing text styles and did not want to change the indicators. After SCI_STARTSTYLING, send multiple SCI_SETSTYLING messages for
        %  each lexical entity to style.

clauses
    setStyling(Length, Style) :-
        sciLexer_api::setStyling(native, Length, Style).
        %  This message setS the style of length characters starting at the styling position and then increases the styling position by length, ready
        %  for the next call. If sCell is the style byte, the operation is:
        %  if ((sCell & mask) != sciLexer_api::style) sCell = sciLexer_api::(sCell & ~mask) | (style & mask);

clauses
    setStylingeX(Length, Styles) :-
        sciLexer_api::setStylingeX(native, Length, Styles).
        %  As an alternative to SCI_SETSTYLING, which applies the same style to each byte, you can use this message which specifies the styles
        %  for each of length bytes from the styling position and then increases the styling position by length, ready for the next call. The length
        %  styling bytes pointed at by styles should not contain any bits not set in mask.

clauses
    setLineState(Line, Value) :-
        sciLexer_api::setLineState(native, fromLineNumber(Line), Value).

clauses
    getLineState(Line) = sciLexer_api::getLineState(native, fromLineNumber(Line)).
        %  As well as the 8 bits of lexical state stored for each character there is also an integer stored for each line. This can be used for longer
        %  lived parse states such as what the current scripting language is in an ASP page. Use SCI_SETLINESTATE to set the integer value and
        %  SCI_GETLINESTATE to get the value. Changing the value produces a SC_MOD_CHANGELINESTATE notification.

clauses
    maxLineState() = toLineNumber(sciLexer_api::getMaxLineState(native)).
        %  This returns the last line that has any line state.

%  Style definition
%  While the style setTing messages mentioned above change the style numbers associated with text,
% these messages define how those
%  style numbers are interpreted visually. There are 256 lexer styles that can be set,
% numbered 0 to STYLE_MAX (255). Unless you use
%  SCI_SETSTYLEBITS to change the number of style bits, styles 0 to 31 are used to set the
% text attributes. There are also some
%  predefined numbered styles starting at 32, The following STYLE_* constants are defined.
%  STYLE_DEFAULT 32 This style defines the attributes that all styles receive when the
% SCI_STYLECLEARALL message is useD.
%  STYLE_LINENUMBER 33 This style setS the attributes of the text used to display line numbers in a
% line number margin. The background
%  colour set for this style also setS the background colour for all margins that do not have any
% folding mask bits set. That is, any margin
%  for which mask & SC_MASK_FOLDERS is 0. See SCI_SETMARGINMASKN for more about masks.
%  STYLE_BRACELIGHT 34 This style setS the attributes used when highLighting braces with the
%  SCI_BRACEHIGHLIGHT message and when
%  highLighting the corresponding indentation with SCI_SETHIGHLIGHTGUIDE.
%  STYLE_BRACEBAD 35 This style setS the display attributes used when marking an unmatched
% brace with the SCI_BRACEBADLIGHT
%  message.
%  STYLE_CONTROLCHAR 36 This style setS the font used when drawing control characters.
% Only the font, size, bold, italics, and character
%  set attributes are used and not the colour attributes. See also: SCI_SETCONTROLCHARSYMBOL.
%  STYLE_INDENTGUIDE 37 This style setS the foreground and background colours used when drawing
% the indentation guides.
%  STYLE_CALLTIP 38 Call tips normally use the font attributes defined by STYLE_DEFAULT.
%  Use of SCI_CALLTIPUSESTYLE causeS call tips
%  to use this style instead. Only the font face name, font size, foreground and background colours and character set attributes are useD.
%  STYLE_LASTPREDEFINED 39 To make it easier for client code to discover the range of styles that are predefined, this is set to the style
%  number of the last predefined style. This is currently set to 39 and the last style with an identifier is 38, which reserves space for one
%  future predefined style.
%  STYLE_MAX 255 This is not a style but is the number of the maximum style that can be set. Styles between STYLE_LASTPREDEFINED
%  and STYLE_MAX would be appropriate if you used SCI_SETSTYLEBITS to set more than 5 style bits.
%  For each style you can set the font name, size and use of bold, italic and underline,
% foreground and background colour and the
%  character set. You can also choose to hide text with a given style, display all characters as upper
%  or lower case and fill from the last
%  character on a line to the end of the line (for embedded languages).
% There is also an experimental attribute to make text read-only.
%  It is entirely up to you how you use styles. If you want to use syntax colouring you might use style 0
% for white space, style 1 for
%  numbers, style 2 for keywords, style 3 for strings, style 4 for preprocessor,
% style 5 for operators, and so on.
clauses
    styleResetDefault() :-
        sciLexer_api::styleResetDefault(native).
        %  This message resets STYLE_DEFAULT to its state when Scintilla was initialised.

clauses
    styleClearAll() :-
        sciLexer_api::styleClearAll(native).
        %  This message setS all styles to have the same attributes as STYLE_DEFAULT. If you are setTing up Scintilla for syntax colouring, it is
        %  likely that the lexical styles you set will be very similar. One way to set the styles is to:
        %  1. Set STYLE_DEFAULT to the common features of all styles.
        %  2. Use SCI_STYLECLEARALL to copy this to all styles.
        %  3. Set the style attributes that make your lexical styles different.

clauses
    styleSetFont(StyleNumber, FontName) :-
        sciLexer_api::styleSetFont(native, StyleNumber, string8::toUtf8(FontName)).

clauses
    styleGetFont(StyleNumber) = string8::fromUtf8(FontName8) :-
        FontName8 = string8::create(string8_defaultLength),
        sciLexer_api::styleGetFont(native, StyleNumber, FontName8).

clauses
    styleSetSize(StyleNumber, SizeInPoints) :-
        sciLexer_api::styleSetSize(native, StyleNumber, SizeInPoints).

clauses
    styleGetSize(StyleNumber) = sciLexer_api::styleGetSize(native, StyleNumber).

clauses
    styleSetSizeFractional(StyleNumber, SizeInPoints) :-
        sciLexer_api::styleSetSizeFractional(native, StyleNumber, SizeInPoints).

clauses
    styleGetSizeFractional(StyleNumber) = sciLexer_api::styleGetSizeFractional(native, StyleNumber).

clauses
    styleSetBold(StyleNumber, Bold) :-
        sciLexer_api::styleSetBold(native, StyleNumber, boolean::toBooleanInt(Bold)).

clauses
    styleGetBold(StyleNumber) = boolean::fromBooleanInt(sciLexer_api::styleGetBold(native, StyleNumber)).

clauses
    styleSetWeight(StyleNumber, Weight) :-
        sciLexer_api::styleSetWeight(native, StyleNumber, Weight).

clauses
    styleGetWeight(StyleNumber) = sciLexer_api::styleGetWeight(native, StyleNumber).

clauses
    styleSetItalic(StyleNumber, Italic) :-
        sciLexer_api::styleSetItalic(native, StyleNumber, boolean::toBooleanInt(Italic)).

clauses
    styleGetItalic(StyleNumber) = boolean::fromBooleanInt(sciLexer_api::styleGetItalic(native, StyleNumber)).
        %  These messages (plus SCI_STYLESETCHARACTERSET) set the font attributes that are used to match the fonts you request to those
        %  available. The fontName is a zero terminated string holding the name of a font. Under Windows, only the first 32 characters of the name
        %  are used and the name is not case sensitive. For internal caching, Scintilla tracks fonts by name and does care about the casing of font
        %  names, so please be consistent. On GTK+ 2.x, either GDK or Pango can be used to display text. Pango antialiases text, works well with
        %  Unicode and is better supported in recent versions of GTK+ but GDK is faster. Prepend a '!' character to the font name to use Pango.

clauses
    styleSetUnderline(StyleNumber, UnderLine) :-
        sciLexer_api::styleSetUnderline(native, StyleNumber, boolean::toBooleanInt(UnderLine)).

clauses
    styleGetUnderline(StyleNumber) = boolean::fromBooleanInt(sciLexer_api::styleGetUnderline(native, StyleNumber)).
        %  You can set a style to be underlined. The underline is drawn in the foreground colour. All characters with a style that includes the
        %  underline attribute are underlined, even if they are white space.

clauses
    styleSetFore(StyleNumber, Color) :-
        sciLexer_api::styleSetFore(native, StyleNumber, Color).

clauses
    styleGetFore(StyleNumber) = sciLexer_api::styleGetFore(native, StyleNumber).

clauses
    styleSetBack(StyleNumber, Color) :-
        sciLexer_api::styleSetBack(native, StyleNumber, Color).

clauses
    styleGetBack(StyleNumber) = sciLexer_api::styleGetBack(native, StyleNumber).
        %  Text is drawn in the foreground colour. The space in each character cell that is not occupied by the character is drawn in the
        %  background colour.

clauses
    styleSetEolFilled(StyleNumber, EolFilled) :-
        sciLexer_api::styleSetEolFilled(native, StyleNumber, boolean::toBooleanInt(EolFilled)).

clauses
    styleGetEolFilled(StyleNumber) = boolean::fromBooleanInt(sciLexer_api::styleGetEolFilled(native, StyleNumber)).
        %  If the last character in the line has a style with this attribute set, the remainder of the line up to the right edge of the window is filled
        %  with the background colour set for the last character. This is useFul when a document contains embedded sections in another language
        %  such as HTML pages with embedded JavaScript. By setTing eolFilled to true and a consistent background colour (different from the
        %  background colour set for the HTML styles) to all JavaScript styles then JavaScript sections will be easily distinguished from HTML.

%%%% This editor only uses utf-8
%  You can set a style to use a different character set than the default. The places where such characters setS are likely to be useFul are
%  comments and literal strings. For example, SCI_STYLESETCHARACTERSET(SCE_C_STRING, SC_CHARSET_RUSSIAN) would ensure
%  that strings in Russian would display correctly in C and C++ (SCE_C_STRING is the style number used by the C and C++ lexer to
%  display literal strings; it has the value 6). This feature works differently on Windows and GTK+.
%  The character setS supported on Windows are:
%  SC_CHARSET_ANSI, SC_CHARSET_ARABIC, SC_CHARSET_BALTIC, SC_CHARSET_CHINESEBIG5, SC_CHARSET_DEFAULT,
%  SC_CHARSET_EASTEUROPE, SC_CHARSET_GB2312, SC_CHARSET_GREEK, SC_CHARSET_HANGUL, SC_CHARSET_HEBREW,
%  SC_CHARSET_JOHAB, SC_CHARSET_MAC, SC_CHARSET_OEM, SC_CHARSET_RUSSIAN (code page 1251), SC_CHARSET_SHIFTJIS,
%  SC_CHARSET_SYMBOL, SC_CHARSET_THAI, SC_CHARSET_TURKISH, and SC_CHARSET_VIETNAMESE.
%  The character setS supported on GTK+ are:
%  SC_CHARSET_ANSI, SC_CHARSET_CYRILLIC (code page 1251), SC_CHARSET_EASTEUROPE, SC_CHARSET_GB2312,
%  SC_CHARSET_HANGUL, SC_CHARSET_RUSSIAN (KOI8-R), SC_CHARSET_SHIFTJIS, and SC_CHARSET_8859_15.
clauses
    styleSetCase(StyleNumber, CaseMode) :-
        sciLexer_api::styleSetCase(native, StyleNumber, CaseMode).

clauses
    styleGetCase(StyleNumber) = sciLexer_api::styleGetCase(native, StyleNumber).
        %  The value of caseMode determines how text is displayed. You can set upper case (SC_CASE_UPPER, 1) or lower case
        %  (SC_CASE_LOWER, 2) or display normally (SC_CASE_MIXED, 0). This does not change the stored text, only how it is displayed.

clauses
    styleSetVisible(StyleNumber, Visible) :-
        sciLexer_api::styleSetVisible(native, StyleNumber, boolean::toBooleanInt(Visible)).

clauses
    styleGetVisible(StyleNumber) = boolean::fromBooleanInt(sciLexer_api::styleGetVisible(native, StyleNumber)).
        %  Text is normally visible. However, you can completely hide it by giving it a style with the visible set to 0. This could be used to hide
        %  embedded formatting instructions or hypertext keywords in HTML or XML.

clauses
    styleSetChangeable(StyleNumber, Changeable) :-
        sciLexer_api::styleSetChangeable(native, StyleNumber, boolean::toBooleanInt(Changeable)).

clauses
    styleGetChangeable(StyleNumber) = boolean::fromBooleanInt(sciLexer_api::styleGetChangeable(native, StyleNumber)).
        %  This is an experimental and incompletely implemented style attribute. The default setTing is changeable set true but when set false it
        %  makes text read-only. Currently it only stops the caret from being within not-changeable text and does not yet stop deleting a range
        %  that contains not-changeable text.

clauses
    styleSetHotSpot(StyleNumber, HotSpot) :-
        sciLexer_api::styleSetHotSpot(native, StyleNumber, boolean::toBooleanInt(HotSpot)).

clauses
    styleGetHotSpot(StyleNumber) = boolean::fromBooleanInt(sciLexer_api::styleGetHotSpot(native, StyleNumber)).
        %  This style is used to mark ranges of text that can detect mouse clicks. The cursor changes to a hand over hotSpots, and the foreground,
        %  and background colours may change and an underline appear to indicate that these areas are sensitive to clicking. This may be used to
        %  allow hyperlinks to other documents.

%  Caret, selection, and hotSpot styles
%  The selection is shown by changing the foreground and/or background colours. If one of these is not set then that attribute is not
%  changed for the selection. The default is to show the selection by changing the background to light gray and leaving the foreground the
%  same as when it was not selected. When there is no selection, the current insertion point is marked by the text caret. This is a vertical
%  line that is normally blinking on and off to attract the users attention.
clauses
    setSelFore(UseSelectionForeColor, Color) :-
        sciLexer_api::setSelFore(native, boolean::toBooleanInt(UseSelectionForeColor), Color).

clauses
    setSelBack(UseSelectionBackColor, Color) :-
        sciLexer_api::setSelBack(native, boolean::toBooleanInt(UseSelectionBackColor), Color).
        %  You can choose to override the default selection colouring with these two messages. The colour you provide is used if you set
        %  useSelection*Colour to true. If it is set to false, the default styled colouring is used and the colour argument has no effect.

clauses
    selAlpha(Alpha) :-
        sciLexer_api::setSelAlpha(native, Alpha).

clauses
    selAlpha() = sciLexer_api::getSelAlpha(native).
        %  The selection can be drawn translucently in the selection background colour by setTing an alpha value.

clauses
    selEolFilled(Filled) :-
        sciLexer_api::setSelEolFilled(native, boolean::toBooleanInt(Filled)).

clauses
    selEolFilled() = boolean::fromBooleanInt(sciLexer_api::getSelEolFilled(native)).
        %  The selection can be drawn up to the right hand border by setTing this property.

clauses
    caretFore(Color) :-
        sciLexer_api::setCaretFore(native, Color).

clauses
    caretFore() = sciLexer_api::getCaretFore(native).
        %  The colour of the caret can be set with SCI_SETCARETFORE and retrieved with SCI_GETCARETFORE.

clauses
    caretLineVisible(Show) :-
        sciLexer_api::setCaretLineVisible(native, boolean::toBooleanInt(Show)).

clauses
    caretLineVisible() = boolean::fromBooleanInt(sciLexer_api::getCaretLineVisible(native)).

clauses
    caretLineBack(Color) :-
        sciLexer_api::setCaretLineBack(native, Color).

clauses
    caretLineBack() = sciLexer_api::getCaretLineBack(native).

clauses
    caretLineBackAlpha(Alpha) :-
        sciLexer_api::setCaretLineBackAlpha(native, Alpha).

clauses
    caretLineBackAlpha() = sciLexer_api::getCaretLineBackAlpha(native).
        %  You can choose to make the background colour of the line containing the caret different with
        % these messages. To do this, set the
        %  desired background colour with SCI_SETCARETLINEBACK,
        % then use SCI_SETCARETLINEVISIBLE(true) to enable the effect. You can
        %  cancel the effect with SCI_SETCARETLINEVISIBLE(false).
        % The two SCI_GETCARET* functions return the state and the colour. This form
        %  of background colouring has highest priority when a line has markers that would otherwise
        % change the background colour. The caret
        %  line may also be drawn translucently which allows other background colours to show through.
        % This is done by setTing the alpha
        %  (translucency) value by calling SCI_SETCARETLINEBACKALPHA.
        % When the alpha is not SC_ALPHA_NOALPHA, the caret line is drawn
        %  after all other features so will affect the colour of all other features.

clauses
    caretPeriod(Milliseconds) :-
        sciLexer_api::setCaretPeriod(native, Milliseconds).

clauses
    caretPeriod() = sciLexer_api::getCaretPeriod(native).
        %  The rate at which the caret blinks can be set with SCI_SETCARETPERIOD which determines the time in milliseconds that the caret is
        %  visible or Invisible before changing state. SetTing the period to 0 stops the caret blinking. The default value is 500 milliseconds.
        %  SCI_GETCARETPERIOD returns the current setTing.

clauses
    caretStyle(Style) :-
        sciLexer_api::setCaretStyle(native, Style).

clauses
    caretStyle() = sciLexer_api::getCaretStyle(native).
        %  The style of the caret can be set with SCI_SETCARETSTYLE to be a line caret (CARETSTYLE_LINE=1), a block caret
        %  (CARETSTYLE_BLOCK=2) or to not draw at all (CARETSTYLE_INVISIBLE=0). The default value is the line caret (CARETSTYLE_LINE=1).
        %  You can determine the current caret style setTing using SCI_GETCARETSTYLE.

%  The block character draws most combining and multibyte character sequences successfully, though some fonts like Thai Fonts (and
%  possibly others) can someTimes appear strange when the cursor is positioned at these characters, which may result in only drawing a
%  part of the cursor character sequence. This is most notable on Windows platforms.
clauses
    caretWidth(Pixels) :-
        sciLexer_api::setCaretWidth(native, Pixels).

clauses
    caretWidth() = sciLexer_api::getCaretWidth(native).
        %  The width of the line caret can be set with SCI_SETCARETWIDTH to a value of 0, 1, 2 or 3 pixels. The default width is 1 pixel. You can
        %  read back the current width with SCI_GETCARETWIDTH. A width of 0 makes the caret Invisible (added at version 1.50), similar to
        %  setTing the caret style to CARETSTYLE_INVISIBLE (though not interchangable). This setTing only affects the width of the cursor when the
        %  cursor style is set to line caret mode, it does not affect the width for a block caret.

clauses
    setHotSpotActivefore(UseHotSpotForeColor, Color) :-
        sciLexer_api::setHotSpotActivefore(native, boolean::toBooleanInt(UseHotSpotForeColor), Color).

clauses
    getHotSpotActivefore() = sciLexer_api::getHotSpotActivefore(native).

clauses
    setHotSpotActiveBack(UseHotSpotBackColor, Color) :-
        sciLexer_api::setHotSpotActiveBack(native, boolean::toBooleanInt(UseHotSpotBackColor), Color).

clauses
    getHotSpotActiveBack() = sciLexer_api::getHotSpotActiveBack(native).

clauses
    hotSpotActiveUnderline(UnderLine) :-
        sciLexer_api::setHotSpotActiveUnderline(native, boolean::toBooleanInt(UnderLine)).

clauses
    hotSpotActiveUnderline() = boolean::fromBooleanInt(sciLexer_api::getHotSpotActiveUnderline(native)).

clauses
    hotSpotSingleLine(SingleLine) :-
        sciLexer_api::setHotSpotSingleLine(native, boolean::toBooleanInt(SingleLine)).

clauses
    hotSpotSingleLine() = boolean::fromBooleanInt(sciLexer_api::getHotSpotSingleLine(native)).
        %  While the cursor hovers over text in a style with the hotSpot attribute set, the default colouring can be modified and an underline drawn
        %  with these setTings. Single line mode stops a hotSpot from wrapping onto next line.

clauses
    controlCharSymbol(Symbol) :-
        sciLexer_api::setControlCharSymbol(native, Symbol).

clauses
    controlCharSymbol() = sciLexer_api::getControlCharSymbol(native).
        %  By default, Scintilla displays control characters (characters with codes less than 32) in a rounded rectangle as ASCII mnemonics:
        %  "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3",
        %  "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US". These mnemonics come from the early days of
        %  signaling, though some are still used (LF = sciLexer_api::Line Feed, BS = sciLexer_api::Back Space, CR = sciLexer_api::Carriage Return, for example).

%  You can choose to replace these mnemonics by a nominated symbol with an ASCII code in the range 32 to 255. If you set a symbol
%  value less than 32, all control characters are displayed as mnemonics. The symbol you set is rendered in the font of the style set for
%  the character. You can read back the current symbol with the SCI_GETCONTROLCHARSYMBOL message. The default symbol value is 0.
clauses
    caretSticky(UseCaretStickyBehaviour) :-
        sciLexer_api::setCaretSticky(native, UseCaretStickyBehaviour).

clauses
    caretSticky() = sciLexer_api::getCaretSticky(native).

clauses
    toggleCaretSticky() :-
        sciLexer_api::toggleCaretSticky(native).
        %  These messages set, get or toggle the caretSticky setTing which controls when the last position of the caret on the line is saved.

%  When set to SC_CARETSTICKY_OFF (0), the sticky flag is off; all text changes (and all caret position changes) will remember the
%  caret's new horizontal position when moving to different lines. This is the default.
%  When set to SC_CARETSTICKY_ON (1), the sticky flag is on, and the only thing which will cause the editor to remember the horizontal
%  caret position is moving the caret with mouse or keyboard (left/right arrow keys, home/end keys, etc).
%  When set to SC_CARETSTICKY_WHITESPACE (2), the caret acts like mode 0 (sticky off) except under one special case; when space or
%  tab characters are inserted. (Including pasting only space/tabs -- undo, redo, etc. do not exhibit this behavior..).
%  SCI_TOGGLECARETSTICKY switches from SC_CARETSTICKY_ON and SC_CARETSTICKY_WHITESPACE to SC_CARETSTICKY_OFF and
%  from SC_CARETSTICKY_OFF to SC_CARETSTICKY_ON.
%  Margins
%  There may be up to five margins to the left of the text display, plus a gap either side of the text.
%  Each margin can be set to display
%  either symbols or line numbers with SCI_SETMARGINTYPEN.
%  The markers that can be displayed in each margin are set with
%  SCI_SETMARGINMASKN. Any markers not associated with a visible margin will be
%  displayed as changes in background colour in the
%  text. A width in pixels can be set for each margin. Margins with a zero width are ignored completely. You can choose if a mouse click in
%  a margin sends a SCN_MARGINCLICK notification to the container or selects a line of text.
%  The margins are numbered 0 to 4. Using a margin number outside the valid range has no effect.
%  By default, margin 0 is set to display
%  line numbers, but is given a width of 0, so it is hidden. Margin 1 is set to display non-folding symbols and is given a width of 16 pixels,
%  so it is visible. Margin 2 is set to display the folding symbols, but is given a width of 0, so it is hidden. Of course, you can set the
%  margins to be whatever you wish.
%  Styled text margins used to show revision and blame information:
clauses
    setMarginTypeN(Margin, IType) :-
        sciLexer_api::setMarginTypeN(native, Margin, IType).

clauses
    getMarginTypeN(Margin) = sciLexer_api::getMarginTypeN(native, Margin).
        %  These two rOutines set and get the type of a margin.
        %  The margin argument should be 0, 1, 2, 3 or 4. You can use the predefined
        %  constants SC_MARGIN_SYMBOL (0) and SC_MARGIN_NUMBER (1) to set a margin as either
        %  a line number or a symbol margin. A
        %  margin with application defined text may use SC_MARGIN_TEXT (4) or SC_MARGIN_RTEXT (5)
        %  to right justify the text. By convention,
        %  margin 0 is used for line numbers and the next two are used for symbols.
        %  You can also use the constants SC_MARGIN_BACK (2) and
        %  SC_MARGIN_FORE (3) for symbol margins that set their background
        % colour to match the STYLE_DEFAULT background and foreground
        %  colours.

clauses
    setMarginWidthN(Margin, PixelWidth) :-
        sciLexer_api::setMarginWidthN(native, Margin, PixelWidth).

clauses
    getMarginWidthN(Margin) = sciLexer_api::getMarginWidthN(native, Margin).
        %  These rOutines set and get the width of a margin in pixels. A margin with zero width is Invisible. By default, Scintilla setS margin 1 for
        %  symbols with a width of 16 pixels, so this is a reasonable guess if you are not sure what would be appropriate. Line number margins
        %  widths should take into account the number of lines in the document and the line number style. You could use something like
        %  SCI_TEXTWIDTH(STYLE_LINENUMBER, "_99999") to get a suitable width.

clauses
    setMarginMaskN(Margin, Mask) :-
        sciLexer_api::setMarginMaskN(native, Margin, Mask).

clauses
    getMarginMaskN(Margin) = sciLexer_api::getMarginMaskN(native, Margin).
        %  The mask is a 32-bit value. Each bit corresponds to one of 32 logical symbols that can be displayed in a margin that is enabled for
        %  symbols. There is a useFul constant, SC_MASK_FOLDERS (0xFE000000 or -33554432), that is a mask for the 7 logical symbols used to
        %  denote folding. You can assign a wide range of symbols and colours to each of the 32 logical symbols, see Markers for more
        %  information. If (mask & SC_MASK_FOLDERS)==0, the margin background colour is controlled by style 33 (STYLE_LINENUMBER).

%  You add logical markers to a line with SCI_MARKERADD. If a line has an associated marker that does not appear in the mask of any
%  margin with a non-zero width, the marker changes the background colour of the line. For example, suppose you decide to use logical
%  marker 10 to mark lines with a syntax error and you want to show such lines by changing the background colour. The mask for this
%  marker is 1 shifted left 10 times (1<<10) which is 0x400. If you make sure that no symbol margin includes 0x400 in its mask, any line
%  with the marker getS the background colour changed.
%  To set a non-folding margin 1 use SCI_SETMARGINMASKN(1, ~SC_MASK_FOLDERS) which is the default set by Scintilla. To set a
%  folding margin 2 use SCI_SETMARGINMASKN(2, SC_MASK_FOLDERS). ~SC_MASK_FOLDERS is 0x1FFFFFF in hexadecimal or 33554431
%  decimal. Of course, you may need to display all 32 symbols in a margin, in which case use SCI_SETMARGINMASKN(margin, -1).
clauses
    setMarginSensitiveN(Margin, Sensitive) :-
        sciLexer_api::setMarginSensitiveN(native, Margin, boolean::toBooleanInt(Sensitive)).

clauses
    getMarginSensitiveN(Margin) = sciLexer_api::getMarginSensitiveN(native, Margin).
        %  Each of the five margins can be set sensitive or insensitive to mouse clicks. A click in a sensitive margin sends a SCN_MARGINCLICK
        %  notification to the container. Margins that are not sensitive act as selection margins which make it easy to select ranges of lines. By
        %  default, all margins are insensitive.

clauses
    setMarginCursorN(Margin, Cursor) :-
        sciLexer_api::setMarginCursorN(native, Margin, Cursor).

clauses
    getMarginCursorN(Margin) = sciLexer_api::getMarginCursorN(native, Margin).
        %  A reversed arrow cursor is normally shown over all margins. This may be changed to a normal arrow with
        %  SCI_SETMARGINCURSORN(margin, SC_CURSORARROW) or restored to a reversed arrow with SCI_SETMARGINCURSORN(margin,
        %  SC_CURSORREVERSEARROW).

clauses
    marginLeft(Pixels) :-
        sciLexer_api::setMarginLeft(native, Pixels).

clauses
    marginLeft() = sciLexer_api::getMarginLeft(native).

clauses
    marginRight(Pixels) :-
        sciLexer_api::setMarginRight(native, Pixels).

clauses
    marginRight() = sciLexer_api::getMarginRight(native).
        %  These messages set and get the width of the blank margin on both sides of the text in pixels. The default is to one pixel on each side.

clauses
    setFoldMarginColor(UseSetting, Color) :-
        sciLexer_api::setFoldMarginColor(native, boolean::toBooleanInt(UseSetting), Color).

clauses
    setFoldMarginHiColor(UseSetting, Color) :-
        sciLexer_api::setFoldMarginHiColor(native, boolean::toBooleanInt(UseSetting), Color).
        %  These messages allow changing the colour of the fold margin and fold margin highLight. On Windows the fold margin colour defaults to
        %  ::GetSysColor(COLOR_3DFACE) and the fold margin highLight colour to ::GetSysColor(COLOR_3DHIGHLIGHT).

clauses
    marginSetText(Line, Text) :-
        sciLexer_api::marginSetText(native, fromLineNumber(Line), string8::toUtf8(Text)).

clauses
    marginGetText(Line) = string8::fromUtf8(Text8) :-
        Text8 = string8::create(string8_defaultLength),
        sciLexer_api::marginGetText(native, fromLineNumber(Line), Text8).

clauses
    marginSetStyle(Line, Style) :-
        sciLexer_api::marginSetStyle(native, fromLineNumber(Line), Style).

clauses
    marginGetStyle(Line) = sciLexer_api::marginGetStyle(native, fromLineNumber(Line)).

clauses
    marginSetStyles(Line, Styles) :-
        sciLexer_api::marginSetStyles(native, fromLineNumber(Line), Styles).

clauses
    marginGetStyles(Line) = Styles8 :-
        Styles8 = string8::create(string8_defaultLength),
        sciLexer_api::marginGetStyles(native, fromLineNumber(Line), Styles8).

clauses
    marginTextClearAll() :-
        sciLexer_api::marginTextClearAll(native).
        %  Text margins are created with the type SC_MARGIN_TEXT or SC_MARGIN_RTEXT. A different string may be set for each line with
        %  SCI_MARGINSETTEXT. The whole of the text margin on a line may be displayed in a particular style with SCI_MARGINSETSTYLE or
        %  each character may be individually styled with SCI_MARGINSETSTYLES which useS an array of bytes with each byte setTing the style of
        %  the corresponding text byte similar to SCI_SETSTYLINGEX. SetTing a text margin will cause a SC_MOD_CHANGEMARGIN notification to
        %  be sent.

clauses
    marginSetStyleOffset(Style) :-
        sciLexer_api::marginSetStyleOffset(native, Style).

clauses
    marginGetStyleOffset() = sciLexer_api::marginGetStyleOffset(native).
        %  Margin styles may be completely separated from standard text styles by setTing a style offset. For example,
        %  SCI_MARGINSETSTYLEOFFSET(256) would allow the margin styles to be numbered from 256 upto 511 so they do not overlap styles set
        %  by lexers. Each style number set with SCI_MARGINSETSTYLE or SCI_MARGINSETSTYLES has the offset added before looking up the
        %  style.

%  Annotations
%  Annotations are read-only lines of text underneath each line of editable text. An annotation may consist of multiple lines separated by
%  '\n'. Annotations can be used to display an assembler version of code for debugging or to show diagnostic messages inline or to line up
%  different versions of text in a merge tool.
%  Annotations used for inline diagnostics:
clauses
    annotationSetText(Line, Text) :-
        sciLexer_api::annotationSetText(native, fromLineNumber(Line), string8::toUtf8(Text)).

clauses
    annotationGetText(Line) = string8::fromUtf8(Text8) :-
        Text8 = string8::create(string8_defaultLength),
        sciLexer_api::annotationGetText(native, fromLineNumber(Line), Text8).

clauses
    annotationSetStyle(Line, Style) :-
        sciLexer_api::annotationSetStyle(native, fromLineNumber(Line), Style).

clauses
    annotationGetStyle(Line) = sciLexer_api::annotationGetStyle(native, fromLineNumber(Line)).

clauses
    annotationSetStyles(Line, Styles) :-
        sciLexer_api::annotationSetStyles(native, fromLineNumber(Line), Styles).

clauses
    annotationGetStyles(Line) = Styles8 :-
        Styles8 = string8::create(string8_defaultLength),
        sciLexer_api::annotationGetStyles(native, fromLineNumber(Line), Styles8).

clauses
    annotationGetLines(Line) = sciLexer_api::annotationGetLines(native, fromLineNumber(Line)).

clauses
    annotationClearAll() :-
        sciLexer_api::annotationClearAll(native).
        %  A different string may be set for each line with SCI_ANNOTATIONSETTEXT. To clear annotations call SCI_ANNOTATIONSETTEXT with a
        %  NULL pointer. The whole of the text ANNOTATION on a line may be displayed in a particular style with SCI_ANNOTATIONSETSTYLE or
        %  each character may be individually styled with SCI_ANNOTATIONSETSTYLES which useS an array of bytes with each byte setTing the
        %  style of the corresponding text byte similar to SCI_SETSTYLINGEX. The text must be set first as it specifies how long the annotation is
        %  so how many bytes of styling to read. SetTing an annotation will cause a SC_MOD_CHANGEANNOTATION notification to be sent.

%  The number of lines annotating a line can be retrieved with SCI_ANNOTATIONGETLINES. All the lines can be clearEd of annotations with
%  SCI_ANNOTATIONCLEARALL which is equivalent to clearIng each line (setTing to 0) and then deleting other memory used for this
%  feature.
clauses
    annotationVisible(Visible) :-
        sciLexer_api::annotationSetVisible(native, Visible).

clauses
    annotationVisible() = sciLexer_api::annotationGetVisible(native).
        %  Annotations can be made visible in a view and there is a choice of display style when visible. The two messages set and get the
        %  annotation display mode. The visible argument can be one of:

%  ANNOTATION_HIDDEN 0 Annotations are not displayed.
%  ANNOTATION_STANDARD 1 Annotations are drawn left justified with no adornment.
%  ANNOTATION_BOXED 2 Annotations are indented to match the text and are surrounded by a box.
clauses
    annotationStyleOffset(Style) :-
        sciLexer_api::annotationSetStyleOffset(native, Style).

clauses
    annotationStyleOffset() = sciLexer_api::annotationGetStyleOffset(native).
        %  Annotation styles may be completely separated from standard text styles by setTing a style offset. For example,
        %  SCI_ANNOTATIONSETSTYLEOFFSET(512) would allow the annotation styles to be numbered from 512 upto 767 so they do not overlap
        %  styles set by lexers (or margins if margins offset is 256). Each style number set with SCI_ANNOTATIONSETSTYLE or
        %  SCI_ANNOTATIONSETSTYLES has the offset added before looking up the style.

%  Other setTings
%  On Windows, there are some problems with visual flashing when switching between applications with palettes and it is also necessary
%  for the application containing the Scintilla control to forward some messages to Scintilla for its palette code to work. Because of this, by
%  default, the palette is not used and the application must tell Scintilla to use one. If Scintilla is not using a palette, it will only display in
%  those colours already available, which are often the 20 Windows system colours.
%  To see an example of how to enable palette support in Scintilla, search the text of SciTE for WM_PALETTECHANGED,
%  WM_QUERYNEWPALETTE and SCI_SETUSEPALETTE. The Windows messages to forward are:
%  WM_SYSCOLORCHANGE, WM_PALETTECHANGED, WM_QUERYNEWPALETTE (should return TRUE).
%  To forward a message (WM_nativeX, WPARAM, LPARAM) to Scintilla, you can use SendMessage(hScintilla, WM_nativeX, WPARAM, LPARAM)
%  where hScintilla is the handle to the Scintilla window you created as your editor.
%  While we are on the subject of forwarding messages in Windows, the top level window should forward any WM_SETTINGCHANGE
%  messages to Scintilla (this is currently used to collect changes to mouse setTings, but could be used for other user interface items in the
%  future).
clauses
    bufferedDraw(IsBuffered) :-
        sciLexer_api::setBufferedDraw(native, boolean::toBooleanInt(IsBuffered)).

clauses
    bufferedDraw() = boolean::fromBooleanInt(sciLexer_api::getBufferedDraw(native)).
        %  These messages turn buffered drawing on or off and report the buffered drawing state. Buffered drawing draws each line into a bitmap
        %  rather than directly to the screen and then copies the bitmap to the screen. This avoids flickering although it does take longer. The
        %  default is for drawing to be buffered.

clauses
    twoPhaseDraw(TwoPhase) :-
        sciLexer_api::setTwoPhaseDraw(native, boolean::toBooleanInt(TwoPhase)).

clauses
    twoPhaseDraw() = boolean::fromBooleanInt(sciLexer_api::getTwoPhaseDraw(native)).
        %  Two phase drawing is a better but slower way of drawing text. In single phase drawing each run of characters in one style is drawn
        %  along with its background. If a character overhangs the end of a run, such as in "V_" where the "V" is in a different style from the "_",
        %  then this can cause the right hand side of the "V" to be overdrawn by the background of the "_" which cuts it off. Two phase drawing
        %  fixes this by drawing all the backgrounds first and then drawing the text in transParent mode. Two phase drawing may flicker more than
        %  single phase unless buffered drawing is on. The default is for drawing to be two phase.

clauses
    technology(Technology) :-
        sciLexer_api::setTechnology(native, Technology).

clauses
    technology() = sciLexer_api::getTechnology(native).
        % The technology property allows choosing between different drawing APIs and options. On most platforms, the only choice
        % is SC_TECHNOLOGY_DEFAULT (0). On Windows Vista or later, SC_TECHNOLOGY_DIRECTWRITE (1) can be chosen to use the Direct2D
        % and DirectWrite APIs for higher quality antialiased drawing. Since Direct2D buffers drawing, Scintilla's buffering can
        % be turned off with SCI_SETBUFFEREDDRAW(0).

clauses
    fontQuality(FontQuality) :-
        sciLexer_api::setFontQuality(native, FontQuality).

clauses
    fontQuality() = sciLexer_api::getFontQuality(native).
        %  Manage font quality (antialiasing method). Currently, the following values are available on Windows: SC_EFF_QUALITY_DEFAULT
        %  (backward compatible), SC_EFF_QUALITY_NON_ANTIALIASED, SC_EFF_QUALITY_ANTIALIASED, SC_EFF_QUALITY_LCD_OPTIMIZED.

%  In case it is necessary to squeeze more options into this property, only a limited number of bits defined by SC_EFF_QUALITY_MASK
%  (0xf) will be used for quality.
clauses
    codePage(CodePage) :-
        sciLexer_api::setCodePage(native, CodePage).

clauses
    codePage() = sciLexer_api::getCodePage(native).
        %  Scintilla has some support for Japanese, Chinese and Korean DBCS. Use this message with codePage set to the code page number to
        %  set Scintilla to use code page information to ensure double byte characters are treated as one character rather than two. This also
        %  stops the caret from moving between the two bytes in a double byte character. Do not use this message to choose between different
        %  single byte character setS: it doesn't do that. Call with codePage set to zero to disable DBCS support. The default is
        %  SCI_SETCODEPAGE(0).

%  Code page SC_CPUtf8 (65001) setS Scintilla into Unicode mode with the document treated as a sequence of characters expressed in
%  UTF-8. The text is converted to the platform's normal Unicode encoding before being drawn by the OS and thus can display Hebrew,
%  Arabic, Cyrillic, and Han characters. Languages which can use two characters stacked vertically in one horizontal space, such as Thai,
%  will mostly work but there are some issues where the characters are drawn separately leading to visual glitches. Bi-directional text is
%  not supported.
%  Code page can be set to 932 (Japanese Shift-JIS), 936 (Simplified Chinese GBK), 949 (Korean Unified Hangul Code), 950 (Traditional
%  Chinese Big5), or 1361 (Korean Johab) although these may require installation of language specific support.
clauses
    setWordChars(Chars) :-
        sciLexer_api::setWordChars(native, string8::toUtf8(Chars)).
        %  Scintilla has several functions that operate on words, which are defined to be contiguous sequences of characters from a particular set
        %  of characters. This message defines which characters are members of that set. The character setS are set to default values before
        %  processing this function. For example, if you don't allow '_' in your set of characters use:
        %  SCI_SETWORDCHARS(0, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");

clauses
    setWhiteSpaceChars(Chars) :-
        sciLexer_api::setWhiteSpaceChars(native, string8::toUtf8(Chars)).
        %  Similar to SCI_SETWORDCHARS, this message allows the user to define which chars Scintilla considers as whitespace. SetTing the
        %  whitespace chars allows the user to fine-tune Scintilla's behaviour doing such things as moving the cursor to the start or end of a word;
        %  for example, by defining punctuation chars as whitespace, they will be skipped over when the user presses ctrl+left or ctrl+right. This
        %  function should be called after SCI_SETWORDCHARS as it will reset the whitespace characters to the default set.

clauses
    setCharsDefault() :-
        sciLexer_api::setCharsDefault(native).
        %  Use the default setS of word and whitespace characters. This setS whitespace to space, tab and other characters with codes less than
        %  0x20, with word characters set to alphanumeric and '_'.

clauses
    grabFocus() :-
        sciLexer_api::grabFocus(native).

clauses
    focus(Focus) :-
        sciLexer_api::setFocus(native, boolean::toBooleanInt(Focus)).

clauses
    focus() = boolean::fromBooleanInt(sciLexer_api::getFocus(native)).
        %  Scintilla can be told to grab the Focus with this message. This is needed more on GTK+ where Focus handling is more complicated than
        %  on Windows.

%  The internal Focus flag can be set with SCI_SETFOCUS. This is used by clients that have complex Focus requirements such as having
%  their own window that getS the real Focus but with the need to indicate that Scintilla has the logical Focus.
%  Brace highLighting
clauses
    braceHighLight(Pos1, Pos2) :-
        sciLexer_api::braceHighLight(native, Pos1, Pos2).
        %  Up to two characters can be highLighted in a 'brace highLighting style', which is defined as style number STYLE_BRACELIGHT (34). If you
        %  have enabled indent guides, you may also wish to highLight the indent that corresponds with the brace. You can locate the column with
        %  SCI_GETCOLUMN and highLight the indent with SCI_SETHIGHLIGHTGUIDE.

clauses
    braceBadLight(Pos1) :-
        sciLexer_api::braceBadLight(native, Pos1).
        %  If there is no matching brace then the brace badLighting style, style STYLE_BRACEBAD (35), can be used to show the brace that is
        %  unmatched. Using a position of INVALID_POSITION (-1) removes the highLight.

clauses
    braceHighlightIndicator(UseBraceHighlightIndicator, IndicatorNumber) :-
        sciLexer_api::braceHighlightIndicator(native, boolean::toBooleanInt(UseBraceHighlightIndicator), IndicatorNumber).
        % SCI_BRACEHIGHLIGHTINDICATOR(bool useBraceHighlightIndicator, int indicatorNumber)
        % Use specified indicator to highlight matching braces instead of changing their style.

clauses
    braceBadlightIndicator(UseBraceBadlightIndicator, IndicatorNumber) :-
        sciLexer_api::braceBadlightIndicator(native, boolean::toBooleanInt(UseBraceBadlightIndicator), IndicatorNumber).
        % SCI_BRACEBADLIGHTINDICATOR(bool useBraceBadLightIndicator, int indicatorNumber)
        % Use specified indicator to highlight non matching brace instead of changing its style.

clauses
    tryBraceMatch(Pos, MaxReStyle) = Match :-
        Match = sciLexer_api::braceMatch(native, Pos, MaxReStyle),
        -1 <> Match.
        %  The SCI_BRACEMATCH message finds a corresponding matching brace given pos, the position of one brace. The brace characters
        %  handled are '(', ')', '[', ']', '{', '}', '<', and '>'. The search is forwards from an opening brace and backwards from a closing brace. If the
        %  character at position is not a brace character, or a matching brace cannot be found, the return value is -1. Otherwise, the return value
        %  is the position of the matching brace.

%  A match only occurs if the style of the matching brace is the same as the starting brace or the matching brace is beyond the end of
%  styling. Nested braces are handled correctly. The maxReStyle parameter must currently be 0 - it may be used in the future to limit the
%  length of brace searches.
%  Tabs and Indentation Guides
%  Indentation (the white space at the start of a line) is often used by programmers to clarify program structure and in some languages,
%  for example Python, it may be part of the language syntax. Tabs are normally used in editors to insert a tab character or to pad text
%  with spaces up to the next tab.
%  Scintilla can be set to treat tab and backspace in the white space at the start of a line in a special way: inserting a tab indents the line to
%  the next indent position rather than just inserting a tab at the current character position and backspace unindents the line rather than
%  deleting a character. Scintilla can also display indentation guides (vertical lines) to help you to generate code.
clauses
    tabWidth(WidthInChars) :-
        sciLexer_api::setTabWidth(native, WidthInChars).

clauses
    tabWidth() = sciLexer_api::getTabWidth(native).
        %  SCI_SETTABWIDTH setS the size of a tab as a multiple of the size of a space character in STYLE_DEFAULT. The default tab width is 8
        %  characters. There are no limits on tab sizes, but values less than 1 or large values may have undesirable effects.

clauses
    useTabs(UseTabs) :-
        sciLexer_api::setUseTabs(native, boolean::toBooleanInt(UseTabs)).

clauses
    useTabs() = boolean::fromBooleanInt(sciLexer_api::getUseTabs(native)).
        %  SCI_SETUSETABS determines whether indentation should be created out of a mixture of tabs and spaces or be based purely on spaces.
        %  Set useTabs to false (0) to create all tabs and indents out of spaces. The default is true. You can use SCI_GETCOLUMN to get the
        %  column of a position taking the width of a tab into account.

clauses
    indent(WidthInChars) :-
        sciLexer_api::setIndent(native, WidthInChars).

clauses
    indent() = sciLexer_api::getIndent(native).
        %  SCI_SETINDENT setS the size of indentation in terms of the width of a space in STYLE_DEFAULT. If you set a width of 0, the indent size
        %  is the same as the tab size. There are no limits on indent sizes, but values less than 0 or large values may have undesirable effects.

clauses
    tabIndents(TabIndents) :-
        sciLexer_api::setTabIndents(native, boolean::toBooleanInt(TabIndents)).

clauses
    tabIndents() = boolean::fromBooleanInt(sciLexer_api::getTabIndents(native)).

clauses
    backSpaceUnIndents(BsUnIndents) :-
        sciLexer_api::setBackSpaceUnIndents(native, boolean::toBooleanInt(BsUnIndents)).

clauses
    backSpaceUnIndents() = boolean::fromBooleanInt(sciLexer_api::getBackSpaceUnIndents(native)).

%  Inside indentation white space, the tab and backspace keys can be made to indent and unindent rather than insert a tab character or
%  delete a character with the SCI_SETTABINDENTS and SCI_SETBACKSPACEUNINDENTS functions.
clauses
    setLineIndentation(Line, Indentation) :-
        sciLexer_api::setLineIndentation(native, fromLineNumber(Line), Indentation).

clauses
    getLineIndentation(Line) = sciLexer_api::getLineIndentation(native, fromLineNumber(Line)).
        %  The amount of indentation on a line can be discovered and set with SCI_GETLINEINDENTATION and SCI_SETLINEINDENTATION. The
        %  indentation is measured in character columns, which correspond to the width of space characters.

clauses
    getLineIndentPosition(Line) = sciLexer_api::getLineIndentPosition(native, fromLineNumber(Line)).
        %  This returns the position at the end of indentation of a line.

clauses
    indentationGuides(IndentView) :-
        sciLexer_api::setIndentationGuides(native, IndentView).

clauses
    indentationGuides() = sciLexer_api::getIndentationGuides(native).
        %  Indentation guides are dotted vertical lines that appear within indentation white space every indent size columns. They make it easy to
        %  see which constructs line up especially when they extend over multiple pages. Style STYLE_INDENTGUIDE (37) is used to specify the
        %  foreground and background colour of the indentation guides.

%  There are 4 indentation guide views. SC_IV_NONE turns the feature off but the other 3 states determine how far the guides appear on
%  empty lines.
%  SC_IV_NONE No indentation guides are shown.
%  SC_IV_REAL Indentation guides are shown inside real indentation white space.
%  SC_IV_LOOKFORWARD Indentation guides are shown beyond the actual indentation up to the level of the next non-empty line. If the
%  previous non-empty line was a fold header then indentation guides are shown for one more level of indent than that line. This setTing is
%  good for Python.
%  SC_IV_LOOKBOTH Indentation guides are shown beyond the actual indentation up to the level of the next non-empty line or previous
%  non-empty line whichever is the greater. This setTing is good for most languages.
clauses
    highLightGuide(Column) :-
        sciLexer_api::setHighLightGuide(native, Column).

clauses
    highLightGuide() = sciLexer_api::getHighLightGuide(native).
        %  When brace highLighting occurs, the indentation guide corresponding to the braces may be highLighted with the brace highLighting style,
        %  STYLE_BRACELIGHT (34). Set column to 0 to cancel this highLight.

%  Markers
%  There are 32 markers, numbered 0 to MARKER_MAX (31), and you can assign any combination of them to each line in the document.
%  Markers appear in the selection margin to the left of the text. If the selection margin is set to zero width, the background colour of the
%  whole line is changed instead. Marker numbers 25 to 31 are used by Scintilla in folding margins, and have symbolic names of the form
%  SC_MARKNUM_*, for example SC_MARKNUM_FOLDEROPEN.
%  Marker numbers 0 to 24 have no pre-defined function; you can use them to mark syntax errors or the current point of execution, break
%  points, or whatever you need marking. If you do not need folding, you can use all 32 for any purpose you wish.
%  Each marker number has a symbol associated with it. You can also set the foreground and background colour for each marker number,
%  so you can use the same symbol more than once with different colouring for different useS. Scintilla has a set of symbols you can
%  assign (SC_MARK_*) or you can use characters. By default, all 32 markers are set to SC_MARK_CIRCLE with a black foreground and a
%  white background.
%  The markers are drawn in the order of their numbers, so higher numbered markers appear on top of lower numbered ones. Markers
%  try to move with their text by tracking where the start of their line moves. When a line is deleted, its markers are combined, by an OR
%  operation, with the markers of the previous line.
clauses
    markerDefine(MarkerNumber, MarkerSymbols) :-
        sciLexer_api::markerDefine(native, MarkerNumber, MarkerSymbols).
        %  This message associates a marker number in the range 0 to 31 with one of the marker symbols or an ASCII character. The general-
        %  purpose marker symbols currently available are:
        %  SC_MARK_CIRCLE, SC_MARK_ROUNDRECT, SC_MARK_ARROW, SC_MARK_SMALLRECT, SC_MARK_SHORTARROW, SC_MARK_EMPTY,
        %  SC_MARK_ARROWDOWN, SC_MARK_MINUS, SC_MARK_PLUS, SC_MARK_ARROWS, SC_MARK_DOTDOTDOT,
        %  SC_MARK_BACKGROUND, SC_MARK_LEFTRECT, SC_MARK_FULLRECT, and SC_MARK_UNDERLINE.

%  The SC_MARK_BACKGROUND marker changes the background colour of the line only. The SC_MARK_FULLRECT symbol mirrors this,
%  changing only the margin background colour. SC_MARK_UNDERLINE draws an underline across the text. The SC_MARK_EMPTY symbol
%  is Invisible, allowing client code to track the movement of lines. You would also use it if you changed the folding style and wanted one or
%  more of the SC_FOLDERNUM_* markers to have no associated symbol.
%  Applications may use the marker symbol SC_MARK_AVAILABLE to indicate that plugins may allocate that marker number.
%  There are also marker symbols designed for use in the folding margin in a flattened tree style.
%  SC_MARK_BOXMINUS, SC_MARK_BOXMINUSCONNECTED, SC_MARK_BOXPLUS, SC_MARK_BOXPLUSCONNECTED,
%  SC_MARK_CIRCLEMINUS, SC_MARK_CIRCLEMINUSCONNECTED, SC_MARK_CIRCLEPLUS, SC_MARK_CIRCLEPLUSCONNECTED,
%  SC_MARK_LCORNER, SC_MARK_LCORNERCURVE, SC_MARK_TCORNER, SC_MARK_TCORNERCURVE, and SC_MARK_VLINE.
%  Characters can be used as markers by adding the ASCII value of the character to SC_MARK_CHARACTER (10000). For example, to
%  use 'A' (ASCII code 65) as marker number 1 use:
%  SCI_MARKERDEFINE(1, SC_MARK_CHARACTER+65).
%  The marker numbers SC_MARKNUM_FOLDER and SC_MARKNUM_FOLDEROPEN are used for showing that a fold is present and open or
%  closed. Any symbols may be assignEd for this purpose although the (SC_MARK_PLUS, SC_MARK_MINUS) pair or the
%  (SC_MARK_ARROW, SC_MARK_ARROWDOWN) pair are good choices. As well as these two, more assignments are needed for the
%  flattened tree style: SC_MARKNUM_FOLDEREND, SC_MARKNUM_FOLDERMIDTAIL, SC_MARKNUM_FOLDEROPENMID,
%  SC_MARKNUM_FOLDERSUB, and SC_MARKNUM_FOLDERTAIL. The bits used for folding are specified by SC_MASK_FOLDERS, which is
%  commonly used as an argument to SCI_SETMARGINMASKN when defining a margin to be used for folding.
%  This table shows which SC_MARK_* symbols should be assignEd to which SC_MARKNUM_* marker numbers to obtain four folding
%  styles: Arrow (mimics Macintosh), plus/minus shows folded lines as '+' and opened folds as '-', Circle tree, Box tree.
%  SC_MARKNUM_* Arrow Plus/minus Circle tree Box tree
%  FOLDEROPEN ARROWDOWN MINUS CIRCLEMINUS BOXMINUS
%  FOLDER ARROW PLUS CIRCLEPLUS BOXPLUS
%  FOLDERSUB EMPTY EMPTY VLINE VLINE
%  FOLDERTAIL EMPTY EMPTY LCORNERCURVE LCORNER
%  FOLDEREND EMPTY EMPTY CIRCLEPLUSCONNECTED BOXPLUSCONNECTED
%  FOLDEROPENMID EMPTY EMPTY CIRCLEMINUSCONNECTED BOXMINUSCONNECTED
%  FOLDERMIDTAIL EMPTY EMPTY TCORNERCURVE TCORNER
clauses
    markerDefinePixmap(MarkerNumber, Xpm) :-
        sciLexer_api::markerDefinePixmap(native, MarkerNumber, Xpm).
        %  Markers can be set to pixmaps with this message. The XPM format is used for the pixmap and it is limited to pixmaps that use one
        %  character per pixel with no named colours. The transParent colour may be named 'None'. The data should be null terminated. Pixmaps
        %  use the SC_MARK_PIXMAP marker symbol. You can find descriptions of the XPM format from here.

clauses
    markerDefineRgbaImage(MarkerNumber, Width, Height, Pixels) :-
        sciLexer_api::rgbaImageSetWidth(native, Width),
        sciLexer_api::rgbaImageSetHeight(native, Height),
        sciLexer_api::markerDefineRgbaImage(native, MarkerNumber, Pixels).
        %  SCI_RGBAIMAGESETWIDTH(int width)
        %  SCI_RGBAIMAGESETHEIGHT(int height)
        %  SCI_MARKERDEFINERGBAIMAGE(int markerNumber, const char8 *pixels)
        %  Markers can be set to translucent pixmaps with this message. The RGBA format is used for the pixmap. The width and height must
        %  previously been set with the SCI_RGBAIMAGESETWIDTH and SCI_RGBAIMAGESETHEIGHT messages. Pixmaps use the
        %  SC_MARK_RGBAIMAGE marker symbol.

clauses
    markerSymbolDefined(MarkerNumber) = sciLexer_api::markerSymbolDefined(native, MarkerNumber).
        %  Returns the symbol defined for a markerNumber with SCI_MARKERDEFINE or SC_MARK_PIXMAP if defined with
        %  SCI_MARKERDEFINEPIXMAP.

clauses
    markerSetFore(MarkerNumber, Color) :-
        sciLexer_api::markerSetFore(native, MarkerNumber, Color).

clauses
    markerSetBack(MarkerNumber, Color) :-
        sciLexer_api::markerSetBack(native, MarkerNumber, Color).
        %  These two messages set the foreground and background colour of a marker number.

clauses
    markerSetBackSelected(MarkerNumber, Color) :-
        sciLexer_api::markerSetBackSelected(native, MarkerNumber, Color).
        %  SCI_MARKERSETBACKSELECTED(int markerNumber, int colour)
        %  This message sets the highlight background colour of a marker number when its folding block is selected. The default colour is
        %  #FF0000.

clauses
    markerEnableHighlight(Enabled) :-
        sciLexer_api::markerEnableHighlight(native, boolean::toBooleanInt(Enabled)).
        %  SCI_MARKERENABLEHIGHLIGHT(bool enabled)
        %  This message allows to enable/disable the highlight folding block when it is selected. (i.e. block that contains the caret)

clauses
    markerSetAlpha(MarkerNumber, Alpha) :-
        sciLexer_api::markerSetAlpha(native, MarkerNumber, Alpha).
        %  When markers are drawn in the content area, either because there is no margin for them or they are of SC_MARK_BACKGROUND or
        %  SC_MARK_UNDERLINE types, they may be drawn translucently by setTing an alpha value.

clauses
    tryMarkerAdd(Line, MarkerNumber) = MarkerHandle :-
        MarkerHandle = sciLexer_api::markerAdd(native, fromLineNumber(Line), MarkerNumber),
        -1 <> MarkerHandle.
        %  This function adds marker number markerNumber to a line. The function fails if this fails (illegal line number, out of memory) or
        %  it returns a marker handle number that identifies the added marker. You can use this returned handle with
        %  SCI_MARKERLINEFROMHANDLE to find where a marker is after moving or combining lines and with SCI_MARKERDELETEHANDLE to
        %  delete the marker based on its handle. The function does not check the value of markerNumber, nor does it check if the line already
        %  contains the marker.

clauses
    markerAddSet(Line, MarkerMask) :-
        sciLexer_api::markerAddSet(native, fromLineNumber(Line), MarkerMask).
        %  This function can add one or more markers to a line with a single call, specified in the same "one-bit-per-marker" 32-bit integer format
        %  returned by SCI_MARKERGET (and used by the mask-based marker search functions SCI_MARKERNEXT and SCI_MARKERPREVIOUS).
        %  As with SCI_MARKERADD, no check is made to see if any of the markers are already present on the targeted line.

clauses
    markerDelete(Line, MarkerNumber) :-
        sciLexer_api::markerDelete(native, fromLineNumber(Line), MarkerNumber).
        %  This searches the given line number for the given marker number and deletes it if it is present. If you added the same marker more
        %  than once to the line, this will delete one copy each time it is useD. If you pass in a marker number of -1, all markers are deleted from
        %  the line.

clauses
    markerDeleteAll(MarkerNumber) :-
        sciLexer_api::markerDeleteAll(native, MarkerNumber).
        %  This removes markers of the given number from all lines. If markerNumber is -1, it deletes all markers from all lines.

clauses
    markerGet(Line) = sciLexer_api::markerGet(native, fromLineNumber(Line)).
        %  This returns a 32-bit integer that indicates which markers were present on the line. Bit 0 is set if marker 0 is present, bit 1 for marker 1
        %  and so on.

clauses
    tryMarkerNext(LineStart, MarkerMask) = toLineNumber(LineNumber) :-
        LineNumber = sciLexer_api::markerNext(native, fromLineNumber(LineStart), MarkerMask),
        -1 <> LineNumber.

clauses
    tryMarkerPrevious(LineStart, MarkerMask) = toLineNumber(LineNumber) :-
        LineNumber = sciLexer_api::markerPrevious(native, fromLineNumber(LineStart), MarkerMask),
        -1 <> LineNumber.
        %  These functions search efficiently for lines that include a given set of markers. The search starts at line number lineStart and continues
        %  forwards to the end of the file (SCI_MARKERNEXT) or backwards to the start of the file (SCI_MARKERPREVIOUS). The markerMask
        %  argument should have one bit set for each marker you wish to find. Set bit 0 to find marker 0, bit 1 for marker 1 and so on. The
        %  function returns the line number of the first line that contains one of the markers in markerMask or fails if no marker is found.

clauses
    tryMarkerLineFromHandle(MarkerHandle) = toLineNumber(LineNumber) :-
        LineNumber = sciLexer_api::markerLineFromHandle(native, MarkerHandle),
        -1 <> LineNumber.
        %  The markerHandle argument is an identifier for a marker returned by SCI_MARKERADD. This function searches the document for the
        %  marker with this handle and returns the line number that contains it or fails if it is not found.

clauses
    markerDeleteHandle(MarkerHandle) :-
        sciLexer_api::markerDeleteHandle(native, MarkerHandle).
        %  The markerHandle argument is an identifier for a marker returned by SCI_MARKERADD. This function searches the document for the
        %  marker with this handle and deletes the marker if it is found.

%  Indicators
%  Indicators are used to display additional information over the top of styling. They can be used to show, for example, syntax errors,
%  deprecated names and bad indentation by drawing underlines under text or boxes around text. Originally, Scintilla stored indicator
%  information in the style bytes but this has proved limiting, so now up to 32 separately stored indicators may be useD. While style byte
%  indicators currently still work, they will soon be removed so all the bits in each style byte can be used for lexical states.
%  Indicators may be displayed as simple underlines, squiggly underlines, a line of small 'T' shapes, a line of diagonal hatching, a strike-
%  out or a rectangle around the text.
%  The SCI_INDIC* messages allow you to get and set the visual appearance of the indicators. They all use an indicatorNumber argument
%  in the range 0 to INDIC_MAX(31) to set the indicator to style. To prevent interference the set of indicators is divided up into a range for
%  use by lexers (0..7) and a range for use by containers (8=INDIC_CONTAINER .. 31=INDIC_MAX).
clauses
    indicSetStyle(IndicatorNumber, IndicatorStyle) :-
        sciLexer_api::indicSetStyle(native, IndicatorNumber, IndicatorStyle).

clauses
    indicGetStyle(IndicatorNumber) = sciLexer_api::indicGetStyle(native, IndicatorNumber).
        %  These two messages set and get the style for a particular indicator. The indicator styles currently available are:

%  Symbol Value Visual effect
%  INDIC_PLAIN 0 Underlined with a single, straight line.
%  INDIC_SQUIGGLE 1 A squiggly underline.
%  INDIC_TT 2 A line of small T shapes.
%  INDIC_DIAGONAL 3 Diagonal hatching.
%  INDIC_STRIKE 4 Strike out.
%  INDIC_HIDDEN 5 An indicator with no visual effect.
%  INDIC_BOX 6 A rectangle around the text.
%  INDIC_ROUNDBOX 7 A rectangle with rounded corners around the text using translucent drawing with the interior more transParent
%  than the border. You can use SCI_INDICSETALPHA to control the alpha transparency value. The default alpha value is 30.
%  The default indicator styles are equivalent to:
%  SCI_INDICSETSTYLE(0, INDIC_SQUIGGLE);
%  SCI_INDICSETSTYLE(1, INDIC_TT);
%  SCI_INDICSETSTYLE(2, INDIC_PLAIN);
clauses
    indicSetFore(IndicatorNumber, Color) :-
        sciLexer_api::indicSetFore(native, IndicatorNumber, Color).

clauses
    indicGetFore(IndicatorNumber) = sciLexer_api::indicGetFore(native, IndicatorNumber).
        %  These two messages set and get the colour used to draw an indicator. The default indicator colours are equivalent to:
        %  SCI_INDICSETFORE(0, 0x007f00); (dark green)
        %  SCI_INDICSETFORE(1, 0xff0000); (light blue)
        %  SCI_INDICSETFORE(2, 0x0000ff); (light red)

clauses
    indicSetAlpha(IndicatorNumber, Alpha) :-
        sciLexer_api::indicSetAlpha(native, IndicatorNumber, Alpha).

clauses
    indicGetAlpha(IndicatorNumber) = sciLexer_api::indicGetAlpha(native, IndicatorNumber).
        %  These two messages set and get the alpha transparency used for drawing the fill color of the INDIC_ROUNDBOX rectangle. The alpha
        %  value can range from 0 (completely transParent) to 255 (no transparency).

clauses
    indicSetOutlineAlpha(IndicatorNumber, OutlineAlpha) :-
        sciLexer_api::indicSetOutlineAlpha(native, IndicatorNumber, OutlineAlpha).

clauses
    indicGetOutlineAlpha(IndicatorNumber) = sciLexer_api::indicGetOutlineAlpha(native, IndicatorNumber).
        %  These two messages set and get the alpha transparency used for drawing the fill color of the INDIC_ROUNDBOX rectangle. The alpha
        %  value can range from 0 (completely transParent) to 255 (no transparency).

clauses
    indicSetUnder(IndicatorNumber, Under) :-
        sciLexer_api::indicSetUnder(native, IndicatorNumber, boolean::toBooleanInt(Under)).

clauses
    indicGetUnder(IndicatorNumber) :-
        sciLexer_api::indicGetUnder(native, IndicatorNumber).
        %  These two messages set and get whether an indicator is drawn under text or over(default). Drawing under text works only for modern
        %  indicators when two phase drawing is enabled.

%  Modern Indicators
%  Modern indicators are stored in a format similar to run length encoding which is efficient in both speed and storage for sparse
%  information.
%  An indicator may store different values for each range but currently all values are drawn the same. In the future, it may be possible to
%  draw different values in different styles.
clauses
    indicatorCurrent(Indicator) :-
        sciLexer_api::setIndicatorCurrent(native, Indicator).

clauses
    indicatorCurrent() = sciLexer_api::getIndicatorCurrent(native).
        %  These two messages set and get the indicator that will be affected by calls to SCI_INDICATORFILLRANGE and
        %  SCI_INDICATORCLEARRANGE.

clauses
    indicatorValue(Value) :-
        sciLexer_api::setIndicatorValue(native, Value).

clauses
    indicatorValue() = sciLexer_api::getIndicatorValue(native).
        %  These two messages set and get the value that will be set by calls to SCI_INDICATORFILLRANGE.

clauses
    indicatorFillRange(Position, FillLength) :-
        sciLexer_api::indicatorFillRange(native, Position, FillLength).

clauses
    indicatorClearRange(Position, ClearLength) :-
        sciLexer_api::indicatorClearRange(native, Position, ClearLength).
        %  These two messages fill or clear a range for the current indicator. SCI_INDICATORFILLRANGE fills with the the current value.

clauses
    indicatorAllOnFor(Position) = sciLexer_api::indicatorAllOnFor(native, Position).
        %  Retrieve a bitmap value representing which indicators are non-zero at a position.

clauses
    indicatorValueAt(Indicator, Position) :-
        sciLexer_api::indicatorValueAt(native, Indicator, Position).
        %  Retrieve the value of a particular indicator at a position.

clauses
    indicatorStart(Indicator, Position) = sciLexer_api::indicatorStart(native, Indicator, Position).

clauses
    indicatorEnd(Indicator, Position) = sciLexer_api::indicatorEnd(native, Indicator, Position).
        %  Find the start or end of a range with one value from a position within the range. Can be used to iterate through the document to
        %  discover all the indicator positions.

%  Style Byte Indicators (deprecated)
%  By default, Scintilla organizes the style byte associated with each text byte as 5 bits of style information (for 32 styles) and 3 bits of
%  indicator information for 3 independent indicators so that, for example, syntax errors, deprecated names and bad indentation could all
%  be displayed at once.
%  The indicators are set using SCI_STARTSTYLING with a INDICS_MASK mask and SCI_SETSTYLING with the values INDIC0_MASK,
%  INDIC1_MASK and INDIC2_MASK.
%  If you are using indicators in a buffer that has a lexer active (see SCI_SETLEXER), you must save lexing state information before
%  setTing any indicators and restore it afterwards. Use SCI_GETENDSTYLED to retrieve the current "styled to" position and
%  SCI_STARTSTYLING to reset the styling position and mask (0x1f in the default layOut of 5 style bits and 3 indicator bits) when you are
%  done.
%  The number of bits used for styles can be altered with SCI_SETSTYLEBITS from 0 to 8 bits. The remaining bits can be used for
%  indicators, so there can be from 1 to 8 indicators. However, the INDIC*_MASK constants defined in Scintilla.h all assume 5 bits of
%  styling information and 3 indicators. If you use a different arrangement, you must define your own constants.
%  Autocompletion
%  Autocompletion displays a list box showing likely identifiers based upon the user's typing. The user chooses the currently selected item
%  by pressing the tab character or another character that is a member of the fillup character set defined with SCI_autoCFILLUPS.
%  Autocompletion is triggered by your application. For example, in C if you detect that the user has just typed fred. you could look up
%  fred, and if it has a known list of members, you could offer them in an autocompletion list. Alternatively, you could monitor the user's
%  typing and offer a list of likely items once their typing has narrowed down the choice to a reasonable list. As yet another alternative,
%  you could define a key code to activate the list.
%  When the user makes a selection from the list the container is sent a SCN_AUTOCSELECTION notification message. On return from the
%  notification Scintilla will insert the selected text unless the autocompletion list has been cancelled, for example by the container sending
%  SCI_AUTOCCANCEL.
%  To make use of autocompletion you must monitor each character added to the document. See SciTEBase::CharAdded() in
%  SciTEBase.cxx for an example of autocompletion.
clauses
    autoCShow(LenEntered, List) :-
        sciLexer_api::autoCShow(native, LenEntered, string8::toUtf8(List)).
        %  This message causeS a list to be displayed. lenEntered is the number of characters of the word already entered and list is the list of
        %  words separated by separator characters. The initial separator character is a space but this can be set or got with
        %  SCI_autoCSEPARATOR and SCI_autoCSEPARATOR.

%  The list of words should be in sorted order. If set to ignore case mode with SCI_autoCIGNORECASE, then strings are matched
%  after being converted to upper case. One result of this is that the list should be sorted with the punctuation characters '[', '\', ']', '^', '_',
%  and '`' sorted after letters.
clauses
    autoCCancel() :-
        sciLexer_api::autoCCancel(native).
        %  This message cancels any displayed autocompletion list. When in autocompletion mode, the list should disappear when the user types a
        %  character that can not be part of the autocompletion, such as '.', '(' or '[' when typing an identifier. A set of characters that will cancel
        %  autocompletion can be specified with SCI_AUTOCSTOPS.

clauses
    autoCActive() = boolean::fromBooleanInt(sciLexer_api::autoCActive(native)).
        %  This message returns non-zero if there is an active autocompletion list and zero if there is not.

clauses
    autoCPosStart() = sciLexer_api::autoCPosStart(native).
        %  This returns the value of the current position when SCI_AUTOCSHOW started display of the list.

clauses
    autoCComplete() :-
        sciLexer_api::autoCComplete(native).
        %  This message triggers autocompletion. This has the same effect as the tab key.

clauses
    autoCSTops(Chars) :-
        sciLexer_api::autoCSTops(native, string8::toUtf8(Chars)).
        %  The chars argument is a string containing a list of characters that will automatically cancel the autocompletion list. When you start the
        %  editor, this list is empty.

clauses
    autoCSeparator(Separator) :-
        sciLexer_api::autoCSetSeparator(native, toUtf8Char(Separator)).

clauses
    autoCSeparator() = fromUtf8Char(sciLexer_api::autoCGetSeparator(native)).
        %  These two messages set and get the separator character used to separate words in the SCI_AUTOCSHOW list. The default is the space
        %  character.

clauses
    autoCSelect(Select) :-
        sciLexer_api::autoCSelect(native, string8::toUtf8(Select)).

clauses
    autoCCurrent() = sciLexer_api::autoCGetCurrent(native).
        %  This message selects an item in the autocompletion list. It searches the list of words for the first that matches select. By default,
        %  comparisons are case sensitive, but you can change this with SCI_autoCIGNORECASE. The match is character by character for
        %  the length of the select string. That is, if select is "Fred" it will match "Frederick" if this is the first item in the list that begins with "Fred".
        %  If an item is found, it is selected. If the item is not found, the autocompletion list closes if auto-hide is true (see
        %  SCI_autoCAUTOHIDE).
        %  The current selection index can be retrieved with SCI_autoCCURRENT.

clauses
    autoCCurrentText() = string8::fromUtf8(Text8) :-
        L = sciLexer_api::autoCGetCurrentText(native, string8::null),
        Text8 = string8::create(L),
        _ = sciLexer_api::autoCGetCurrentText(native, Text8).
        %  This message retrieves the current selected text in the autocompletion list. Normally the SCN_AUTOCSELECTION notification is useD
        %  instead.

%  The value is copied to the text buffer, returning the length (not including the terminating 0). If not found, an empty string is copied to
%  the buffer and 0 is returned.
%  If the value argument is 0 then the length that should be allocated to store the value is returned; again, the terminating 0 is not
%  included.
clauses
    autoCCancelAtStart(Cancel) :-
        sciLexer_api::autoCSetCancelAtStart(native, boolean::toBooleanInt(Cancel)).

clauses
    autoCCancelAtStart() = boolean::fromBooleanInt(sciLexer_api::autoCGetCancelAtStart(native)).
        %  The default behavior is for the list to be cancelled if the caret moves to the location it was at when the list was displayed. By calling this
        %  message with a false argument, the list is not cancelled until the caret moves before the first character of the word being completed.

clauses
    autoCFillups(Chars) :-
        sciLexer_api::autoCSetFillups(native, string8::toUtf8(Chars)).
        %  If a fillup character is typed with an autocompletion list active, the currently selected item in the list is added into the document, then
        %  the fillup character is added. Common fillup characters are '(', '[' and '.' but others are possible depending on the language. By default,
        %  no fillup characters are set.

clauses
    autoCChooseSingle(ChooseSingle) :-
        sciLexer_api::autoCSetChooseSingle(native, boolean::toBooleanInt(ChooseSingle)).

clauses
    autoCChooseSingle() = boolean::fromBooleanInt(sciLexer_api::autoCGetChooseSingle(native)).
        %  If you use SCI_autoCCHOOSESINGLE(1) and a list has only one item, it is automatically added and no list is displayed. The default
        %  is to display the list even if there is only a single item.

clauses
    autoCIgnoreCase(IgnoreCase) :-
        sciLexer_api::autoCSetIgnoreCase(native, boolean::toBooleanInt(IgnoreCase)).

clauses
    autoCIgnoreCase() = boolean::fromBooleanInt(sciLexer_api::autoCGetIgnoreCase(native)).
        %  By default, matching of characters to list members is case sensitive. These messages let you set and get case sensitivity.

clauses
    autoCCaseInsensitiveBehaviour(Behaviour) :-
        sciLexer_api::autoCSetCaseInsensitiveBehaviour(native, Behaviour).

clauses
    autoCCaseInsensitiveBehaviour() = sciLexer_api::autoCGetCaseInsensitiveBehaviour(native).

clauses
    autoCAutoHide(AutoHide) :-
        sciLexer_api::autoCSetAutoHide(native, boolean::toBooleanInt(AutoHide)).

clauses
    autoCAutoHide() = boolean::fromBooleanInt(sciLexer_api::autoCGetAutoHide(native)).
        %  By default, the list is cancelled if there are no viable matches (the user has typed characters that no longer match a list entry). If you
        %  want to keep displaying the original list, set autoHide to false. This also effects SCI_AUTOCSELECT.

clauses
    autoCDropRestOfWord(DropRestOfWord) :-
        sciLexer_api::autoCSetDropRestOfWord(native, boolean::toBooleanInt(DropRestOfWord)).

clauses
    autoCDropRestOfWord() = boolean::fromBooleanInt(sciLexer_api::autoCGetDropRestOfWord(native)).
        %  When an item is selected, any word characters following the caret are first erased if dropRestOfWord is set true. The default is false.

clauses
    registerImage(Type, XpmData) :-
        sciLexer_api::registerImage(native, Type, XpmData).

clauses
    registerRgbaImage(Type, Width, Height, Pixels) :-
        sciLexer_api::rgbaImageSetWidth(native, Width),
        sciLexer_api::rgbaImageSetHeight(native, Height),
        sciLexer_api::registerRgbaImage(native, Type, Pixels).

clauses
    clearRegisteredImages() :-
        sciLexer_api::clearRegisteredImages(native).

clauses
    autoCTypeSeparator(SeparatorCharacter) :-
        sciLexer_api::autoCSetTypeSeparator(native, toUtf8Char(SeparatorCharacter)).

clauses
    autoCTypeSeparator() = fromUtf8Char(sciLexer_api::autoCGetTypeSeparator(native)).
        %  Autocompletion list items may display an image as well as text. Each image is first registered with an integer type. Then this integer is
        %  included in the text of the list separated by a '?' from the text. For example, "fclose?2 fopen" displays image 2 before the string "fclose"
        %  and no image before "fopen". The images are in XPM format as is described for SCI_MARKERDEFINEPIXMAP The set of registered
        %  images can be clearEd with SCI_CLEARREGISTEREDIMAGES and the '?' separator changed with SCI_autoCTYPESEPARATOR.

clauses
    autoCMaxHeight(RowCount) :-
        sciLexer_api::autoCSetMaxHeight(native, RowCount).

clauses
    autoCMaxHeight() = sciLexer_api::autoCGetMaxHeight(native).
        %  Get or set the maximum number of rows that will be visible in an autocompletion list. If there are more rows in the list, then a vertical
        %  scrollbar is shown. The default is 5.

clauses
    autoCMaxWidth(CharacterCount) :-
        sciLexer_api::autoCSetMaxWidth(native, CharacterCount).

clauses
    autoCMaxWidth() = sciLexer_api::autoCGetMaxWidth(native).
        %  Get or set the maximum width of an autocompletion list expressed as the number of characters in the longest item that will be totally
        %  visible. If zero (the default) then the list's width is calculated to fit the item with the most characters. Any items that cannot be fully
        %  displayed within the available width are indicated by the presence of ellipsis.

%  User lists
%  User lists use the same internal mechanisms as autocompletion lists, and all the calls listed for autocompletion work on them; you
%  cannot display a user list at the same time as an autocompletion list is active. They differ in the following respects:
%  o The SCI_autoCCHOOSESINGLE message has no effect.
%  o When the user makes a selection you are sent a SCN_USERLISTSELECTION notification message rather than
%  SCN_AUTOCSELECTION.
%  BEWARE: if you have set fillup characters or stop characters, these will still be active with the user list, and may result in items being
%  selected or the user list cancelled due to the user typing into the editor.
clauses
    userListShow(ListType, List) :-
        sciLexer_api::userListShow(native, ListType, string8::toUtf8(List)).
        %  The listType parameter is returned to the container as the wParam field of the SCNotification structure. It must be greater than 0 as this
        %  is how Scintilla tells the difference between an autocompletion list and a user list. If you have different types of list, for example a list of
        %  buffers and a list of macros, you can use listType to tell which one has returned a selection.

%  Call tips
%  Call tips are small windows displaying the arguments to a function and are displayed after the user has typed the name of the function.
%  They normally display characters using the font faceName, size and character set defined by STYLE_DEFAULT. You can choose to use
%  STYLE_CALLTIP to define the faceName, size, foreground and background colours and character set with SCI_CALLTIPUSESTYLE. This
%  also enables support for Tab characters. There is some interaction between call tips and autocompletion lists in that showing a call tip
%  cancels any active autocompletion list, and vice versa.
%  Call tips can highLight part of the text within them. You could use this to highLight the current argument to a function by counting the
%  number of commas (or whatever separator your language useS). See SciTEBase::CharAdded() in SciTEBase.cxx for an example of call
%  tip use.
%  The mouse may be clicked on call tips and this causeS a SCN_CALLTIPCLICK notification to be sent to the container. Small up and down
%  arrows may be displayed within a call tip by, respectively, including the characters '\001', or '\002'. This is useFul for showing that there
%  are overloaded variants of one function name and that the user can click on the arrows to cycle through the overloads.
%  Alternatively, call tips can be displayed when you leave the mouse pointer for a while over a word in response to the
%  SCN_DWELLSTART notification and cancelled in response to SCN_DWELLEND. This method could be used in a debugger to give the
%  value of a variable, or during editing to give information about the word under the pointer.
clauses
    calltipShow(PosStart, Definition) :-
        sciLexer_api::calltipShow(native, PosStart, string8::toUtf8(Definition)).
        %  This message starts the process by displaying the call tip window. If a call tip is already active, this has no effect.
        %  posStart is the position in the document at which to align the call tip. The call tip text is aligned to start 1 line below this character unless
        %  you have included up and/or down arrows in the call tip text in which case the tip is aligned to the right-hand edge of the rightmost
        %  arrow. The assumption is that you will start the text with something like "\001 1 of 3 \002".
        %  definition is the call tip text. This can contain multiple lines separated by '\n' (Line Feed, ASCII code 10) characters. Do not include '\r'
        %  (Carriage Return, ASCII code 13), as this will most likely print as an empty box. '\t' (Tab, ASCII code 9) is supported if you set a
        %  tabsize with SCI_CALLTIPUSESTYLE.

clauses
    calltipCancel() :-
        sciLexer_api::calltipCancel(native).
        %  This message cancels any displayed call tip. Scintilla will also cancel call tips for you if you use any keyboard commands that are not
        %  compatible with editing the argument list of a function.

clauses
    calltipActive() :-
        sciLexer_api::calltipActive(native).
        %  This returns 1 if a call tip is active and 0 if it is not active.

clauses
    calltipPosStart() :-
        sciLexer_api::calltipPosStart(native).
        %  This message returns the value of the current position when SCI_CALLTIPSHOW started to display the tip.

clauses
    calltipSetHlt(HlStart, HlEnd) :-
        sciLexer_api::calltipSetHlt(native, HlStart, HlEnd).
        %  This setS the region of the call tips text to display in a highLighted style. hlStart is the zero-based index into the string of the first
        %  character to highLight and hlEnd is the index of the first character after the highLight. hlEnd must be greater than hlStart; hlEnd-hlStart is
        %  the number of characters to highLight. HighLights can extend over line ends if this is required.

%  UnhighLighted text is drawn in a mid gray. Selected text is drawn in a dark blue. The background is white. These can be changed with
%  SCI_CALLTIPSETBACK, SCI_CALLTIPSETFORE, and SCI_CALLTIPSETFOREHLT.
clauses
    calltipSetBack(Color) :-
        sciLexer_api::calltipSetBack(native, Color).
        %  The background colour of call tips can be set with this message; the default colour is white. It is not a good idea to set a dark colour as
        %  the background as the default colour for normal calltip text is mid gray and the defaultcolour for highLighted text is dark blue. This also
        %  setS the background colour of STYLE_CALLTIP.

clauses
    calltipSetFore(Color) :-
        sciLexer_api::calltipSetFore(native, Color).
        %  The colour of call tip text can be set with this message; the default colour is mid gray. This also setS the foreground colour of
        %  STYLE_CALLTIP.

clauses
    calltipForeHlt(Color) :-
        sciLexer_api::calltipSetForehlt(native, Color).
        %  The colour of highLighted call tip text can be set with this message; the default colour is dark blue.

clauses
    calltipUseStyle(TabSize) :-
        sciLexer_api::calltipUseStyle(native, TabSize).
        %  This message changes the style used for call tips from STYLE_DEFAULT to STYLE_CALLTIP and setS a tab size in screen pixels. If
        %  tabsize is less than 1, Tab characters are not treated specially. Once this call has been useD, the call tip foreground and background
        %  colours are also taken from the style.

clauses
    calltipSetPosition(Above) :-
        sciLexer_api::calltipSetPosition(native, boolean::toBooleanInt(Above)).
        %  Keyboard commands
        %  To allow the container application to perform any of the actions available to the user with keyboard, all the keyboard actions are
        %  messages. They do not take any parameters. These commands are also used when redefining the key bindings with the
        %  SCI_ASSIGNCMDKEY message.

clauses
    lineDown() :-
        sciLexer_api::lineDown(native).

clauses
    lineDownExtend() :-
        sciLexer_api::lineDownExtend(native).

clauses
    lineDownRectExtend() :-
        sciLexer_api::lineDownRectExtend(native).

clauses
    lineScrollDown() :-
        sciLexer_api::lineScrollDown(native).

clauses
    lineUp() :-
        sciLexer_api::lineUp(native).

clauses
    lineUpExtend() :-
        sciLexer_api::lineUpExtend(native).

clauses
    lineUpRectExtend() :-
        sciLexer_api::lineUpRectExtend(native).

clauses
    lineScrollUp() :-
        sciLexer_api::lineScrollUp(native).

clauses
    paraDown() :-
        sciLexer_api::paraDown(native).

clauses
    paraDownExtend() :-
        sciLexer_api::paraDownExtend(native).

clauses
    paraUp() :-
        sciLexer_api::paraUp(native).

clauses
    paraUpExtend() :-
        sciLexer_api::paraUpExtend(native).

clauses
    charLeft() :-
        sciLexer_api::charLeft(native).

clauses
    charLeftExtend() :-
        sciLexer_api::charLeftExtend(native).

clauses
    charLeftRectExtend() :-
        sciLexer_api::charLeftRectExtend(native).

clauses
    charRight() :-
        sciLexer_api::charRight(native).

clauses
    charRightExtend() :-
        sciLexer_api::charRightExtend(native).

clauses
    charRightRectExtend() :-
        sciLexer_api::charRightRectExtend(native).

clauses
    wordLeft() :-
        sciLexer_api::wordLeft(native).

clauses
    wordLeftExtend() :-
        sciLexer_api::wordLeftExtend(native).

clauses
    wordRight() :-
        sciLexer_api::wordRight(native).

clauses
    wordRightExtend() :-
        sciLexer_api::wordRightExtend(native).

clauses
    wordLeftEnd() :-
        sciLexer_api::wordLeftEnd(native).

clauses
    wordLeftEndExtend() :-
        sciLexer_api::wordLeftEndExtend(native).

clauses
    wordRightEnd() :-
        sciLexer_api::wordRightEnd(native).

clauses
    wordRightEndExtend() :-
        sciLexer_api::wordRightEndExtend(native).

clauses
    wordPartLeft() :-
        sciLexer_api::wordPartLeft(native).

clauses
    wordPartLeftExtend() :-
        sciLexer_api::wordPartLeftExtend(native).

clauses
    wordPartRight() :-
        sciLexer_api::wordPartRight(native).

clauses
    wordPartRightExtend() :-
        sciLexer_api::wordPartRightExtend(native).

clauses
    home() :-
        sciLexer_api::home(native).

clauses
    homeExtend() :-
        sciLexer_api::homeExtend(native).

clauses
    homeRectExtend() :-
        sciLexer_api::homeRectExtend(native).

clauses
    homeDisplay() :-
        sciLexer_api::homeDisplay(native).

clauses
    homeDisplayExtend() :-
        sciLexer_api::homeDisplayExtend(native).

clauses
    homeWrap() :-
        sciLexer_api::homeWrap(native).

clauses
    homeWrapExtend() :-
        sciLexer_api::homeWrapExtend(native).

clauses
    vcHome() :-
        sciLexer_api::vcHome(native).

clauses
    vcHomeExtend() :-
        sciLexer_api::vcHomeExtend(native).

clauses
    vcHomeRectExtend() :-
        sciLexer_api::vcHomeRectExtend(native).

clauses
    vcHomeWrap() :-
        sciLexer_api::vcHomeWrap(native).

clauses
    vcHomeWrapExtend() :-
        sciLexer_api::vcHomeWrapExtend(native).

clauses
    lineEnd() :-
        sciLexer_api::lineEnd(native).

clauses
    lineEndExtend() :-
        sciLexer_api::lineEndExtend(native).

clauses
    lineEndRectExtend() :-
        sciLexer_api::lineEndRectExtend(native).

clauses
    lineEndDisplay() :-
        sciLexer_api::lineEndDisplay(native).

clauses
    lineEndDisplayExtend() :-
        sciLexer_api::lineEndDisplayExtend(native).

clauses
    lineEndWrap() :-
        sciLexer_api::lineEndWrap(native).

clauses
    lineEndWrapExtend() :-
        sciLexer_api::lineEndWrapExtend(native).

clauses
    documentStart() :-
        sciLexer_api::documentStart(native).

clauses
    documentStartExtend() :-
        sciLexer_api::documentStartExtend(native).

clauses
    documentEnd() :-
        sciLexer_api::documentEnd(native).

clauses
    documentEndExtend() :-
        sciLexer_api::documentEndExtend(native).

clauses
    pageUp() :-
        sciLexer_api::pageUp(native).

clauses
    pageUpExtend() :-
        sciLexer_api::pageUpExtend(native).

clauses
    pageUpRectExtend() :-
        sciLexer_api::pageUpRectExtend(native).

clauses
    pageDown() :-
        sciLexer_api::pageDown(native).

clauses
    pageDownExtend() :-
        sciLexer_api::pageDownExtend(native).

clauses
    pageDownRectExtend() :-
        sciLexer_api::pageDownRectExtend(native).

clauses
    stutteredPageUp() :-
        sciLexer_api::stutteredPageUp(native).

clauses
    stutteredPageUpExtend() :-
        sciLexer_api::stutteredPageUpExtend(native).

clauses
    stutteredPageDown() :-
        sciLexer_api::stutteredPageDown(native).

clauses
    stutteredPageDownExtend() :-
        sciLexer_api::stutteredPageDownExtend(native).

clauses
    deleteBack() :-
        sciLexer_api::deleteBack(native).

clauses
    deleteBackNotLine() :-
        sciLexer_api::deleteBackNotLine(native).

clauses
    delWordLeft() :-
        sciLexer_api::delWordLeft(native).

clauses
    delWordRight() :-
        sciLexer_api::delWordRight(native).

clauses
    delWordRightEnd() :-
        sciLexer_api::delWordRightEnd(native).

clauses
    delLineLeft() :-
        sciLexer_api::delLineLeft(native).

clauses
    delLineRight() :-
        sciLexer_api::delLineRight(native).

clauses
    lineDelete() :-
        sciLexer_api::lineDelete(native).

clauses
    lineCut() :-
        sciLexer_api::lineCut(native).

clauses
    lineCopy() :-
        sciLexer_api::lineCopy(native).

clauses
    lineTranspose() :-
        sciLexer_api::lineTranspose(native).

clauses
    lineDuplicate() :-
        sciLexer_api::lineDuplicate(native).

clauses
    lowerCase() :-
        sciLexer_api::lowerCase(native).

clauses
    upperCase() :-
        sciLexer_api::upperCase(native).

clauses
    cancel() :-
        sciLexer_api::cancel(native).

clauses
    editToggleOvertype() :-
        sciLexer_api::editToggleOvertype(native).

clauses
    newline() :-
        sciLexer_api::newline(native).

clauses
    formfeed() :-
        sciLexer_api::formfeed(native).

clauses
    tab() :-
        sciLexer_api::tab(native).

clauses
    backTab() :-
        sciLexer_api::backTab(native).

clauses
    selectionDuplicate() :-
        sciLexer_api::selectionDuplicate(native).

clauses
    verticalCentreCaret() :-
        sciLexer_api::verticalCentreCaret(native).

%  The SCI_*EXTEND messages extend the selection.
%  The SCI_*RECTEXTEND messages extend the rectangular selection (and convert regular selection to rectangular one, if any).
%  The SCI_WORDPART* commands are used to move between word segments marked by capitalisation (aCamelCaseIdentifier) or
%  underscores (an_under_bar_ident).
%  The SCI_HOME* commands move the caret to the start of the line, while the SCI_VCHOME*commands move the caret to the first non-
%  blank character of the line (ie. just after the indentation) unless it is already there; in this case, it acts as SCI_HOME*.
%  The SCI_[HOME|LINEEND]DISPLAY* commands are used when in line wrap mode to allow movement to the start or end of display lines
%  as opposed to the normal SCI_[HOME|LINEEND] commands which move to the start or end of document lines.
%  The SCI_[[VC]HOME|LINEEND]WRAP* commands are like their namesakes SCI_[[VC]HOME|LINEEND]* except they behave differently
%  when word-wrap is enabled: They go first to the start / end of the display line, like SCI_[HOME|LINEEND]DISPLAY*, but if the cursor is
%  already at the point, it goes on to the start or end of the document line, as appropriate for SCI_[[VC]HOME|LINEEND]*.
%  Key bindings
%  There is a default binding of keys to commands that is defined in the Scintilla source in the file KeyMap.cxx by the constant
%  KeyMap::MapDefault[]. This table maps key definitions to SCI_* messages with no parameters (mostly the keyboard commands
%  discussed above, but any Scintilla command that has no arguments can be mapped). You can change the mapping to suit your own
%  requirements.
%  keyDefinition
%  A key definition contains the key code in the low 16-bits and the key modifiers in the high 16-bits. To combine keyCode and key_mod
%  set:
%  keyDefinition = sciLexer_api::keyCode + (key_mod << 16)
%  The key code is a visible or control character or a key from the SCK_* enumeration, which contains:
%  SCK_ADD, SCK_BACK, SCK_DELETE, SCK_DIVIDE, SCK_DOWN, SCK_END, SCK_ESCAPE, SCK_HOME, SCK_INSERT, SCK_LEFT,
%  SCK_MENU, SCK_NEXT (Page Down), SCK_PRIOR (Page Up), SCK_RETURN, SCK_RIGHT, SCK_RWIN, SCK_SUBTRACT, SCK_TAB,
%  SCK_UP, and SCK_WIN.
%  The modifiers are a combination of zero or more of SCMOD_ALT, SCMOD_CTRL, and SCMOD_SHIFT. If you are building a table, you
%  might want to use SCMOD_NORM, which has the value 0, to mean no modifiers.
clauses
    assignCmdKey(KeyDefinition, SciCommand) :-
        sciLexer_api::assignCmdKey(native, KeyDefinition, SciCommand).
        %  This assignS the given key definition to a Scintilla command identified by sciCommand. sciCommand can be any SCI_* command that
        %  has no arguments.

clauses
    clearCmdKey(KeyDefinition) :-
        sciLexer_api::clearCmdKey(native, KeyDefinition).
        %  This makes the given key definition do nothing by assignIng the action SCI_NULL to it.

clauses
    clearAllCmdKeys() :-
        sciLexer_api::clearAllCmdKeys(native).
        %  This command removes all keyboard command mapping by setTing an empty mapping table.

%  Popup edit menu
%  Macro recording
%  Start and stop macro recording mode. In macro recording mode, actions are reported to the container through SCN_MACRORECORD
%  notifications. It is then up to the container to record these actions for future replay.
clauses
    startRecord() :-
        sciLexer_api::startRecord(native).

clauses
    stopRecord() :-
        sciLexer_api::stopRecord(native).
        %  These two messages turn macro recording on and off.

%  Printing
%  On Windows SCI_FORMATRANGE can be used to draw the text onto a display context which can include a printer display context.
%  Printed output shows text styling as on the screen, but it hides all margins except a line number margin. All special marker effects are
%  removed and the selection and caret are hidden.
clauses
    formatRange(BDraw, Pfr) :-
        sciLexer_api::formatRange(native, boolean::toBooleanInt(BDraw), Pfr).
        %  This call allows Windows users to render a range of text into a device context. If you use this for printing, you will probably want to
        %  arrange a page header and footer; Scintilla does not do this for you. See SciTEWin::Print() in SciTEWinDlg.cxx for an example. Each
        %  use of this message renders a range of text into a rectangular area and returns the position in the document of the next character to
        %  print.

%  bDraw controls if any output is done. Set this to false if you are paginating (for example, if you use this with MFC you will need to
%  paginate in OnBeginPrinting() before you output each page.
%  struct Sci_Rectangle { int left; int top; int right; int bottom; };
%  struct Sci_RangeToFormat {
%      Sci_SurfaceID hdc;        // The HDC (device context) we print to
%      Sci_SurfaceID hdcTarget;  // The HDC we use for measuring (may be same as hdc)
%      Sci_Rectangle rc;         // Rectangle in which to print
%      Sci_Rectangle rcPage;     // Physically printable page size
%      Sci_CharacterRange chrg;  // Range of characters to print
%  };
%  hdc and hdcTarget should both be set to the device context handle of the output device (usually a printer). If you print to a metafile
%  these will not be the same as Windows metafiles (unlike extended metafiles) do not implement the full API for returning information. In
%  this case, set hdcTarget to the screen DC.
%  rcPage is the rectangle {0, 0, maxX, maxY} where maxX+1 and maxY+1 are the number of physically printable pixels in x and y.
%  rc is the rectangle to render the text in (which will, of course, fit within the rectangle defined by rcPage).
%  chrg.cpMin and chrg.cpMax define the start position and maximum position of characters to output. All of each line within this character
%  range is drawn.
%  When printing, the most tedious part is always working out what the margins should be to allow for the non-printable area of the paper
%  and printing a header and footer. If you look at the printing code in SciTE, you will find that most of it is taken up with this. The loop
%  that causeS Scintilla to render text is quite simple if you strip out all the margin, non-printable area, header and footer code.
clauses
    printMagnification(Magnification) :-
        sciLexer_api::setPrintMagnification(native, Magnification).

clauses
    printMagnification() = sciLexer_api::getPrintMagnification(native).
        %  SCI_GETPRINTMAGNIFICATION lets you to print at a different size than the screen font. magnification is the number of points to add to
        %  the size of each screen font. A value of -3 or -4 gives reasonably small print. You can get this value with
        %  SCI_GETPRINTMAGNIFICATION.

clauses
    printColorMode(Mode) :-
        sciLexer_api::setPrintColorMode(native, Mode).

clauses
    printColorMode() = sciLexer_api::getPrintColorMode(native).
        %  These two messages set and get the method used to render coloured text on a printer that is probably using white paper. It is
        %  especially important to consider the treatment of colour if you use a dark or black screen background. Printing white on black useS up
        %  toner and ink very many times faster than the other way around. You can set the mode to one of:

%  Symbol Value Purpose
%  SC_PRINT_NORMAL 0 Print using the current screen colours. This is the default.
%  SC_PRINT_INVERTLIGHT 1 If you use a dark screen background this saves ink by inverting the light value of all colours and printing on
%  a white background.
%  SC_PRINT_BLACKONWHITE 2 Print all text as black on a white background.
%  SC_PRINT_COLOURONWHITE 3 Everything prints in its own colour on a white background.
%  SC_PRINT_COLOURONWHITEDEFAULTBG 4 Everything prints in its own colour on a white background except that line numbers use
%  their own background colour.
clauses
    printWrapMode(WrapMode) :-
        sciLexer_api::setPrintWrapMode(native, WrapMode).

clauses
    printWrapMode() = sciLexer_api::getPrintWrapMode(native).
        %  These two functions get and set the printer wrap mode. wrapMode can be set to SC_WRAP_NONE (0), SC_WRAP_WORD (1) or
        %  SC_WRAP_CHAR (2). The default is SC_WRAP_WORD, which wraps printed output so that all characters fit into the print rectangle. If
        %  you set SC_WRAP_NONE, each line of text generates one line of output and the line is truncated if it is too long to fit into the print area.
        %  SC_WRAP_WORD tries to wrap only between words as indicated by white space or style changes although if a word is longer than a
        %  line, it will be wrapped before the line end. SC_WRAP_CHAR is preferred to SC_WRAP_WORD for Asian languages where there is no
        %  white space between words.

%  Direct access
%  On Windows, the message-passing scheme used to communicate between the container and Scintilla is mediated by the operating
%  system SendMessage function and can lead to bad performance when calling intensively. To avoid this overhead, Scintilla provides
%  messages that allow you to call the Scintilla message function directly. The code to do this in C/C++ is of the form:
%  #include "Scintilla.h"
%  SciFnDirect pSciMsg = sciLexer_api::(SciFnDirect)SendMessage(hSciWnd, SCI_GETDIRECTFUNCTION, 0, 0);
%  sptr_t pSciWndData = sciLexer_api::(sptr_t)SendMessage(hSciWnd, SCI_GETDIRECTPOINTER, 0, 0);
%  // now a wrapper to call Scintilla directly
%  sptr_t CallScintilla(unsigned int iMessage, uptr_t wParam, sptr_t lParam){
%      return pSciMsg(pSciWndData, iMessage, wParam, lParam);
%  }
%  SciFnDirect, sptr_t and uptr_t are declared in Scintilla.h. hSciWnd is the window handle returned when you created the Scintilla window.
%  While faster, this direct calling will cause problems if performed from a different thread to the native thread of the Scintilla window in
%  which case SendMessage(hSciWnd, SCI_*, wParam, lParam) should be used to synchronize with the window's thread.
%  This feature also works on GTK+ but has no significant impact on speed.
%  From version 1.47 on Windows, Scintilla exports a function called Scintilla_DirectFunction that can be used the same as the function
%  returned by SCI_GETDIRECTFUNCTION. This saves you the call to SCI_GETDIRECTFUNCTION and the need to call Scintilla indirectly
%  via the function pointer.
%  This message returns the address of the function to call to handle Scintilla messages withOut the overhead of passing through the
%  Windows messaging system. You need only call this once, regardless of the number of Scintilla windows you create.
clauses
    getCharacterPointer() = sciLexer_api::getCharacterPointer(native).

clauses
    getRangePointer(Position, RangeLength) = sciLexer_api::getRangePointer(native, Position, RangeLength).

clauses
    getGapPosition() = sciLexer_api::getGapPosition(native).
        %  Move the gap within Scintilla so that the text of the document is stored consecutively and ensure there is a NUL character after the text,
        %  then return a pointer to the first character. Applications may then pass this to a function that accepts a character pointer such as a
        %  regular expression search or a parser. The pointer should not be written to as that may desynchronize the internal state of Scintilla.

%  Since any action in Scintilla may change its internal state this pointer becomes invalid after any call or by allowing user interface
%  activity. The application should reacquire the pointer after making any call to Scintilla or performing any user-interface calls such as
%  modifying a progress indicator.
%  This call takes similar time to inserting a character at the end of the document and this may include moving the document contents.
%  Specifically, all the characters after the document gap are moved to before the gap. This compacted state should persist over calls and
%  user interface actions that do not change the document contents so reacquiring the pointer afterwards is very quick. If this call is useD
%  to implement a global replace operation, then each replacement will move the gap so if SCI_GETCHARACTERPOINTER is called after
%  each replacement then the operation will become O(n^2) rather than O(n). Instead, all matches should be found and remembered,
%  then all the replacements performed.
%  Multiple views
%  A Scintilla window and the document that it displays are separate entities. When you create a new window, you also create a new,
%  empty document. Each document has a reference count that is initially set to 1. The document also has a list of the Scintilla windows
%  that are linked to it so when any window changes the document, all other windows in which it appears are notified to cause them to
%  update. The system is arranged in this way so that you can work with many documents in a single Scintilla window and so you can
%  display a single document in multiple windows (for use with splitter windows).
%  Although these messages use document *pDoc, to ensure compatibility with future releases of Scintilla you should treat pDoc as an
%  opaque void*. That is, you can use and store the pointer as described in this section but you should not dereference it.
clauses
    docPointer() = sciLexer_api::getDocPointer(native).
        %  This returns a pointer to the document currently in use by the window. It has no other effect.

clauses
    docPointer(PDoc) :-
        sciLexer_api::setDocPointer(native, PDoc).
        %  This message does the following:
        %  1. It removes the current window from the list held by the current document.
        %  2. It reduces the reference count of the current document by 1.
        %  3. If the reference count reaches 0, the document is deleted.
        %  4. pDoc is set as the new document for the window.
        %  5. If pDoc was 0, a new, empty document is created and attached to the window.
        %  6. If pDoc was not 0, its reference count is increased by 1.

clauses
    createDocument() :-
        sciLexer_api::createDocument(native).
        %  This message creates a new, empty document and returns a pointer to it. This document is not selected into the editor and starts with a
        %  reference count of 1. This means that you have ownership of it and must either reduce its reference count by 1 after using
        %  SCI_SETDOCPOINTER so that the Scintilla window owns it or you must make sure that you reduce the reference count by 1 with
        %  SCI_RELEASEDOCUMENT before you close the application to avoid memory leaks.

clauses
    addrefDocument(PDoc) :-
        sciLexer_api::addrefDocument(native, PDoc).
        %  This increases the reference count of a document by 1. If you want to replace the current document in the Scintilla window and take
        %  ownership of the current document, for example if you are editing many documents in one window, do the following:
        %  1. Use SCI_GETDOCPOINTER to get a pointer to the document, pDoc.
        %  2. Use SCI_ADDREFDOCUMENT(0, pDoc) to increment the reference count.
        %  3. Use SCI_SETDOCPOINTER(0, pNewDoc) to set a different document or SCI_SETDOCPOINTER(0, 0) to set a new, empty document.

clauses
    releaseDocument(PDoc) :-
        sciLexer_api::releaseDocument(native, PDoc).
        %  This message reduces the reference count of the document identified by pDoc. pDoc must be the result of SCI_GETDOCPOINTER or
        %  SCI_CREATEDOCUMENT and must point at a document that still exists. If you call this on a document with a reference count of 1 that is
        %  still attached to a Scintilla window, bad things will happen. To keep the world spinning in its orbit you must balance each call to
        %  SCI_CREATEDOCUMENT or SCI_ADDREFDOCUMENT with a call to SCI_RELEASEDOCUMENT.

%  Folding
%  The fundamental operation in folding is making lines Invisible or visible. Line visibility is a property of the view rather than the document
%  so each view may be displaying a different set of lines. From the point of view of the user, lines are hidden and displayed using fold
%  points. Generally, the fold points of a document are based on the hierarchical structure of the document contents. In Python, the
%  hierarchy is determined by indentation and in C++ by brace characters. This hierarchy can be represented within a Scintilla document
%  object by attaching a numeric "fold level" to each line. The fold level is most easily set by a lexer, but you can also set it with
%  messages.
%  It is up to your code to set the connection between user actions and folding and unFolding. The best way to see how this is done is to
%  search the SciTE source code for the messages used in this section of the documentation and see how they are useD. You will also need
%  to use markers and a folding margin to complete your folding implementation. The "fold" property should be set to "1" with
%  SCI_SETPROPERTY("fold", "1") to enable folding.
clauses
    visibleFromDocLine(DocLine) :-
        sciLexer_api::visibleFromDocLine(native, DocLine).
        %  When some lines are folded, then a particular line in the document may be displayed at a different position to its document position. If
        %  no lines are folded, this message returns docLine. Otherwise, this returns the display line (counting the very first visible line as 0). The
        %  display line of an Invisible line is the same as the previous visible line. The display line number of the first line in the document is 0. If
        %  there is folding and docLine is outside the range of lines in the document, the return value is -1. Lines can occupy more than one
        %  display line if they wrap.

clauses
    docLineFromVisible(DisplayLine) :-
        sciLexer_api::docLineFromVisible(native, DisplayLine).
        %  When some lines are hidden, then a particular line in the document may be displayed at a different position to its document position.
        %  This message returns the document line number that corresponds to a display line (counting the display line of the first line in the
        %  document as 0). If displayLine is less than or equal to 0, the result is 0. If displayLine is greater than or equal to the number of
        %  displayed lines, the result is the number of lines in the document.

clauses
    showLines(LineStart, LineEnd) :-
        sciLexer_api::showLines(native, fromLineNumber(LineStart), fromLineNumber(LineEnd)).

clauses
    hideLines(LineStart, LineEnd) :-
        sciLexer_api::hideLines(native, fromLineNumber(LineStart), fromLineNumber(LineEnd)).

clauses
    isLineVisible(Line) :-
        b_false <> sciLexer_api::getLineVisible(native, fromLineNumber(Line)).
        %  The first two messages mark a range of lines as visible or Invisible and then redraw the display. The third message reports on the
        %  visible state of a line and returns 1 if it is visible and 0 if it is not visible. These messages have no effect on fold levels or fold flags. The
        %  first line can not be hidden.

clauses
    isAllLinesVisible() = boolean::fromBooleanInt(sciLexer_api::getAllLinesVisible(native)).
        % returns true if all lines are visible and false if some lines are hidden.
        % It have no effect on fold levels or fold flags. The first line can not be hidden.

clauses
    setFoldLevel(Line, Level) :-
        sciLexer_api::setFoldLevel(native, fromLineNumber(Line), Level).

clauses
    getFoldLevel(Line) = sciLexer_api::getFoldLevel(native, fromLineNumber(Line)).
        %  These two messages set and get a 32-bit value that contains the fold level of a line and some flags associated with folding. The fold
        %  level is a number in the range 0 to SC_FOLDLEVELNUMBERMASK (4095). However, the initial fold level is set to SC_FOLDLEVELBASE
        %  (1024) to allow unsigned arithmetic on folding levels. There are two addition flag bits. SC_FOLDLEVELWHITEFLAG indicates that the line
        %  is blank and allows it to be treated sLightly different then its level may indicate. For example, blank lines should generally not be fold
        %  points and will be considered part of the preceding section even though they may have a lesser fold level. SC_FOLDLEVELHEADERFLAG
        %  indicates that the line is a header (fold point).

%  Use SCI_GETFOLDLEVEL(line) & SC_FOLDLEVELNUMBERMASK to get the fold level of a line. Likewise, use SCI_GETFOLDLEVEL(line) &
%  SC_FOLDLEVEL*FLAG to get the state of the flags. To set the fold level you must or in the associated flags. For instance, to set the level
%  to thisLevel and mark a line as being a fold point use: SCI_SETFOLDLEVEL(line, thisLevel | SC_FOLDLEVELHEADERFLAG).
%  If you use a lexer, you should not need to use SCI_SETFOLDLEVEL as this is far better handled by the lexer. You will need to use
%  SCI_GETFOLDLEVEL to decide how to handle user folding requests. If you do change the fold levels, the folding margin will update to
clauses
    foldFlags(Flags) :-
        sciLexer_api::setFoldFlags(native, Flags).
        %  In addition to showing markers in the folding margin, you can indicate folds to the user by drawing lines in the text area. The lines are
        %  drawn in the foreground colour set for STYLE_DEFAULT. Bits set in flags determine where folding lines are drawn:

%  Symbol Value Effect
%   1 Experimental feature that has been removed.
%  SC_FOLDFLAG_LINEBefore_EXPANDED 2 Draw above if expanded
%  SC_FOLDFLAG_LINEBefore_CONTRACTED 4 Draw above if not expanded
%  SC_FOLDFLAG_LINEAFTER_EXPANDED 8 Draw below if expanded
%  SC_FOLDFLAG_LINEAFTER_CONTRACTED 16 Draw below if not expanded
%  SC_FOLDFLAG_LEVELNUMBERS 64 display hexadecimal fold levels in line margin to aid debugging of folding. The appearance of this
%  feature may change in the future.
%  This message causeS the display to redraw.
clauses
    getLastChild(StartLine, Level) = toLineNumber(sciLexer_api::getLastChild(native, StartLine, Level)).
        %  This message searches for the next line after startLine, that has a folding level that is less than or equal to level and then returns the
        %  previous line number. If you set level to -1, level is set to the folding level of line startLine. If from is a fold point,
        %  SCI_GETLASTCHILD(from, -1) returns the last line that would be in made visible or hidden by toggling the fold state.

clauses
    getFoldParent(StartLine) = toLineNumber(sciLexer_api::getFoldParent(native, StartLine)).
        %  This message returns the line number of the first line before startLine that is marked as a fold point with SC_FOLDLEVELHEADERFLAG
        %  and has a fold level less than the startLine. If no line is found, or if the header flags and fold levels are inconsistent, the return value is
        %  -1.

clauses
    toggleFold(Line) :-
        sciLexer_api::toggleFold(native, fromLineNumber(Line)).
        %  Each fold point may be either expanded, displaying all its child lines, or contracted, hiding all the child lines. This message toggles the
        %  folding state of the given line as long as it has the SC_FOLDLEVELHEADERFLAG set. This message takes care of folding or expanding all
        %  the lines that depend on the line. The display updates after this message.

clauses
    setFoldExpanded(Line, Expanded) :-
        sciLexer_api::setFoldExpanded(native, fromLineNumber(Line), boolean::toBooleanInt(Expanded)).

clauses
    getFoldExpanded(Line) = boolean::fromBooleanInt(sciLexer_api::getFoldExpanded(native, fromLineNumber(Line))).
        %  These messages set and get the expanded state of a single line. The set message has no effect on the visible state of the line or any
        %  lines that depend on it. It does change the markers in the folding margin. If you ask for the expansion state of a line that is outside the
        %  document, the result is false (0).

%  If you just want to toggle the fold state of one line and handle all the lines that are dependent on it, it is much easier to use
%  SCI_TOGGLEFOLD. You would use the SCI_SETFOLDEXPANDED message to process many folds withOut updating the display until you
%  had finished. See SciTEBase::FoldAll() and SciTEBase::Expand() for examples of the use of these messages.
clauses
    contractedFoldNext(LineStart) :-
        sciLexer_api::contractedFoldNext(native, fromLineNumber(LineStart)).
        %  Search efficiently for lines that are contracted fold headers. This is useFul when saving the user's folding when switching documents or
        %  saving folding with a file. The search starts at line number lineStart and continues forwards to the end of the file. lineStart is returned if
        %  it is a contracted fold header otherwise the next contracted fold header is returned. If there are no more contracted fold headers then
        %  -1 is returned.

clauses
    ensureVisible(Line) :-
        sciLexer_api::ensureVisible(native, fromLineNumber(Line)).

clauses
    ensureVisibleEnforcePolicy(Line) :-
        sciLexer_api::ensureVisibleEnforcePolicy(native, fromLineNumber(Line)).
        %  A line may be hidden because more than one of its parent lines is contracted. Both these message travels up the fold hierarchy,
        %  expanding any contracted folds until they reach the top level. The line will then be visible. If you use
        %  SCI_ENSUREVISIBLEENFORCEPOLICY, the vertical caret policy set by SCI_SETVISIBLEPOLICY is then applied.

%  Line wrapping
%  By default, Scintilla does not wrap lines of text. If you enable line wrapping, lines wider than the window width are continued on the
%  following lines. Lines are broken after space or tab characters or between runs of different styles. If this is not possible because a word
%  in one style is wider than the window then the break occurs after the last character that completely fits on the line. The horizontal scroll
%  bar does not appear when wrap mode is on.
%  For wrapped lines Scintilla can draw visual flags (little arrows) at end of a a subline of a wrapped line and at begin of the next subline.
%  These can be enabled individually, but if Scintilla draws the visual flag at the beginning of the next subline this subline will be indented
%  by one char. Independent from drawing a visual flag at the begin the subline can have an indention.
%  Much of the time used by Scintilla is spent on laying out and drawing text. The same text layOut calculations may be performed many
%  times even when the data used in these calculations does not change. To avoid these unnecessary calculations in some circumstances,
%  the line layOut cache can store the results of the calculations. The cache is invalidated whenever the underlying data, such as the
%  contents or styling of the document changes. Caching the layOut of the whole document has the most effect, making dynamic line wrap
%  as much as 20 times faster but this requires 7 times the memory required by the document contents plus around 80 bytes per line.
%  WrapPing is not performed immediately there is a change but is delayed until the display is redrawn. This delay improves peformance
%  by allowing a set of changes to be performed and then wrapped and displayed once. Because of this, some operations may not occur
%  as expected. If a file is read and the scroll position moved to a particular line in the text, such as occurs when a container tries to
%  restore a previous editing session, then the scroll position will have been determined before wrapping so an unexpected range of text
%  will be displayed. To scroll to the position correctly, delay the scroll until the wrapping has been performed by waiting for an initial
%  SCN_PAINTED notification.
clauses
    wrapMode(WrapMode) :-
        sciLexer_api::setWrapMode(native, WrapMode).

clauses
    wrapMode() = sciLexer_api::getWrapMode(native).
        %  Set wrapMode to SC_WRAP_WORD (1) to enable wrapping on word boundaries, SC_WRAP_CHAR (2) to enable wrapping between any
        %  characters, and to SC_WRAP_NONE (0) to disable line wrapping. SC_WRAP_CHAR is preferred to SC_WRAP_WORD for Asian languages
        %  where there is no white space between words.

clauses
    wrapVisualFlags(WrapVisualFlags) :-
        sciLexer_api::setWrapVisualFlags(native, WrapVisualFlags).

clauses
    wrapVisualFlags() = sciLexer_api::getWrapVisualFlags(native).
        %  You can enable the drawing of visual flags to indicate a line is wrapped. Bits set in wrapVisualFlags determine which visual flags are
        %  drawn.

%  Symbol Value Effect
%  SC_WRAPVISUALFLAG_NONE 0 No visual flags
%  SC_WRAPVISUALFLAG_END 1 Visual flag at end of subline of a wrapped line.
%  SC_WRAPVISUALFLAG_START 2 Visual flag at begin of subline of a wrapped line.
%  Subline is indented by at least 1 to make room for the flag.
clauses
    wrapVisualFlagsLocation(WrapVisualFlagsLocation) :-
        sciLexer_api::setWrapVisualFlagsLocation(native, WrapVisualFlagsLocation).

clauses
    wrapVisualFlagsLocation() = sciLexer_api::getWrapVisualFlagsLocation(native).
        %  You can set whether the visual flags to indicate a line is wrapped are drawn near the border or near the text. Bits set in
        %  wrapVisualFlagsLocation set the location to near the text for the corresponding visual flag.

%  Symbol Value Effect
%  SC_WRAPVISUALFLAGLOC_DEFAULT 0 Visual flags drawn near border
%  SC_WRAPVISUALFLAGLOC_END_BY_TEXT 1 Visual flag at end of subline drawn near text
%  SC_WRAPVISUALFLAGLOC_START_BY_TEXT 2 Visual flag at beginning of subline drawn near text
clauses
    wrapIndentMode(IndentMode) :-
        sciLexer_api::setWrapIndentMode(native, IndentMode).

clauses
    wrapIndentMode() = sciLexer_api::getWrapIndentMode(native).
        %  WrapPed sublines can be indented to the position of their first subline or one more indent level. The default is SC_WRAPINDENT_FIXED.
        %  The modes are:

%  Symbol Value Effect
%  SC_WRAPINDENT_FIXED 0 WrapPed sublines aligned to left of window plus amount set by SCI_SETWRAPSTARTINDENT
%  SC_WRAPINDENT_SAME 1 WrapPed sublines are aligned to first subline indent
%  SC_WRAPINDENT_INDENT 2 WrapPed sublines are aligned to first subline indent plus one more level of indentation
clauses
    wrapStartIndent(Indent) :-
        sciLexer_api::setWrapStartIndent(native, Indent).

clauses
    wrapStartIndent() = sciLexer_api::getWrapStartIndent(native).
        %  SCI_SETWRAPSTARTINDENT setS the size of indentation of sublines for wrapped lines in terms of the average character width in
        %  STYLE_DEFAULT. There are no limits on indent sizes, but values less than 0 or large values may have undesirable effects.
        %  The indention of sublines is independent of visual flags, but if SC_WRAPVISUALFLAG_START is set an indent of at least 1 is useD.

clauses
    layOutCache(CacheMode) :-
        sciLexer_api::setLayOutCache(native, CacheMode).

clauses
    layOutCache() = sciLexer_api::getLayOutCache(native).
        %  You can set cacheMode to one of the symbols in the table:

%  Symbol Value LayOut cached for these lines
%  SC_CACHE_NONE 0 No lines are cached.
%  SC_CACHE_CARET 1 The line containing the text caret. This is the default.
%  SC_CACHE_PAGE 2 Visible lines plus the line containing the caret.
%  SC_CACHE_DOCUMENT 3 All lines in the document.
clauses
    positionCache(Size) :-
        sciLexer_api::setPositionCache(native, Size).

clauses
    positionCache() = sciLexer_api::getPositionCache(native).
        %  The position cache stores position information for short runs of text so that their layOut can be determined more quickly if the run
        %  recurs. The size in entries of this cache can be set with SCI_SETPOSITIONCACHE.

clauses
    linesSplit(PixelWidth) :-
        sciLexer_api::linesSplit(native, PixelWidth).
        %  Split a range of lines indicated by the target into lines that are at most pixelWidth wide. Splitting occurs on word boundaries wherever
        %  possible in a similar manner to line wrapping. When pixelWidth is 0 then the width of the window is useD.

clauses
    linesJoin() :-
        sciLexer_api::linesJoin(native).
        %  Join a range of lines indicated by the target into one line by removing line end characters. Where this would lead to no space between
        %  words, an extra space is inserted.

clauses
    wrapCount(DocLine) :-
        sciLexer_api::wrapCount(native, DocLine).
        %  Document lines can occupy more than one display line if they wrap and this returns the number of display lines needed to wrap a
        %  document line.

%  Zooming
%  Scintilla incorporates a "zoom factor" that lets you make all the text in the document larger or smaller in steps of one point. The
%  displayed point size never goes below 2, whatever zoom factor you set. You can set zoom factors in the range -10 to +20 points.
clauses
    zoomIn() :-
        sciLexer_api::zoomIn(native).

clauses
    zoomOut() :-
        sciLexer_api::zoomOut(native).
        %  SCI_ZOOMIN increases the zoom factor by one point if the current zoom factor is less than 20 points. SCI_ZOOMOUT decreases the
        %  zoom factor by one point if the current zoom factor is greater than -10 points.

clauses
    zoom(ZoomInPoints) :-
        sciLexer_api::setZoom(native, ZoomInPoints).

clauses
    zoom() = sciLexer_api::getZoom(native).
        %  These messages let you set and get the zoom factor directly. There is no limit set on the factors you can set, so limiting yourself to -10
        %  to +20 to match the incremental zoom functions is a good idea.

%  Long lines
%  You can choose to mark lines that exceed a given length by drawing a vertical line or by colouring the background of characters that
%  exceed the set length.
clauses
    edgeMode(EdgeMode) :-
        sciLexer_api::setEdgeMode(native, EdgeMode).

clauses
    edgeMode() = sciLexer_api::getEdgeMode(native).
        %  These two messages set and get the mode used to display long lines. You can set one of the values in the table:

%  Symbol Value Long line display mode
%  EDGE_NONE 0 Long lines are not marked. This is the default state.
%  EDGE_LINE 1 A vertical line is drawn at the column number set by SCI_SETEDGECOLUMN. This works well for monospaced fonts. The
%  line is drawn at a position based on the width of a space character in STYLE_DEFAULT, so it may not work very well if your styles use
%  proportional fonts or if your style have varied font sizes or you use a mixture of bold, italic and normal text.
%  EDGE_BACKGROUND 2 The background colour of characters after the column limit is changed to the colour set by
%  SCI_SETEDGECOLOUR. This is recommended for proportional fonts.
clauses
    edgeColumn(Column) :-
        sciLexer_api::setEdgeColumn(native, Column).

clauses
    edgeColumn() = sciLexer_api::getEdgeColumn(native).
        %  These messages set and get the column number at which to display the long line marker. When drawing lines, the column setS a
        %  position in units of the width of a space character in STYLE_DEFAULT. When setTing the background colour, the column is a character
        %  count (allowing for tabs) into the line.

clauses
    edgeColor(Color) :-
        sciLexer_api::setEdgeColor(native, Color).

clauses
    edgeColor() = sciLexer_api::getEdgeColor(native).
        %  These messages set and get the colour of the marker used to show that a line has exceeded the length set by SCI_SETEDGECOLUMN.

%  Lexer
%  If you define the symbol SCI_LEXER when building Scintilla, (this is someTimes called the SciLexer version of Scintilla), lexing support
%  for a wide range of programming languages is included and the messages in this section are supported. If you want to set styling and
%  fold points for an unsupported language you can either do this in the container or better still, write your own lexer following the pattern
%  of one of the existing ones.
%  Scintilla also supports external lexers. These are DLLs (on Windows) or .so modules (on GTK+/Linux) that export three functions:
%  GetLexerCount, GetLexerName, and GetLexerFactory. See externalLexer.cxx for more.
clauses
    lexer(Lexer) :-
        sciLexer_api::setLexer(native, Lexer).

clauses
    lexer() = sciLexer_api::getLexer(native).
        %  You can select the lexer to use with an integer code from the SCLEX_* enumeration in Scintilla.h. There are two codes in this sequence
        %  that do not use lexers: SCLEX_NULL to select no lexing action and SCLEX_CONTAINER which sends the SCN_STYLENEEDED notification
        %  to the container whenever a range of text needs to be styled. You cannot use the SCLEX_AUTOMATIC value; this identifies additional
        %  external lexers that Scintilla assignS unused lexer numbers to.

clauses
    lexerLanguage(Name) :-
        sciLexer_api::setLexerLanguage(native, string8::toUtf8(Name)).

clauses
    lexerLanguage() = string8::fromUtf8(Name8) :-
        Name8 = string8::create(string8_defaultLength),
        sciLexer_api::getLexerLanguage(native, Name8).
        %  SCI_SETLEXERLANGUAGE lets you select a lexer by name, and is the only method if you are using an external lexer or if you have
        %  written a lexer module for a language of your own and do not wish to assign it an explicit lexer number. To select an existing lexer, set
        %  name to match the (case sensitive) name given to the module, for example "ada" or "python", not "Ada" or "Python". To locate the
        %  name for the built-in lexers, open the relevant Lex*.cxx file and search for LexerModule. The third argument in the LexerModule
        %  constructor is the name to use.

%  To test if your lexer assignment worked, use SCI_GETLEXER before and after setTing the new lexer to see if the lexer number changed.
%  SCI_GETLEXERLANGUAGE retrieves the name of the lexer.
clauses
    loadLexerLibrary(Path) :-
        sciLexer_api::loadLexerLibrary(native, string8::toUtf8(Path)).
        %  Load a lexer implemented in a shared library. This is a .so file on GTK+/Linux or a .DLL file on Windows.

clauses
    colorise(StartPos, EndPos) :-
        sciLexer_api::colorise(native, StartPos, EndPos).
        %  This requests the current lexer or the container (if the lexer is set to SCLEX_CONTAINER) to style the document between startPos and
        %  endPos. If endPos is -1, the document is styled from startPos to the end. If the "fold" property is set to "1" and your lexer or container
        %  supports folding, fold levels are also set. This message causeS a redraw.

clauses
    changeLexerState(StartPos, EndPos) :-
        sciLexer_api::changeLexerState(native, StartPos, EndPos).
        %  Indicate that the internal state of a lexer has changed over a range and therefore there may be a need to redraw.

clauses
    propertyNames() = string8::fromUtf8(Names8) :-
        L = sciLexer_api::propertyNames(native, string8::null),
        Names8 = string8::create(L),
        _ = sciLexer_api::propertyNames(native, Names8).

clauses
    propertyType(Name) :-
        sciLexer_api::propertyType(native, string8::toUtf8(Name)).

clauses
    describeProperty(Name) = string8::fromUtf8(Description8) :-
        Name8 = string8::toUtf8(Name),
        L = sciLexer_api::describeProperty(native, Name8, string8::null),
        Description8 = string8::create(L),
        _ = sciLexer_api::describeProperty(native, Name8, Description8).

%  Information may be retrieved about the properties that can be set for the current lexer. This information is only available for newer
%  lexers. SCI_PROPERTYNAMES returns a string with all of the valid properties separated by "\n". If the lexer does not support this call
%  then an empty string is returned. Properties may be boolean (SC_TYPE_BOOLEAN), integer (SC_TYPE_INTEGER), or string
%  (SC_TYPE_STRING) and this is found with SCI_PROPERTYTYPE. A description of a property in English is returned by
%  SCI_DESCRIBEPROPERTY.
clauses
    setProperty(Key, Value) :-
        sciLexer_api::setProperty(native, string8::toUtf8(Key), string8::toUtf8(Value)).
        %  You can communicate setTings to lexers with keyword:value string pairs. There is no limit to the number of keyword pairs you can set,
        %  other than available memory. key is a case sensitive keyword, value is a string that is associated with the keyword. If there is already
        %  a value string associated with the keyword, it is replaced. If you pass a zero length string, the message does nothing. Both key and
        %  value are used withOut modification; extra spaces at the beginning or end of key are significant.

%  The value string can refer to other keywords. For example, SCI_SETPROPERTY("foldTimes10", "$(fold)0") stores the string "$(fold)0",
%  but when this is accessed, the $(fold) is replaced by the value of the "fold" keyword (or by nothing if this keyword does not exist).
%  Currently the "fold" property is defined for most of the lexers to set the fold structure if set to "1". SCLEX_PYTHON understands
%  "tab.timmy.whinge.level" as a setTing that determines how to indicate bad indentation. Most keywords have values that are interpreted
%  as integers. Search the lexer sources for GetPropertyInt to see how properties are useD.
%  There is a convention for naming properties used by lexers so that the set of properties can be found by scripts. Property names should
%  start with "lexer.<lexer>." or "fold.<lexer>." when they apply to one lexer or start with "lexer." or "fold." if they apply to multiple
%  lexers.
%  Applications may discover the set of properties used by searching the source code of lexers for lines that contain GetProperty and a
%  double quoted string and extract the value of the double quoted string as the property name. The scintilla/src/LexGen.py script does
%  this and can be used as an example. Documentation for the property may be located above the call as a multi-line comment starting
%  with
%  // property <property-name>
clauses
    tryGetProperty(Key) = string8::fromUtf8(Value8) :-
        Key8 = string8::toUtf8(Key),
        L = sciLexer_api::getProperty(native, Key8, string8::null),
        0 <> L,
        Value8 = string8::create(L),
        _ = sciLexer_api::getProperty(native, Key8, Value8).
        %  Lookup a keyword:value pair using the specified key; if found, copy the value to the user-supplied buffer and return the length (not
        %  including the terminating 0). If not found, copy an empty string to the buffer and return 0.

%  Note that "keyword replacement" as described in SCI_SETPROPERTY will not be performed.
%  If the value argument is 0 then the length that should be allocated to store the value is returned; again, the terminating 0 is not
%  included.
clauses
    tryGetPropertyExpanded(Key) = string8::fromUtf8(Value8) :-
        Key8 = string8::toUtf8(Key),
        L = sciLexer_api::getPropertyExpanded(native, Key8, string8::null),
        0 <> L,
        Value8 = string8::create(L),
        _ = sciLexer_api::getPropertyExpanded(native, Key8, Value8).
        %  Lookup a keyword:value pair using the specified key; if found, copy the value to the user-supplied buffer and return the length (not
        %  including the terminating 0). If not found, copy an empty string to the buffer and return 0.

%  Note that "keyword replacement" as described in SCI_SETPROPERTY will be performed.
%  If the value argument is 0 then the length that should be allocated to store the value (including any indicated keyword replacement) is
%  returned; again, the terminating 0 is not included.
clauses
    getPropertyInt(Key, Default) = sciLexer_api::getPropertyInt(native, string8::toUtf8(Key), Default).
        %  Lookup a keyword:value pair using the specified key; if found, interpret the value as an integer and return it. If not found (or the value
        %  is an empty string) then return the supplied default. If the keyword:value pair is found but is not a number, then return 0.

%  Note that "keyword replacement" as described in SCI_SETPROPERTY will be performed before any numeric interpretation.
clauses
    setKeyWords(KeyWordSet, KeyWordList) :-
        sciLexer_api::setKeyWords(native, KeyWordSet, string8::toUtf8(KeyWordList)).
        %  You can set up to 9 lists of keywords for use by the current lexer. This was increased from 6 at revision 1.50. keyWordSet can be 0 to 8
        %  (actually 0 to KEYWORDSET_MAX) and selects which keyword list to replace. keyWordList is a list of keywords separated by spaces,
        %  tabs, "\n" or "\r" or any combination of these. It is expected that the keywords will be composed of standard ASCII printing characters,
        %  but there is nothing to stop you using any non-separator character codes from 1 to 255 (except common sense).

%  How these keywords are used is entirely up to the lexer. Some languages, such as HTML may contain embedded languages, VBScript
%  and JavaScript are common for HTML. For HTML, key word set 0 is for HTML, 1 is for JavaScript and 2 is for VBScript, 3 is for Python, 4
%  is for PHP and 5 is for SGML and DTD keywords. Review the lexer code to see examples of keyword list. A fully conforming lexer setS
%  the fourth argument of the LexerModule constructor to be a list of strings that describe the useS of the keyword lists.
%  Alternatively, you might use set 0 for general keywords, set 1 for keywords that cause indentation and set 2 for keywords that cause
%  unindentation. Yet again, you might have a simple lexer that colours keywords and you could change languages by changing the
%  keywords in set 0. There is nothing to stop you building your own keyword lists into the lexer, but this means that the lexer must be
%  rebuilt if more keywords are added.
clauses
    describeKeyWordSets(Descriptions) :-
        sciLexer_api::describeKeyWordSets(native, string8::toUtf8(Descriptions)).
        %  A description of all of the keyword setS separated by "\n" is returned by SCI_DESCRIBEKEYWORDSETS.

%  Lexer Objects
%  Lexers are programmed as objects that implement the ILexer interface and that interact with the document they are lexing through the
%  IDocument interface. Previously lexers were defined by providing lexing and folding functions but creating an object to handle the
%  interaction of a lexer with a document allows the lexer to store state information that can be used during lexing. For example a C++
%  lexer may store a set of preprocessor definitions or variable declarations and style these depending on their role.
%  A set of helper classes allows older lexers defined by functions to be used in Scintilla.
%  ILexer
%  class ILexer {
%  public:
%      virtual int SCI_METHOD Version() const = sciLexer_api::0;
%      virtual void SCI_METHOD Release() = sciLexer_api::0;
%      virtual const char * SCI_METHOD PropertyNames() = sciLexer_api::0;
%      virtual int SCI_METHOD PropertyType(const char *name) = sciLexer_api::0;
%      virtual const char * SCI_METHOD DescribeProperty(const char *name) = sciLexer_api::0;
%      virtual int SCI_METHOD PropertySet(const char *key, const char *val) = sciLexer_api::0;
%      virtual const char * SCI_METHOD DescribeWordListSets() = sciLexer_api::0;
%      virtual int SCI_METHOD WordListSet(int n, const char *wl) = sciLexer_api::0;
%      virtual void SCI_METHOD Lex(unsigned int startPos, int lengthDoc, int initStyle, IDocument *pAccess) = sciLexer_api::0;
%      virtual void SCI_METHOD Fold(unsigned int startPos, int lengthDoc, int initStyle, IDocument *pAccess) = sciLexer_api::0;
%      virtual void * SCI_METHOD PrivateCall(int operation, void *pointer) = sciLexer_api::0;
%  };
%  The return values from PropertySet and WordListSet are used to indicate whether the change requires performing lexing or folding over
%  any of the document. It is the position at which to restart lexing and folding or -1 if the change does not require any extra work on the
%  document. A simple approach is to return 0 if there is any possibility that a change requires lexing the document again while an
%  optimisation could be to remember where a setTing first affects the document and return that position.
%  Release is called to destroy the lexer object.
%  PrivateCall allows for direct communication between the application and a lexer. An example would be where an application maintains a
%  single large data structure containing symbolic information about system headers (like Windows.h) and provides this to the lexer where
%  it can be applied to each document. This avoids the costs of constructing the system header information for each document. This is
%  invoked with the SCI_PRIVATELEXERCALL API.
%  IDocument
%  class IDocument {
%  public:
%      virtual int SCI_METHOD Version() const = sciLexer_api::0;
%      virtual void SCI_METHOD SetErrorStatus(int status) = sciLexer_api::0;
%      virtual int SCI_METHOD Length() const = sciLexer_api::0;
%      virtual void SCI_METHOD GetCharRange(char *buffer, int position, int lengthRetrieve) const = sciLexer_api::0;
%      virtual char SCI_METHOD StyleAt(int position) const = sciLexer_api::0;
%      virtual int SCI_METHOD LineFromPosition(int position) const = sciLexer_api::0;
%      virtual int SCI_METHOD LineStart(int line) const = sciLexer_api::0;
%      virtual int SCI_METHOD GetLevel(int line) const = sciLexer_api::0;
%      virtual int SCI_METHOD SetLevel(int line, int level) = sciLexer_api::0;
%      virtual int SCI_METHOD GetLineState(int line) const = sciLexer_api::0 ;
%      virtual int SCI_METHOD SetLineState(int line, int state) = sciLexer_api::0;
%      virtual void SCI_METHOD StartStyling(int position, char mask) = sciLexer_api::0;
%      virtual bool SCI_METHOD SetStyleFor(int length, char style) = sciLexer_api::0 ;
%      virtual bool SCI_METHOD SetStyles(int length, const char *styles) = sciLexer_api::0;
%      virtual void SCI_METHOD DecorationSetCurrentIndicator(int indicator) = sciLexer_api::0;
%      virtual void SCI_METHOD DecorationFillRange(int position, int value, int fillLength) = sciLexer_api::0;
%      virtual void SCI_METHOD ChangeLexerState(int start, int end) = sciLexer_api::0;
%      virtual int SCI_METHOD CodePage() const = sciLexer_api::0 ;
%      virtual bool SCI_METHOD IsDBCSLeadByte(char ch) const = sciLexer_api::0;
%  };
%  Scintilla tries to minimize the consequences of modifying text to only relex and redraw the line of the change where possible. Lexer
%  objects contain their own private extra state which can affect later lines. For example, if the C++ lexer is greying out inactive code
%  segments then changing the statement #define BEOS 0 to #define BEOS 1 may require restyling and redisplaying later parts of the
%  document. The lexer can call ChangeLexerState to signal to the document that it should relex and display more.
%  SetErrorStatus is used to notify the document of exceptions. Exceptions should not be thrown over build boundaries as the two sides
%  may be built with different compilers or incompatible exception options.
%  The ILexer and IDocument interfaces may be expanded in the future with extended versions (ILexer2...). The Version method indicates
%  which interface is implemented and thus which methods may be called.
%  Notifications
%  Notifications are sent (fired) from the Scintilla control to its container when an event has occurred that may interest the container.
%  Notifications are sent using the WM_NOTIFY message on Windows and the "notify" signal on GTK+. The container is passed a
%  SCNotification structure containing information about the event.
%  struct NotifyHeader {   // This matches the Win32 NMHDR structure
%      void *hwndFrom;     // environment specific window handle/pointer
%      uptr_t idFrom;        // CtrlID of the window issuing the notification
%      unsigned int code;  // The SCN_* notification code
%  };
%  struct SCNotification {
%      struct NotifyHeader nmhdr;
%      int position;
%      // SCN_STYLENEEDED, SCN_DOUBLECLICK, SCN_MODIFIED, SCN_DWELLSTART,
%      // SCN_DWELLEND, SCN_CALLTIPCLICK,
%      // SCN_HotSpotCLICK, SCN_HotSpotDOUBLECLICK, SCN_HotSpotRELEASECLICK
%      int ch;             // SCN_CHARADDED, SCN_KEY
%      int modifiers;
%      // SCN_KEY, SCN_DOUBLECLICK, SCN_HotSpotCLICK, SCN_HotSpotDOUBLECLICK, SCN_HotSpotRELEASECLICK
%      int modificationType; // SCN_MODIFIED
%      const char *text;   // SCN_MODIFIED, SCN_USERLISTSELECTION, SCN_AUTOCSELECTION
%      int length;         // SCN_MODIFIED
%      int linesAdded;     // SCN_MODIFIED
%      int message;        // SCN_MACRORECORD
%      uptr_t wParam;      // SCN_MACRORECORD
%      sptr_t lParam;      // SCN_MACRORECORD
%      int line;           // SCN_MODIFIED, SCN_DOUBLECLICK
%      int foldLevelNow;   // SCN_MODIFIED
%      int foldLevelPrev;  // SCN_MODIFIED
%      int margin;         // SCN_MARGINCLICK
%      int listType;       // SCN_USERLISTSELECTION, SCN_AUTOCSELECTION
%      int x;              // SCN_DWELLSTART, SCN_DWELLEND
%      int y;              // SCN_DWELLSTART, SCN_DWELLEND
%      int token;           // SCN_MODIFIED with SC_MOD_CONTAINER
%      int annotationLinesAdded;    // SC_MOD_CHANGEANNOTATION
%      int updated; // SCN_UPDATEUI
%  };
%  The notification messages that your container can choose to handle and the messages associated with them are:
clauses
    dispatchWmNotify(Notification, NMHDR) = window::defaultNativeHandling :-
        if Handler = tryGetNotificationHandler(Notification) then
            Handler(uncheckedConvert(scNotification, NMHDR))
        end if.

predicates
    tryGetNotificationHandler : (unsigned Code) -> predicate{scNotification} Handler determ.
clauses
    % tryGetNotificationHandler(scen_change) = scenChange. % Doesn't seem to come through VPI (not needed anyway)
    % tryGetNotificationHandler(scen_setFocus) = scenSetFocus.
    % tryGetNotificationHandler(scen_killFocus) = scenKillFocus.
    tryGetNotificationHandler(scn_styleNeeded) = scnStyleNeeded.
    tryGetNotificationHandler(scn_charAdded) = scnCharAdded.
    tryGetNotificationHandler(scn_savepointReached) = scnSavepointReached.
    tryGetNotificationHandler(scn_savepointLeft) = scnSavepointLeft.
    tryGetNotificationHandler(scn_modifyAttemptRO) = scnModifyAttemptRO.
        % tryGetNotificationHandler(scn_key) = scnKey.  Not sent on Windows
    tryGetNotificationHandler(scn_doubleClick) = scnDoubleClick.
    tryGetNotificationHandler(scn_updateUI) = scnUpdateUI.
    tryGetNotificationHandler(scn_modified) = scnModified.
    tryGetNotificationHandler(scen_setfocus) = scenSetfocus.
    tryGetNotificationHandler(scen_killfocus) = scenKillfocus.
    tryGetNotificationHandler(scn_macroRecord) = scnMacroRecord.
    tryGetNotificationHandler(scn_marginClick) = scnMarginClick.
    tryGetNotificationHandler(scn_needShown) = scnNeedShown.
    tryGetNotificationHandler(scn_painted) = scnPainted.
    tryGetNotificationHandler(scn_userListSelection) = scnUserListSelection.
        % tryGetNotificationHandler(scn_uriDropped) = scnUriDropped. Not sent on Windows
    tryGetNotificationHandler(scn_dwellStart) = scnDwellStart.
    tryGetNotificationHandler(scn_dwellEnd) = scnDwellEnd.
    tryGetNotificationHandler(scn_zoom) = scnZoom.
    tryGetNotificationHandler(scn_hotspotClick) = scnHotspotClick.
    tryGetNotificationHandler(scn_hotspotDoubleClick) = scnHotspotDoubleClick.
    tryGetNotificationHandler(scn_hotspotReleaseClick) = scnHotspotReleaseClick.
    tryGetNotificationHandler(scn_callTipClick) = scnCallTipClick.
    tryGetNotificationHandler(scn_autocSelection) = scnAutoCSelection.
    tryGetNotificationHandler(scn_indicatorClick) = scnIndicatorClick.
    tryGetNotificationHandler(scn_indicatorRelease) = scnIndicatorRelease.
    tryGetNotificationHandler(scn_autoCCancelled) = scnAutoCCancelled.
    tryGetNotificationHandler(scn_autoCCharDeleted) = scnAutoCCharDeleted.

class predicates
    notification_position : (scNotification Notification) -> integer Position.
clauses
    notification_position(scNotification(:Position = Position | _)) = convert(integer, Position).

class predicates
    notification_ch : (scNotification Notification) -> integer Ch.
clauses
    notification_ch(scNotification(:Ch = Ch | _)) = Ch.

%  SCN_STYLENEEDED
%  SCN_CHARADDED
%  SCN_SAVEPOINTREACHED
%  SCN_SAVEPOINTLEFT
%  SCN_MODIFYATTEMPTRO
%  SCN_KEY
%  SCN_DOUBLECLICK
%  SCN_UPDATEUI
%  SCN_MODIFIED
%  SCN_MACRORECORD
%  SCN_MARGINCLICK
%  SCN_NEEDSHOWN
%  SCN_PAINTED
%  SCN_USERLISTSELECTION
%  SCN_URIDROPPED
%  SCN_DWELLSTART
%  SCN_DWELLEND
%  SCN_ZOOM
%  SCN_HotSpotCLICK
%  SCN_HotSpotDOUBLECLICK
%  SCN_HotSpotRELEASECLICK
%  SCN_INDICATORCLICK
%  SCN_INDICATORRELEASE
%  SCN_CALLTIPCLICK
%  SCN_AUTOCSELECTION
%  SCN_AUTOCCANCELLED
%  SCN_AUTOCCHARDELETED
%  The following SCI_* messages are associated with these notifications:
%  The following additional notifications are sent using the WM_COMMAND message on Windows and the "Command" signal on GTK+. This
%  emulates the Windows Edit control. Only the lower 16 bits of the control's ID is passed in these notifications.
%  SCEN_CHANGE
%  SCEN_SETFOCUS
%  SCEN_KILLFOCUS
facts
    styleNeededEvent : event2{integer FromPos, integer ToPos} := event2::new().

predicates
    scnStyleNeeded : (scNotification Notification).
clauses
    scnStyleNeeded(Notification) :-
        StartPos = positionFromLine(lineFromPosition(endStyled)),
        EndPos = notification_position(Notification),
        styleNeededEvent:notify(StartPos, EndPos).

%  SCN_STYLENEEDED
%  If you used SCI_SETLEXER(SCLEX_CONTAINER) to make the container act as the lexer, you will receive this notification when Scintilla
%  is about to display or print text that requires styling. You are required to style the text from the line that contains the position returned
%  by SCI_GETENDSTYLED up to the position passed in SCNotification.position. Symbolically, you need code of the form:
%      startPos = sciLexer_api::SCI_GETENDSTYLED()
%      lineNumber = sciLexer_api::SCI_LINEFROMPOSITION(startPos);
%      startPos = sciLexer_api::SCI_POSITIONFROMLINE(lineNumber);
%      MyStyleRoutine(startPos, SCNotification.position);
facts
    charAddedEvent : event1{integer Char} := event1::new().

predicates
    scnCharAdded : (scNotification Notification).
clauses
    scnCharAdded(Notification) :-
        charAddedEvent:notify(notification_ch(Notification)).
        %  SCN_CHARADDED
        %  This is sent when the user types an ordinary text character (as opposed to a command character) that is entered into the text. The
        %  container can use this to decide to display a call tip or an auto completion list. The character is in SCNotification.ch. This notification is
        %  sent before the character has been styled so processing that depends on styling should instead be performed in the SCN_UPDATEUI
        %  notification.

facts
    savepointEvent : event1{savepointAction SavepointAction} := event1::new().

predicates
    scnSavepointReached : (scNotification Notification).
clauses
    scnSavepointReached(_Notification) :-
        savepointEvent:notify(savepointReached).

predicates
    scnSavepointLeft : (scNotification Notification).
clauses
    scnSavepointLeft(_Notification) :-
        savepointEvent:notify(savepointLeft).
        %  SCN_SAVEPOINTREACHED
        %  SCN_SAVEPOINTLEFT
        %  Sent to the container when the save point is entered or left, allowing the container to display a "document dirty" indicator and change
        %  its menus.
        %  See also: SCI_SETSAVEPOINT, SCI_GETMODIFY

facts
    modifyAttemptROEvent : event0 := event0::new().

predicates
    scnModifyAttemptRO : (scNotification Notification).
clauses
    scnModifyAttemptRO(_Notification) :-
        modifyAttemptROEvent:notify().
        %  SCN_MODIFYATTEMPTRO
        %  When in read-only mode, this notification is sent to the container if the user tries to change the text. This can be used to check the
        %  document out of a version control system. You can set the read-only state of a document with SCI_SETREADONLY.

%  SCN_KEY
%  Reports all keys pressed but not consumed by Scintilla. Used on GTK+ because of some problems with keyboard Focus and is not sent
%  by the Windows version. SCNotification.ch holds the key code and SCNotification.modifiers holds the modifiers. This notification is sent
%  if the modifiers include SCMOD_ALT or SCMOD_CTRL and the key code is less than 256.
facts
    doubleClickEvent : event3{integer Position, lineNumber Line, integer Modifiers} := event3::new().

predicates
    scnDoubleClick : (scNotification Notification).
clauses
    scnDoubleClick(Notification) :-
        scNotification(:Position = SciPosition, :Line = SciLine, :Modifiers = Modifiers | _) = Notification,
        Position = convert(integer, SciPosition),
        Line = toLineNumber(convert(integer, SciLine)),
        doubleClickEvent:notify(Position, Line, Modifiers).
        %  SCN_DOUBLECLICK
        %  The mouse button was double clicked in editor. The position field is set to the text position of the double click and the line field is set to
        %  the line of the double click.

facts
    updateUIEvent : event1{unsigned UpdatedFlag} := event1::new() [constant].

predicates
    scnUpdateUI : (scNotification Notification).
clauses
    scnUpdateUI(scNotification(:Updated = Updated | _)) :-
        updateUIEvent:notify(Updated),
        if B2 = tryPositionBefore(currentPos) and B1 = tryBraceMatch(B2, 0) then
        else
            B1 = invalid_position,
            B2 = invalid_position
        end if,
        braceHighLight(B1, B2).
        %  SCN_UPDATEUI
        %  Either the text or styling of the document has changed or the selection range or scroll position has changed. Now would be a good time
        %  to update any container UI elements that depend on document or view state. The updated field is set to the bit set of things changed
        %  since the previous notiication.

%  Symbol Value Meaning
%  SC_UPDATE_CONTENT 0x01 Contents, styling or markers have been changed.
%  SC_UPDATE_SELECTION 0x02 Selection has been changed.
%  SC_UPDATE_V_SCROLL 0x04 Scrolled vertically.
%  SC_UPDATE_H_SCROLL 0x08 Scrolled horizontally.
facts
    modifiedEvent :
        event5{modifyAction Action, modifyPerformed Performed, boolean MultiStepUndoRedo, boolean LastStepInUndoRedo, boolean StartAction} :=
        event5::new() [constant].

predicates
    scnModified : (scNotification Notification).
clauses
    scnModified(scNotification(:ModificationType = ModificationType | Notification)) :-
        if 0 <> ModificationType ** sc_mod_beforeInsert ++ sc_mod_beforeDelete then
            %            multipleSelection := false
        end if,
        Performed = getModifyPerformed(ModificationType),
        MultiStepUndoRedo = bit::bit2boolean(ModificationType, sc_multiStepUndoRedo),
        LastStepInUndoRedo = bit::bit2boolean(ModificationType, sc_lastStepInUndoRedo),
        StartAction = bit::bit2boolean(ModificationType, sc_startAction),
        foreach Action = getModifyAction_nd(ModificationType, Notification) do
            modifiedEvent:notify(Action, Performed, MultiStepUndoRedo, LastStepInUndoRedo, StartAction)
        end foreach.

class predicates
    getModifyAction_nd : (unsigned ModificationType, scNotification Notification) -> modifyAction Action nondeterm.
clauses
    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_insertText),
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, Text8, Length, LinesAdded, _Message, _WParam, _LParam, Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        IsTyped = toBoolean(0 <> Line),
        Action = insertText(convert(integer, Position), Text8, convert(integer, Length), convert(integer, LinesAdded), IsTyped).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_deleteText),
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, Text8, Length, LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = deleteText(convert(integer, Position), Text8, convert(integer, Length), convert(integer, LinesAdded)).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_changeStyle),
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, _Text8, Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = changeStyle(convert(integer, Position), convert(integer, Length)).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_changeFold),
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, Line,
                FoldLevelNow, FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = changeFold(toLineNumber(convert(integer, Line)), FoldLevelNow, FoldLevelPrev).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_changeMarker),
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = changeMarker(toLineNumber(convert(integer, Line))).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_beforeInsert),
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, Text8, Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = beforeInsert(convert(integer, Position), Text8, convert(integer, Length)).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_beforeDelete),
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, _Text8, Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = beforeDelete(convert(integer, Position), convert(integer, Length)).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_changeIndicator),
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, _Text8, Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = changeIndicator(convert(integer, Position), convert(integer, Length)).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_changeLineState),
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = changeLineState(toLineNumber(convert(integer, Line))).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_lexerState),
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, _Text8, Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = lexerState(convert(integer, Position), convert(integer, Length)).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_changeMargin),
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = changeMargin(toLineNumber(convert(integer, Line))).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_changeAnnotation),
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = changeAnnotation(toLineNumber(convert(integer, Line))).

    getModifyAction_nd(ModificationType, Notification) = Action :-
        bit::isSet(ModificationType, sc_mod_container),
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        Action = container(Token).

class predicates
    getModifyPerformed : (unsigned ModificationType) -> modifyPerformed Performed.
clauses
    getModifyPerformed(ModificationType) = performedUser :-
        bit::isSet(ModificationType, sc_performed_user),
        !.

    getModifyPerformed(ModificationType) = performedUndo :-
        bit::isSet(ModificationType, sc_performed_undo),
        !.

    getModifyPerformed(ModificationType) = performedRedo :-
        bit::isSet(ModificationType, sc_performed_redo),
        !.

    getModifyPerformed(_ModificationType) = performedNone.

%  SCN_MODIFIED
%  This notification is sent when the text or styling of the document changes or is about to change. You can set a mask for the notifications
%  that are sent to the container with SCI_SETMODEVENTMASK. The notification structure contains information about what changed, how
%  the change occurred and whether this changed the number of lines in the document. No modifications may be performed while in a
%  SCN_MODIFIED event. The SCNotification fields used are:
%  Field Usage
%  modificationType A set of flags that identify the change(s) made. See the next table.
%  position Start position of a text or styling change. Set to 0 if not useD.
%  length Length of the change in cells or characters when the text or styling changes. Set to 0 if not useD.
%  linesAdded Number of added lines. If negative, the number of deleted lines. Set to 0 if not used or no lines added or deleted.
%  text Valid for text changes, not for style changes. If we are collecting undo information this holds a pointer to the text that is handed to
%  the Undo system, otherwise it is zero. For user performed SC_MOD_BeforeDELETE the text field is 0 and for user performed
%  SC_MOD_BeforeINSERT the text field points to an array of cells, not bytes and the length is the number of cells.
%  line The line number at which a fold level or marker change occurred. This is 0 if unused and may be -1 if more than one line changed.
%  foldLevelNow The new fold level applied to the line or 0 if this field is unused.
%  foldLevelPrev The previous folding level of the line or 0 if this field is unused.
%  The SCNotification.modificationType field has bits set to tell you what has been done. The SC_MOD_* bits correspond to actions. The
%  SC_PERFORMED_* bits tell you if the action was done by the user, or the result of Undo or Redo of a previous action.
%  Symbol Value Meaning SCNotification fields
%  SC_MOD_INSERTTEXT 0x01 Text has been inserted into the document. position, length, text, linesAdded
%  SC_MOD_DELETETEXT 0x02 Text has been removed from the document. position, length, text, linesAdded
%  SC_MOD_CHANGESTYLE 0x04 A style change has occurred. position, length
%  SC_MOD_CHANGEFOLD 0x08 A folding change has occurred. line, foldLevelNow, foldLevelPrev
%  SC_PERFORMED_USER 0x10 Information: the operation was done by the user. None
%  SC_PERFORMED_UNDO 0x20 Information: this was the result of an Undo. None
%  SC_PERFORMED_REDO 0x40 Information: this was the result of a Redo. None
%  SC_MULTISTEPUNDOREDO 0x80 This is part of a multi-step Undo or Redo transaction. None
%  SC_LASTSTEPINUNDOREDO 0x100 This is the final step in an Undo or Redo transaction. None
%  SC_MOD_CHANGEMARKER 0x200 One or more markers has changed in a line. line
%  SC_MOD_BeforeINSERT 0x400 Text is about to be inserted into the document. position, if performed by user then text in cells, length
%  in cells
%  SC_MOD_BeforeDELETE 0x800 Text is about to be deleted from the document. position, length
%  SC_MOD_CHANGEINDICATOR 0x4000 An indicator has been added or removed from a range of text. position, length
%  SC_MOD_CHANGELINESTATE 0x8000 A line state has changed because SCI_SETLINESTATE was called. line
%  SC_MOD_LEXERSTATE 0x80000 The internal state of a lexer has changed over a range. position, length
%  SC_MOD_CHANGEMARGIN 0x10000 A text margin has changed. line
%  SC_MOD_CHANGEANNOTATION 0x20000 An annotation has changed. line
%  SC_MULTILINEUNDOREDO 0x1000 This is part of an Undo or Redo with multi-line changes. None
%  SC_STARTACTION 0x2000 This is set on a SC_PERFORMED_USER action when it is the first or only step in an undo transaction. This
%  can be used to integrate the Scintilla undo stack with an undo stack in the container application by adding a Scintilla action to the
%  container's stack for the currently opened container transaction or to open a new container transaction if there is no open container
%  transaction.  None
%  SC_MOD_CONTAINER 0x40000 This is set on for actions that the container stored into the undo stack with SCI_ADDUNDOACTION.
%  token
%  SC_MODEVENTMASKALL 0x7FFFF This is a mask for all valid flags. This is the default mask state set by SCI_SETMODEVENTMASK. None
%  SCEN_CHANGE
%  SCEN_CHANGE (768) is fired when the text (not the style) of the document changes. This notification is sent using the WM_COMMAND
%  message on Windows and the "Command" signal on GTK+ as this is the behavior of the standard Edit control (SCEN_CHANGE has the
%  same value as the Windows Edit control EN_CHANGE). No other information is sent. If you need more detailed information use
%  SCN_MODIFIED. You can filter the types of changes you are notified about with SCI_SETMODEVENTMASK.
clauses
    modEventMask(EventMask) :-
        sciLexer_api::setModEventMask(native, EventMask).

clauses
    modEventMask() = sciLexer_api::getModEventMask(native).
        %  These messages set and get an event mask that determines which document change events are notified to the container with
        %  SCN_MODIFIED and SCEN_CHANGE. For example, a container may decide to see only notifications about changes to text and not
        %  styling changes by calling SCI_SETMODEVENTMASK(SC_MOD_INSERTTEXT|SC_MOD_DELETETEXT).

%  The possible notification types are the same as the modificationType bit flags used by SCN_MODIFIED: SC_MOD_INSERTTEXT,
%  SC_MOD_DELETETEXT, SC_MOD_CHANGESTYLE, SC_MOD_CHANGEFOLD, SC_PERFORMED_USER, SC_PERFORMED_UNDO,
%  SC_PERFORMED_REDO, SC_MULTISTEPUNDOREDO, SC_LASTSTEPINUNDOREDO, SC_MOD_CHANGEMARKER,
%  SC_MOD_BeforeINSERT, SC_MOD_BeforeDELETE, SC_MULTILINEUNDOREDO, and SC_MODEVENTMASKALL.
facts
    setFocusEvent : event0 := event0::new() [constant].

predicates
    scenSetFocus : (scNotification Notification).
clauses
    scenSetFocus(_Notification) :-
        setFocusEvent:notify().

facts
    killFocusEvent : event0 := event0::new() [constant].

predicates
    scenKillFocus : (scNotification Notification).
clauses
    scenKillFocus(_Notification) :-
        killFocusEvent:notify().
        %  SCEN_SETFOCUS
        %  SCEN_KILLFOCUS
        %  SCEN_SETFOCUS (512) is fired when Scintilla receives Focus and SCEN_KILLFOCUS (256) when it loses Focus. These notifications are
        %  sent using the WM_COMMAND message on Windows and the "Command" signal on GTK+ as this is the behavior of the standard Edit
        %  control. Unfortunately, these codes do not match the Windows Edit notification codes EN_SETFOCUS (256) and EN_KILLFOCUS (512). It
        %  is now too late to change the Scintilla codes as clients depend on the current values.

facts
    macroRecordEvent : event3{unsigned Message, wParam WParam, lParam LParam} := event3::new() [constant].

predicates
    scnMacroRecord : (scNotification Notification).
clauses
    scnMacroRecord(Notification) :-
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, Message, WParam, LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        macroRecordEvent:notify(Message, WParam, LParam).
        %  SCN_MACRORECORD
        %  The SCI_STARTRECORD and SCI_STOPRECORD messages enable and disable macro recording. When enabled, each time a recordable
        %  change occurs, the SCN_MACRORECORD notification is sent to the container. It is up to the container to record the action. To see the
        %  complete list of SCI_* messages that are recordable, search the Scintilla source Editor.cxx for Editor::NotifyMacroRecord. The fields of
        %  SCNotification set in this notification are:

%  Field Usage
%  message The SCI_* message that caused the notification.
%  wParam The value of wParam in the SCI_* message.
%  lParam The value of lParam in the SCI_* message.
facts
    marginClickEvent : event3{integer Position, integer Modifiers, integer Margin} := event3::new() [constant].

predicates
    scnMarginClick : (scNotification Notification).
clauses
    scnMarginClick(Notification) :-
        scNotification(_Nmhdr, Position, _Ch, Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        marginClickEvent:notify(convert(integer, Position), Modifiers, Margin).
        %  SCN_MARGINCLICK
        %  This notification tells the container that the mouse was clicked inside a margin that was marked as sensitive (see
        %  SCI_SETMARGINSENSITIVEN). This can be used to perform folding or to place breakpoints. The following SCNotification fields are
        %  useD:

%  Field Usage
%  position The position of the start of the line in the document that corresponds to the margin click.
%  modifiers The appropriate combination of SCI_SHIFT, SCI_CTRL and SCI_ALT to indicate the keys that were held down at the time of the margin click.
%  margin The margin number that was clicked.
facts
    needShownEvent : event2{integer Position, integer Length} := event2::new() [constant].

predicates
    scnNeedShown : (scNotification Notification).
clauses
    scnNeedShown(Notification) :-
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, _Text8, Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        needShownEvent:notify(convert(integer, Position), convert(integer, Length)).
        %  SCN_NEEDSHOWN
        %  Scintilla has determined that a range of lines that is currently Invisible should be made visible. An example of where this may be
        %  needed is if the end of line of a contracted fold point is deleted. This message is sent to the container in case it wants to make the line
        %  visible in some unusual way such as making the whole document visible. Most containers will just ensure each line in the range is visible
        %  by calling SCI_ENSUREVISIBLE. The position and length fields of SCNotification indicate the range of the document that should be made
        %  visible. The container code will be similar to the following code skeleton:

%  firstLine = sciLexer_api::SCI_LINEFROMPOSITION(scn.position)
%  lastLine = sciLexer_api::SCI_LINEFROMPOSITION(scn.position+scn.length-1)
%  for line = sciLexer_api::lineStart to lineEnd do SCI_ENSUREVISIBLE(line) next
facts
    paintedEvent : event0 := event0::new() [constant].

predicates
    scnPainted : (scNotification Notification).
clauses
    scnPainted(_Notification) :-
        paintedEvent:notify().
        %  SCN_PAINTED
        %  Painting has just been done. Useful when you want to update some other widgets based on a change in Scintilla, but want to have the
        %  paint occur first to appear more responsive. There is no other information in SCNotification.

facts
    userListSelectionEvent : event2{string8 Text8, wParam WParam} := event2::new() [constant].

predicates
    scnUserListSelection : (scNotification Notification).
clauses
    scnUserListSelection(Notification) :-
        scNotification(_Nmhdr, _Position, _Ch, _Modifiers, _ModificationType, Text8, _Length, _LinesAdded, _Message, WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        userListSelectionEvent:notify(Text8, WParam).
        %  SCN_USERLISTSELECTION
        %  The user has selected an item in a user list. The SCNotification fields used are:

%  Field Usage
%  wParam This is set to the listType parameter from the SCI_USERLISTSHOW message that initiated the list.
%  text The text of the selection.
%  SCN_URIDROPPED
%  Only on the GTK+ version. Indicates that the user has dragged a URI such as a file name or Web address onto Scintilla. The container
%  could interpret this as a request to open the file. The text field of SCNotification points at the URI text.
facts
    dwellEvent : event4{dwellAction DwellAction, integer Position, integer X, integer Y} := event4::new() [constant].

predicates
    scnDwellStart : (scNotification Notification).
    scnDwellEnd : (scNotification Notification).

clauses
    scnDwellStart(Notification) :-
        scnDwellStartEnd(dwellStart, Notification).

clauses
    scnDwellEnd(Notification) :-
        scnDwellStartEnd(dwellEnd, Notification).

predicates
    scnDwellStartEnd : (dwellAction DwellAction, scNotification Notification).
clauses
    scnDwellStartEnd(DwellAction, Notification) :-
        scNotification(_Nmhdr, Position, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, X, Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        dwellEvent:notify(DwellAction, convert(integer, Position), X, Y).
        %  SCN_DWELLSTART
        %  SCN_DWELLEND
        %  SCN_DWELLSTART is generated when the user keeps the mouse in one position for the dwell period (see SCI_SETMouseDwellTime).
        %  SCN_DWELLEND is generated after a SCN_DWELLSTART and the mouse is moved or other activity such as key press indicates the
        %  dwell is over. Both notifications set the same fields in SCNotification:

%  Field Usage
%  position This is the nearest position in the document to the position where the mouse pointer was lingering.
%  x, y Where the pointer lingered. The position field is set to SCI_POSITIONFROMPOINTCLOSE(x, y).
clauses
    mouseDwellTime(DwellTime) :-
        sciLexer_api::setMouseDwellTime(native, DwellTime).

clauses
    mouseDwellTime() = sciLexer_api::getMouseDwellTime(native).
        %  These two messages set and get the time the mouse must sit still, in milliseconds, to generate a SCN_DWELLSTART notification. If set
        %  to SC_TIME_FOREVER, the default, no dwell events are generated.

facts
    zoomEvent : event0 := event0::new() [constant].

predicates
    scnZoom : (scNotification Notification).
clauses
    scnZoom(_Notification) :-
        zoomEvent:notify().
        % SCN_ZOOM
        % SCN_ZOOM
        % This notification is generated when the user zooms the display using the keyboard or the SCI_SETZOOM method is called.
        % This notification can be used to recalculate positions, such as the width of the line number margin to maintain sizes in terms of characters
        % rather than pixels.
        % SCNotification has no additional information.

facts
    hotspotActionEvent : event1{hotspotAction HotspotAction} := event1::new() [constant].

predicates
    scnHotspotClick : (scNotification Notification).
clauses
    scnHotspotClick(Notification) :-
        scNotification(_Nmhdr, Position, _Ch, Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        hotspotActionEvent:notify(hotspotClick(convert(integer, Position), Modifiers)).

predicates
    scnHotspotDoubleClick : (scNotification Notification).
clauses
    scnHotspotDoubleClick(Notification) :-
        scNotification(_Nmhdr, Position, _Ch, Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        hotspotActionEvent:notify(hotspotDoubleClick(convert(integer, Position), Modifiers)).

predicates
    scnHotspotReleaseClick : (scNotification Notification).
clauses
    scnHotspotReleaseClick(Notification) :-
        scNotification(_Nmhdr, _Position, _Ch, Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        hotspotActionEvent:notify(hotspotReleaseClick(Modifiers)).
        % SCN_HOTSPOTDOUBLECLICK
        % SCN_HOTSPOTRELEASECLICK
        % These notifications are generated when the user clicks or double clicks on text that is in a style with the hotSpot attribute set.
        % This notification can be used to link to variable definitions or web pages.
        % The position field is set the text position of the click or double click and the modifiers field set to the key modifiers held down in a similar manner to SCN_KEY.
        % Only the state of the Ctrl key is reported for SCN_HOTSPOTRELEASECLICK.

facts
    indicatorActionEvent : event1{indicatorAction IndicatorAction} := event1::new() [constant].

predicates
    scnIndicatorClick : (scNotification Notification).
clauses
    scnIndicatorClick(Notification) :-
        scNotification(_Nmhdr, Position, _Ch, Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        indicatorActionEvent:notify(indicatorClick(convert(integer, Position), Modifiers)).

predicates
    scnIndicatorRelease : (scNotification Notification).
clauses
    scnIndicatorRelease(Notification) :-
        scNotification(_Nmhdr, _Position, _Ch, Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        indicatorActionEvent:notify(indicatorRelease(Modifiers)).
        % SCN_INDICATORCLICK
        % SCN_INDICATORRELEASE
        % These notifications are generated when the user clicks or releases the mouse on text that has an indicator.
        % The position field is set the text position of the click and the modifiers field set to the key modifiers held down in a similar manner to SCN_KEY.

facts
    callTipClickEvent : event1{integer Arrow} := event1::new() [constant].

predicates
    scnCallTipClick : (scNotification Notification).
clauses
    scnCallTipClick(Notification) :-
        scNotification(_Nmhdr, Arrow, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, _LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        callTipClickEvent:notify(convert(integer, Arrow)).
        % SCN_CALLTIPCLICK
        % This notification is generated when the user clicks on a calltip.
        % This notification can be used to display the next function prototype when a function name is overloaded with different arguments.
        % The position field is set to 1 if the click is in an up arrow, 2 if in a down arrow, and 0 if elsewhere.

facts
    autoCEvent : event1{autoCAction AutoCAction} := event1::new() [constant].

predicates
    scnAutoCSelection : (scNotification Notification).
clauses
    scnAutoCSelection(Notification) :-
        scNotification(_Nmhdr, _Arrow, _Ch, _Modifiers, _ModificationType, _Text8, _Length, _LinesAdded, _Message, _WParam, LParam, _Line,
                _FoldLevelNow, _FoldLevelPrev, _Margin, _ListType, _X, _Y, _Token, _AnnotationLinesAdded, _Updated)
            = Notification,
        autoCEvent:notify(autoCSelection(LParam)).
        % SCN_AUTOCSELECTION
        % The user has selected an item in an autocompletion list.
        % The notification is sent before the selection is inserted.
        % Automatic insertion can be cancelled by sending a SCI_AUTOCCANCEL message before returning from the notification.
        % The SCNotification fields used are: Field Usage Field Usage
        % lParam The start position of the word being completed.
        % text The text of the selection.

predicates
    scnAutoCCancelled : (scNotification Notification).
clauses
    scnAutoCCancelled(_Notification) :-
        autoCEvent:notify(autoCCancelled).
        % SCN_AUTOCCANCELLED
        % The user has cancelled an autocompletion list.
        % There is no other information in SCNotification.

predicates
    scnAutoCCharDeleted : (scNotification Notification).
clauses
    scnAutoCCharDeleted(_Notification) :-
        autoCEvent:notify(autoCCharDeleted).
        % SCN_AUTOCCHARDELETED
        % The user deleted a character while autocompletion list was active.
        % There is no other information in SCNotification.

/***********************************************
    Auxillary predicates
*/
class predicates
    fromUtf8Char : (char8 Char8) -> char Char.
clauses
    fromUtf8Char(Char8) = string8::mapToChar(Char8, utf8).

class predicates
    toUtf8Char : (char Char) -> char8 Char8.
clauses
    toUtf8Char(Char) = string8::mapFromChar(Char, utf8).

clauses
    displayFindAndReplace_modal() = sciLexerSupport\findAndReplaceControl::displayModal(This):getTopLevelContainerWindow().

clauses
    displayIncrementalFind_modal() = sciLexerSupport\incrementalFindDialog::displayModal(This).

clauses
    displayFindAndReplace_modeless(Owner) = displayFindAndReplace_modeless(Owner, false).

clauses
    displayFindAndReplace_modeless(Owner, AsDialog) =
        sciLexerSupport\findAndReplaceControl::displayModeless(Owner, AsDialog):getTopLevelContainerWindow().

clauses
    findPrevious() :-
        sciLexerSupport\findAndReplaceControl::findPrevious().

clauses
    findNext() :-
        sciLexerSupport\findAndReplaceControl::findNext().

clauses
    tryGetFindAndReplace_modeless() = convert(dialog, sciLexerSupport\findAndReplaceControl::tryGetModeless():getTopLevelContainerWindow()).

clauses
    displayGotoLine_modal() = true :-
        Dlg = commonDialogs::newGet_integerControl(This, "Go to line", "Line number"),
        Dlg:control:setMinimum(0),
        gotoLine(convert(lineNumber, Dlg:tryGet())),
        !.

    displayGotoLine_modal() = false.

class predicates
    toLineNumber : (integer SciLineNumber) -> lineNumber LineNumber.
clauses
    toLineNumber(V) = convert(lineNumber, V + 1).

class predicates
    fromLineNumber : (lineNumber LineNumber) -> integer SciLineNumber.
clauses
    fromLineNumber(V) = convert(integer, V) - 1.

class predicates
    toLinePosition : (integer SciLinePosition) -> linePosition LinePosition.
clauses
    toLinePosition(V) = convert(linePosition, V + 1).

class predicates
    fromLinePosition : (linePosition LinePosition) -> integer SciLinePosition.
clauses
    fromLinePosition(V) = convert(integer, V) - 1.

clauses
    getText() = text.

end implement sciLexer
