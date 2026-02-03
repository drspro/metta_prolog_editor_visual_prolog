% Copyright PDC

namespace sciLexerSupport

implement findAndReplaceControl inherits userControlSupport
    open core, vpiDomains

domains
    charRange = charRange(integer From, integer To).

class facts
    modeless_fact : (findAndReplaceControl Modeless) determ.
    asDialog : boolean := false.

clauses
    displayModal(Owner) = FindAndReplaceControl :-
        Dialog = dialog::new(Owner),
        Dialog:setDecoration(frameDecoration::titlebar([frameDecoration::closeButton, frameDecoration::helpButton])),
        FindAndReplaceControl = new(Owner, Dialog, Dialog, true),
        Dialog:show().

clauses
    displayModeless(_, _) = FindAndReplaceControl :-
        modeless_fact(FindAndReplaceControl),
        !,
        FindAndReplaceControl:reactivateModeless().

    displayModeless(Owner, AsDialog) = FindAndReplaceControl :-
        if true = AsDialog then
            Dialog = dialog::new(Owner),
            Dialog:setModal(false),
            TopLevelContainerWindow = Dialog,
            OuterWindow = Dialog,
            FrameDecoration = Dialog
        else
            FormWindow = formWindow::new(applicationWindow::get()),
            TopLevelContainerWindow = FormWindow,
            OuterWindow = FormWindow:frameWindow,
            FrameDecoration = FormWindow
        end if,
        FrameDecoration:setBorder(frameDecoration::sizeBorder()),
        FrameDecoration:setDecoration(frameDecoration::titlebar([frameDecoration::closeButton, frameDecoration::helpButton])),
        FindAndReplaceControl = new(Owner, TopLevelContainerWindow, OuterWindow, false),
        asDialog := AsDialog,
        TopLevelContainerWindow:show().

clauses
    reactivateModeless() :-
        updateHistory(),
        setFromEditorState(),
        getTopLevelContainerWindow():bringToTop().

constructors
    new : (sciLexer Owner, topLevelContainerWindow Parent, window OuterWindow, boolean Modal).
clauses
    new(Owner, RootContainer, OuterWindow, Modal) :-
        userControlSupport::new(RootContainer),
        generatedInitialize(),
        RootContainer:setDefaultButton(next_btn),
        RootContainer:setText(getText()),
        setPosition(0, 0),
        if rct(Left, Top, Width, _Heights) = readRect() then
            This:setWidth(Width),
            RootContainer:setPosition(Left, Top)
        end if,
        RootContainer:setSize(getWidth(), getHeight() + 4),
        RootContainer:addNativeMessageHandler(onDialogNative),
        OuterWindow:addNativeMessageHandler(onOuterNative),
        whenCreated(
            {  :-
                RootContainer:addSizeListener(onParentSize),
                dockStyle := dockFill,
                if WriteRect = notErroneous(writeRect) then
                    RootContainer:addDestroyListener(
                        {  :-
                            pnt(L, T) = RootContainer:getPosition(),
                            WriteRect(rct(L, T, getWidth(), 0))
                        })
                end if
            }),
        if true = Modal then
            assert(modalOwner_fact(Owner))
        else
            retractAll(modeless_fact(_)),
            assert(modeless_fact(This))
        end if,
        findText_edit:setText(findTextFromHistory),
        replaceText_edit:setText(replaceTextFromHistory),
        setFromEditorState(),
        updateHistory(),
        enableReplaceButtons(),
        regularExpression_chk:setChecked(regularExpression),
        caseSensitive_chk:setChecked(caseSensitive),
        wholeWords_chk:setChecked(wholeWords),
        incremental_chk:setChecked(incrementalFind),
        inSelection_chk:setChecked(inSelection),
        % Don't listen to changes untill after the initial settings have been set
        % delay to avoid starting incremental search as result of initialization:
        delayCall(10, {  :- findText_edit:addModifiedListener(onFindText_editModified) }),
        regularExpression_chk:addStateChangedListener(onRegularExpression_chkStateChanged),
        wholeWords_chk:addStateChangedListener(onWholeWords_chkStateChanged),
        caseSensitive_chk:addStateChangedListener(onCaseSensitive_chkStateChanged),
        inSelection_chk:addStateChangedListener(onInSelection_chkStateChanged),
        incremental_chk:addStateChangedListener(onIncremental_chkStateChanged).

clauses
    tryGetModeless() = FindAndReplaceControl :-
        modeless_fact(FindAndReplaceControl).

facts
    modalOwner_fact : (sciLexer Editor) determ.

predicates
    setFromEditorState : ().
clauses
    setFromEditorState() :-
        if Editor = tryGetEditor() then
            setScopeAndTarget(Editor),
            if false = inSelection and SelText = Editor:selText and "" <> SelText then
                % selection on single line
                findText_edit:setText(SelText)
            end if
        end if.

predicates
    tryGetEditor : () -> sciLexer Editor determ.
clauses
    tryGetEditor() = Editor :-
        if modalOwner_fact(Editor) then
        else
            Editor = sciLexer::getInVisitOrder_nd(),
            !
        end if.

class facts - settings
    regularExpression : boolean := false.
    caseSensitive : boolean := false.
    wholeWords : boolean := false.
    incrementalFind : boolean := false.
    inSelection : boolean := false.
    findHistory : string* := [].
    replaceHistory : string* := [].

class facts
    readHistory : historyReader :=
        { (ForReplace) =
            if true = ForReplace then
                replaceHistory
            else
                findHistory
            end if
        }.
    appendHistory : historyAppender :=
        { (ForReplace, Text) :-
            if true = ForReplace then
                replaceHistory := [Text | list::removeAll(replaceHistory, Text)]
            else
                findHistory := [Text | list::removeAll(findHistory, Text)]
            end if
        }.
    writeSettings : settingWriter := {  :- succeed }.

clauses
    loadSettings([RegExp, CaseSens, WholeWords, InSelect, IncremFind]) :-
        !,
        regularExpression := RegExp,
        caseSensitive := CaseSens,
        wholeWords := WholeWords,
        inSelection := InSelect,
        incrementalFind := IncremFind.

    loadSettings(_).

class properties
    findTextFromHistory : string (o).

clauses
    findTextFromHistory() = getFirstOrEmpty(readHistory(false)).

class properties
    replaceTextFromHistory : string (o).

clauses
    replaceTextFromHistory() = getFirstOrEmpty(readHistory(true)).

predicates
    updateHistory : ().
clauses
    updateHistory() :-
        updateHistory(false),
        updateHistory(true).

predicates
    updateHistory : (boolean ForReplace).
clauses
    updateHistory(ForBoolean) :-
        Edit = if true = ForBoolean then replaceText_edit else findText_edit end if,
        Text = Edit:getText(),
        if "" <> Text then
            appendHistory(ForBoolean, Text)
        end if,
        History = readHistory(ForBoolean),
        Edit:setList(History),
        Edit:setText(Text).

class predicates
    updateSettings : ().
clauses
    updateSettings() :-
        writeSettings([regularExpression, caseSensitive, wholeWords, inSelection, incrementalFind]).

predicates
    updateHistoryIfFoundOrNonIncremental : (boolean Found, boolean IncrementalFind).
clauses
    updateHistoryIfFoundOrNonIncremental(Found, IncrementalFind) :-
        if not((false = Found and true = IncrementalFind)) then
            updateHistory()
        end if.

class predicates
    setScopeAndTarget : (sciLexer Editor).
clauses
    setScopeAndTarget(Editor) :-
        if isMultiLineSelection(Editor) then
            InSelection = true,
            Target = math::min(Editor:currentPos, Editor:anchor),
            TargetStart = Target,
            TargetEnd = Target
        else
            InSelection = false,
            TargetStart = Editor:selectionStart,
            TargetEnd = Editor:selectionEnd
        end if,
        inSelection := InSelection,
        Editor:targetStart := TargetStart,
        Editor:targetEnd := TargetEnd,
        updateSettings().

class predicates
    isMultiLineSelection : (sciLexer Editor) determ.
clauses
    isMultiLineSelection(Editor) :-
        Editor:lineFromPosition(Editor:currentPos) <> Editor:lineFromPosition(Editor:anchor).

class predicates
    onDialogNative : nativeMessageHandler.
clauses
    onDialogNative(Window, gui_native::wm_command, uncheckedConvert(gui_native::wParam, idc_cancel), _LParam) = defaultNativeHandling :-
        !,
        _ = gui_native::postMessage(Window:getVpiWindow(), gui_native::wm_close, gui_api::wNull, gui_api::lNull).

    onDialogNative(_Window, Message, WParam, _LParam) = nativeResult(uncheckedConvert(gui_native::lResult, Brush)) :-
        gui_api::isCtrlColorMsg(Message),
        uxTheme_api::isAppThemed(),
        !,
        DC = uncheckedConvert(gui_native::hDC, WParam),
        _ = gui_native::setBkColor(DC, color_white),
        Brush = gui_native::getStockObject(gui_native::white_brush).

    onDialogNative(_, _, _, _) = defaultNativeHandling.

predicates
    onOuterNative : nativeMessageHandler.
clauses
    onOuterNative(Window, ActivateMsg, WParam, _LParam) = defaultNativeHandling :-
        if gui_native::wm_activate = ActivateMsg then
            IsActive = toBoolean(gui_api::wNull <> WParam)
        else
            gui_native::wm_mdiactivate = ActivateMsg,
            IsActive = toBoolean(WParam <> Window:getVpiWindow())
        end if,
        Editor = tryGetEditor(),
        !,
        if true = IsActive then
            setScopeAndTarget(Editor),
            inSelection_chk:removeStateChangedListener(onInSelection_chkStateChanged),
            inSelection_chk:setChecked(inSelection),
            inSelection_chk:addStateChangedListener(onInSelection_chkStateChanged),
            findText_edit:setFocus()
        else
            restoreOneSelection(Editor, false)
        end if.

    onOuterNative(Window, gui_native::wm_getminmaxinfo, _WParam, LParam) = nativeResult_null :-
        getParent():isShown(),
        false = switchingLayout,
        !,
        MinWidthPtr = memory::pointerAdd(uncheckedConvert(pointer, LParam), 6 * sizeOfDomain(unsigned)),
        MinHeightPtr = memory::pointerAdd(uncheckedConvert(pointer, LParam), 7 * sizeOfDomain(unsigned)),
        MaxHeightPtr = memory::pointerAdd(uncheckedConvert(pointer, LParam), 9 * sizeOfDomain(unsigned)),
        Window:getOuterSize(_, Height),
        pnt(MinimalWidth, _) = getTopLevelContainerWindow():pntUnit2Pixel(pnt(minimalWidth, 0)),
        memory::setInteger(MinWidthPtr, MinimalWidth),
        memory::setInteger(MinHeightPtr, Height),
        memory::setInteger(MaxHeightPtr, Height).

    onOuterNative(_Window, _Msg, _WParam, _LParam) = defaultNativeHandling.

class properties
    findFlags : unsigned (o).

clauses
    findFlags() = WW ++ CS ++ RE :-
        WW = if true = wholeWords then sciLexer_native::scfind_wholeWord else 0 end if,
        CS = if true = caseSensitive then sciLexer_native::scfind_matchCase else 0 end if,
        RE = if true = regularExpression then sciLexer_native::scfind_regExp ++ sciLexer_native::scfind_cxx11Regex else 0 end if.

predicates
    incrementalFindInit : ().
clauses
    incrementalFindInit() :-
        % delay the find (mainly to avoid multiple finds initiated for different reasons)
        delayCall(10, incrementalFindInitDelayed).

predicates
    incrementalFindInitDelayed : ().
clauses
    incrementalFindInitDelayed() :-
        NotRegEx = ~~regularExpression,
        prev_btn:setEnabled(NotRegEx),
        wholeWords_chk:setEnabled(NotRegEx),
        if Editor = tryGetEditor() then
            _ = incrementalFindDialog(Editor, initialFindRange(Editor))
        end if.

clauses
    incrementalFindNextRange(Editor) = incrementalFindDialog(Editor, nextRange(Editor)).

clauses
    incrementalFindPreviousRange(Editor) = incrementalFindDialog(Editor, previousRange(Editor)).

predicates
    incrementalFindDialog : (sciLexer Editor, charRange SearchRange) -> boolean Found.
clauses
    incrementalFindDialog(Editor, FindRange) = Found :-
        Found = doIncrementalFind(Editor, FindRange, findText_edit:getText()),
        statusText_ctl:setText(formatStatus(Editor, Found)),
        if false = Found then
            Editor:targetEnd := Editor:targetStart
        end if.

class predicates
    doIncrementalFind : (sciLexer Editor, charRange FindRange, string FindText) -> boolean Found.
clauses
    doIncrementalFind(Editor, FindRange, FindText) = Found :-
        if From = tryFind(Editor, FindRange, FindText, false) then
            To = Editor:targetEnd,
            setMatchSelection(Editor, From, To),
            % ToDo also format the replace result (in regExp case)
            Found = true
        else
            restoreOneSelection(Editor, true),
            Found = false
        end if.

class predicates
    formatStatus : (sciLexer Editor, boolean Found) -> string RegExpMsg.
clauses
    formatStatus(Editor, true) = S:getString() :-
        S = outputStream_string::new(),
        foreach true = regularExpression and N = std::fromTo(1, 9) do
            T = Editor:getTag(N),
            if "" <> T then
                S:writef(@" \% = '%'", N, T)
            end if
        end foreach.

    formatStatus(_Editor, false) = "No match".

class predicates
    tryFind : (sciLexer Editor, charRange SearcRange, string FindText, boolean ReplaceAll) -> integer FoundPos determ.
clauses
    tryFind(Editor, charRange(S, E), FindText, ReplaceAll) = Editor:trySearchInTarget(FindText) :-
        if true = ReplaceAll then
            S <= E
        end if,
        Editor:searchFlags := findFlags,
        Editor:targetStart := S,
        Editor:targetEnd := E.

class predicates
    initialFindRange : (sciLexer Editor) -> charRange Range.
clauses
    initialFindRange(Editor) =
        if true = inSelection then charRange(Editor:selectionStart, Editor:selectionEnd) else charRange(0, Editor:lastPos) end if.

class predicates
    setMatchSelection : (sciLexer Editor, integer From, integer To).
clauses
    setMatchSelection(Editor, From, To) :-
        if true = inSelection then
            Editor:multipleSelection := true,
            AX = Editor:anchor,
            CX = Editor:currentPos,
            Editor:setSelection(To, From),
            Editor:scrollCaret(),
            Editor:addSelection(CX, AX),
            Editor:mainSelection := 1
        else
            Editor:setSelection(To, From),
            Editor:scrollCaret()
        end if.

class predicates
    restoreOneSelection : (sciLexer Editor, boolean RestoreOld).
clauses
    restoreOneSelection(Editor, RestoreOld) :-
        if false = RestoreOld or false = inSelection then
            Editor:mainSelection := 0
        end if,
        Editor:multipleSelection := false.

predicates
    onNext_btnClick : button::clickResponder.
clauses
    onNext_btnClick(_Source) = button::defaultAction :-
        if Editor = tryGetEditor() then
            Found = incrementalFindNextRange(Editor),
            updateHistoryIfFoundOrNonIncremental(Found, incrementalFind)
        end if.

predicates
    onPrev_btnClick : button::clickResponder.
clauses
    onPrev_btnClick(_Source) = button::defaultAction :-
        if Editor = tryGetEditor() then
            Found = incrementalFindPreviousRange(Editor),
            updateHistoryIfFoundOrNonIncremental(Found, incrementalFind)
        end if.

clauses
    findNext() :-
        if Editor = sciLexer::tryGetEditor() then
            setScopeAndTarget(Editor),
            if FindAndReplaceControl = tryGetControl_nextPrevious(Editor) then
                _ = FindAndReplaceControl:incrementalFindNextRange(Editor)
            else
                nextPrev(Editor, charRange(Editor:selectionEnd, Editor:lastPos))
            end if
        end if.

clauses
    findPrevious() :-
        if Editor = sciLexer::tryGetEditor() then
            setScopeAndTarget(Editor),
            if FindAndReplaceControl = tryGetControl_nextPrevious(Editor) then
                _ = FindAndReplaceControl:incrementalFindPreviousRange(Editor)
            else
                nextPrev(Editor, charRange(Editor:selectionStart, 0))
            end if
        end if.

class predicates
    tryGetControl_nextPrevious : (sciLexer Editor) -> findAndReplaceControl Dialog determ.
clauses
    tryGetControl_nextPrevious(_Editor) = FindAndReplaceControl :-
        modeless_fact(FindAndReplaceControl),
        !.

    tryGetControl_nextPrevious(Editor) = displayModeless(Editor, asDialog) :-
        isMultiLineSelection(Editor).

class predicates
    nextPrev : (sciLexer Editor, charRange NextRange).
clauses
    nextPrev(Editor, Range) :-
        FindText = Editor:selText,
        if "" <> FindText then
            regularExpression := false,
            caseSensitive := false,
            wholeWords := false,
            inSelection := false,
            appendHistory(false, FindText)
        end if,
        _Status = doIncrementalFind(Editor, Range, findTextFromHistory).

class predicates
    nextRange : (sciLexer Editor) -> charRange NextRange.
clauses
    nextRange(Editor) = charRange(S, E) :-
        charRange(_, E) = initialFindRange(Editor),
        TS = Editor:targetStart,
        TE = Editor:targetEnd,
        S = math::max(TS, TE).

class predicates
    previousRange : (sciLexer Editor) -> charRange NextRange.
clauses
    previousRange(Editor) = charRange(E, S) :-
        charRange(S, _) = initialFindRange(Editor),
        TS = Editor:targetStart,
        TE = Editor:targetEnd,
        E = math::min(TS, TE).

predicates
    onReplace_btnClick : button::clickResponder.
clauses
    onReplace_btnClick(_Source) = button::defaultAction :-
        Editor = tryGetEditor(),
        !,
        if Editor:targetStart = Editor:targetEnd then
            _ = incrementalFindNextRange(Editor)
        else
            doReplace(Editor)
        end if.

    onReplace_btnClick(_Source) = button::defaultAction.

predicates
    doReplace : (sciLexer Editor).
clauses
    doReplace(Editor) :-
        replaceTarget_re(Editor, replaceText_edit:getText()),
        Found = incrementalFindNextRange(Editor),
        updateHistoryIfFoundOrNonIncremental(Found, incrementalFind).

predicates
    onReplaceAll_btnClick : button::clickResponder.
clauses
    onReplaceAll_btnClick(_Source) = button::defaultAction :-
        if Editor = tryGetEditor() then
            updateHistory(),
            Editor:beginUndoAction(),
            try
                Count = varM_unsigned::new(),
                replaceAll(Editor, initialFindRange(Editor), findText_edit:getText(), replaceText_edit:getText(), Count),
                statusText_ctl:setText(string::format("% replacements", Count:value))
            finally
                Editor:endUndoAction(),
                restoreOneSelection(Editor, true)
            end try
        end if.

class predicates
    replaceAll : (sciLexer Editor, charRange FindRange, string FindText, string ReplaceText, varM_unsigned Counter).
clauses
    replaceAll(Editor, FindRange, FindText, ReplaceText, Counter) :-
        if "" <> FindText and _Found = tryFind(Editor, FindRange, FindText, true) then
            replaceTarget_re(Editor, ReplaceText),
            if false = Editor:readOnly then
                Counter:inc(),
                replaceAll(Editor, nextRange(Editor), FindText, ReplaceText, Counter)
            end if
        end if.

class predicates
    replaceTarget_re : (sciLexer Editor, string Replace).
clauses
    replaceTarget_re(Editor, Replace) :-
        if true = regularExpression then
            Editor:replaceTargetRE(Replace)
        else
            Editor:replaceTarget(Replace)
        end if.

predicates
    onFindText_editModified : editControl::modifiedListener.
clauses
    onFindText_editModified(_Source) :-
        enableReplaceButtons(),
        if true = incremental_chk:getChecked() and not(_ = findText_edit:tryGetSelectedIndex()) then
            % we don't want to search if the change was caused by changing the selection in the control, only if it is by typing
            incrementalFindInit()
        end if.

predicates
    enableReplaceButtons : ().
clauses
    enableReplaceButtons() :-
        IsText = toBoolean("" <> findText_edit:getText()),
        replace_btn:setEnabled(IsText),
        replaceAll_btn:setEnabled(IsText).

predicates
    onIncremental_chkStateChanged : checkButton::stateChangedListener.
clauses
    onIncremental_chkStateChanged(_Source, _OldState, NewState) :-
        incrementalFind := incremental_chk:getChecked(),
        updateSettings(),
        if checkButton::checked = NewState then
            onFindText_editModified(findText_edit)
        else
            statusText_ctl:setText("")
        end if.

predicates
    onFirst_btnClick : button::clickResponder.
clauses
    onFirst_btnClick(_Source) = button::defaultAction :-
        incrementalFindInit().

predicates
    onWholeWords_chkStateChanged : checkButton::stateChangedListener.
clauses
    onWholeWords_chkStateChanged(_Source, _OldState, NewState) :-
        wholeWords := toBoolean(checkButton::checked = NewState),
        updateSettings(),
        incrementalFindInit().

predicates
    onCaseSensitive_chkStateChanged : checkButton::stateChangedListener.
clauses
    onCaseSensitive_chkStateChanged(_Source, _OldState, NewState) :-
        caseSensitive := toBoolean(checkButton::checked = NewState),
        updateSettings(),
        incrementalFindInit().

predicates
    onRegularExpression_chkStateChanged : checkButton::stateChangedListener.
clauses
    onRegularExpression_chkStateChanged(_Source, _OldState, NewState) :-
        regularExpression := toBoolean(checkButton::checked = NewState),
        updateSettings(),
        incrementalFindInit().

predicates
    onInSelection_chkStateChanged : checkButton::stateChangedListener.
clauses
    onInSelection_chkStateChanged(_Source, _OldState, _NewState) :-
        inSelection := inSelection_chk:getChecked(),
        incrementalFindInit().

predicates
    onDestroy : window::destroyListener.
clauses
    onDestroy(_Source) :-
        updateHistory(),
        if Editor = tryGetEditor() then
            restoreOneSelection(Editor, false)
        end if,
        retractAll(modeless_fact(This)).

class predicates
    getFirstOrEmpty : (string* List) -> string First.
clauses
    getFirstOrEmpty([]) = "".
    getFirstOrEmpty([F | _]) = F.

predicates
    onParentSize : window::sizeListener.
clauses
    onParentSize(_Source) :-
        if wideLayout <> toBoolean(getWidth() > layoutSwitchWidth) then
            wideLayout := ~~wideLayout,
            switchingLayout := true,
            switchLayout(wideLayout),
            %TODO: Sometimes we still get artifacts when switching layout
            vpi::winInvalidate(getParent():getVpiWindow()),
            switchingLayout := false
        end if.

facts
    wideLayout : boolean := true.
    switchingLayout : boolean := false.

constants
    layoutSwitchWidth = 280.
    minimalWidth = 172.

predicates
    switchLayout : (boolean WideLayout).
clauses
    switchLayout(false) :-
        getTopLevelContainerWindow():setHeight(getHeight() + controlBlock_ctl:getHeight() + 4),
        controlBlock_ctl:setPosition(8, 68),
        controlBlock_ctl:setAnchors([control::left, control::top]),
        foreach Ctl in [findText_edit, statusText_ctl, replaceText_edit, replaceText_ctl] do
            Ctl:setWidth(Ctl:getWidth() + controlBlock_ctl:getWidth())
        end foreach.

    switchLayout(true) :-
        getTopLevelContainerWindow():setHeight(getHeight() - controlBlock_ctl:getHeight() + 4),
        foreach Ctl in [findText_edit, statusText_ctl, replaceText_edit, replaceText_ctl] do
            Ctl:setWidth(Ctl:getWidth() - controlBlock_ctl:getWidth())
        end foreach,
        findText_edit:getPosition(FindTextX, _),
        controlBlock_ctl:setPosition(FindTextX + findText_edit:getWidth() + 4, 8),
        controlBlock_ctl:setAnchors([control::right, control::top]).

class facts
    writeRect : predicate{vpiDomains::rct} := erroneous.
    readRect : function_dt{vpiDomains::rct} := {  = _ :- fail }.

% This code is maintained automatically, do not update it manually. 15:02:46-7.12.2012
facts
    staticText_ctl : textControl.
    findText_edit : listEdit.
    statusText_ctl : textControl.
    replace_st : textControl.
    replaceText_edit : listEdit.
    replaceText_ctl : textControl.
    controlBlock_ctl : groupBox.
    first_btn : button.
    prev_btn : button.
    next_btn : button.
    replace_btn : button.
    replaceAll_btn : button.
    regularExpression_chk : checkButton.
    wholeWords_chk : checkButton.
    caseSensitive_chk : checkButton.
    inSelection_chk : checkButton.
    incremental_chk : checkButton.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Find and Replace"),
        This:setSize(460, 72),
        addDestroyListener(onDestroy),
        staticText_ctl := textControl::new(This),
        staticText_ctl:setText("F&ind:"),
        staticText_ctl:setPosition(8, 10),
        staticText_ctl:setSize(28, 10),
        findText_edit := listEdit::new(This),
        findText_edit:setText(""),
        findText_edit:setPosition(40, 10),
        findText_edit:setWidth(260),
        findText_edit:setSort(false),
        findText_edit:setAnchors([control::left, control::top, control::right]),
        statusText_ctl := textControl::new(This),
        statusText_ctl:setText(""),
        statusText_ctl:setPosition(40, 22),
        statusText_ctl:setSize(260, 10),
        statusText_ctl:setAnchors([control::left, control::top, control::right]),
        replace_st := textControl::new(This),
        replace_st:setText("R&eplace:"),
        replace_st:setPosition(8, 38),
        replace_st:setSize(28, 10),
        replaceText_edit := listEdit::new(This),
        replaceText_edit:setText(""),
        replaceText_edit:setPosition(40, 38),
        replaceText_edit:setWidth(260),
        replaceText_edit:setSort(false),
        replaceText_edit:setAnchors([control::left, control::top, control::right]),
        replaceText_ctl := textControl::new(This),
        replaceText_ctl:setText(""),
        replaceText_ctl:setPosition(40, 50),
        replaceText_ctl:setSize(260, 10),
        replaceText_ctl:setAnchors([control::left, control::top, control::right]),
        controlBlock_ctl := groupBox::new(This),
        controlBlock_ctl:setText(""),
        controlBlock_ctl:setPosition(304, 8),
        controlBlock_ctl:setSize(152, 66),
        controlBlock_ctl:setAnchors([control::top, control::right]),
        controlBlock_ctl:setBorderStyle(groupbox::noBorder()),
        first_btn := button::new(controlBlock_ctl),
        first_btn:setText("&First"),
        first_btn:setPosition(0, 0),
        first_btn:setWidth(22),
        first_btn:defaultHeight := true,
        first_btn:setAnchors([control::top, control::right]),
        first_btn:setClickResponder(onFirst_btnClick),
        prev_btn := button::new(controlBlock_ctl),
        prev_btn:setText("&Prev"),
        prev_btn:setPosition(23, 0),
        prev_btn:setWidth(22),
        prev_btn:defaultHeight := true,
        prev_btn:setAnchors([control::top, control::right]),
        prev_btn:setClickResponder(onPrev_btnClick),
        next_btn := button::new(controlBlock_ctl),
        next_btn:setText("&Next"),
        next_btn:setPosition(46, 0),
        next_btn:setWidth(22),
        next_btn:defaultHeight := true,
        next_btn:setAnchors([control::top, control::right]),
        next_btn:setClickResponder(onNext_btnClick),
        replace_btn := button::new(controlBlock_ctl),
        replace_btn:setText("&Replace"),
        replace_btn:setPosition(0, 18),
        replace_btn:setWidth(68),
        replace_btn:defaultHeight := true,
        replace_btn:setAnchors([control::top, control::right]),
        replace_btn:setEnabled(false),
        replace_btn:setClickResponder(onReplace_btnClick),
        replaceAll_btn := button::new(controlBlock_ctl),
        replaceAll_btn:setText("Replace &All"),
        replaceAll_btn:setPosition(0, 36),
        replaceAll_btn:setWidth(68),
        replaceAll_btn:defaultHeight := true,
        replaceAll_btn:setAnchors([control::top, control::right]),
        replaceAll_btn:setEnabled(false),
        replaceAll_btn:setClickResponder(onReplaceAll_btnClick),
        regularExpression_chk := checkButton::new(controlBlock_ctl),
        regularExpression_chk:setText("Regular E&xpression"),
        regularExpression_chk:setPosition(71, -2),
        regularExpression_chk:setWidth(76),
        regularExpression_chk:setAnchors([control::top, control::right]),
        wholeWords_chk := checkButton::new(controlBlock_ctl),
        wholeWords_chk:setText("Whole &Words"),
        wholeWords_chk:setPosition(71, 10),
        wholeWords_chk:setWidth(76),
        wholeWords_chk:setAnchors([control::top, control::right]),
        caseSensitive_chk := checkButton::new(controlBlock_ctl),
        caseSensitive_chk:setText("&Case Sensitive"),
        caseSensitive_chk:setPosition(71, 22),
        caseSensitive_chk:setWidth(76),
        caseSensitive_chk:setAnchors([control::top, control::right]),
        caseSensitive_chk:addStateChangedListener(onCaseSensitive_chkStateChanged),
        inSelection_chk := checkButton::new(controlBlock_ctl),
        inSelection_chk:setText("In &Selection"),
        inSelection_chk:setPosition(71, 34),
        inSelection_chk:setWidth(76),
        inSelection_chk:setAnchors([control::top, control::right]),
        incremental_chk := checkButton::new(controlBlock_ctl),
        incremental_chk:setText("Incre&mental Find"),
        incremental_chk:setPosition(71, 46),
        incremental_chk:setWidth(76).
    % end of automatic code

end implement findAndReplaceControl
