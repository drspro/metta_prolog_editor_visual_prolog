% Copyright PDC

namespace sciLexerSupport

implement incrementalFindDialog inherits dialog
    open vpiDomains

clauses
    displayModal(Parent) = Dialog :-
        Dialog = newModal(Parent),
        Dialog:show().

constructors
    newModal : (sciLexer Owner).
clauses
    newModal(Owner) :-
        owner := Owner,
        dialog::new(Owner),
        generatedInitialize(),
        addNativeMessageHandler(onNative).

facts
    owner : sciLexer [constant].

class predicates
    onNative : nativeMessageHandler.
clauses
    onNative(_Window, Message, WParam, _LParam) =
            nativeResult(uncheckedConvert(gui_native::lResult, gui_native::getStockObject(gui_native::white_brush))) :-
        gui_api::isCtrlColorMsg(Message),
        uxTheme_api::isAppThemed(),
        !,
        DC = uncheckedConvert(gui_native::hDC, WParam),
        _ = gui_native::setBkColor(DC, color_white).

    onNative(Window, gui_native::wm_command, uncheckedConvert(gui_native::wParam, idc_cancel), _LParam) = defaultNativeHandling :-
        !,
        _ = gui_native::postMessage(Window:getVpiWindow(), gui_native::wm_close, gui_api::wNull, gui_api::lNull).

    onNative(_Window, _Msg, _WParam, _LParam) = defaultNativeHandling.

predicates
    incrementalSearch : (integer FromPos).
clauses
    incrementalSearch(FromPos) :-
        owner:searchFlags := 0,
        owner:targetStart := FromPos,
        owner:targetEnd := owner:lastPos,
        if From = owner:trySearchInTarget(findText_edit:getText()) then
            To = owner:targetEnd,
            owner:setSelection(To, From),
            owner:scrollCaret()
        else
            owner:gotoPos(owner:lastPos)
        end if.

predicates
    onFindText_editModified : editControl::modifiedListener.
clauses
    onFindText_editModified(_Source) :-
        incrementalSearch(0).

predicates
    onForward_btnClick : button::clickResponder.
clauses
    onForward_btnClick(_Source) = button::defaultAction :-
        incrementalSearch(owner:targetEnd).

% This code is maintained automatically, do not update it manually. 12:10:44-20.4.2015
facts
    findText_edit : editControl.
    forward_btn : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Find"),
        setRect(rct(50, 40, 252, 60)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        findText_edit := editControl::new(This),
        findText_edit:setText(""),
        findText_edit:setPosition(4, 4),
        findText_edit:setWidth(176),
        findText_edit:addModifiedListener(onFindText_editModified),
        forward_btn := button::new(This),
        forward_btn:setText(">"),
        forward_btn:setPosition(184, 2),
        forward_btn:setWidth(16),
        forward_btn:defaultHeight := true,
        forward_btn:setClickResponder(onForward_btnClick),
        setDefaultButton(forward_btn).
    % end of automatic code

end implement incrementalFindDialog
