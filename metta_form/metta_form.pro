% Copyright ©

implement metta_form inherits formWindow
    open core, vpiDomains

clauses
    display(Parent) = Form :-
        Form = new(Parent),
        Form:show().

clauses
    new(Parent) :-
        formWindow::new(Parent),
        generatedInitialize().

% This code is maintained automatically, do not update it manually.
%  11:49:19-13.4.2025

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    sciLexer_ctl : scilexer.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("metta_form"),
        setRect(rct(50, 40, 535, 298)),
        setDecoration(titlebar([closeButton, maximizeButton, minimizeButton])),
        setBorder(sizeBorder()),
        setState([wsf_ClipSiblings, wsf_ClipChildren]),
        menuSet(noMenu),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(420, 238),
        ok_ctl:setSize(56, 16),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(4, 238),
        cancel_ctl:setSize(56, 16),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(216, 236),
        help_ctl:setSize(56, 16),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        sciLexer_ctl := scilexer::new(This),
        sciLexer_ctl:setPosition(36, 14),
        sciLexer_ctl:setSize(408, 186),
        sciLexer_ctl:setBorder(true).
% end of automatic code

end implement metta_form
