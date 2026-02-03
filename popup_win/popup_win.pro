% Copyright ©

implement popup_win inherits dialog
    open core, vpiDomains

clauses
    display(Parent) = Dialog :-
        Dialog = new(Parent),
        Dialog:show().

clauses
    new(Parent) :-
        dialog::new(Parent),
        generatedInitialize().





predicates
onShow : window::showListener.
clauses
onShow( Source, _Data):-
 %Ha = vpi::winGetCtlHandle( _Source ),
  %vpi::winCenter( Ha ),
    Source:center(),
        sciLexer_ctl:lexerDefault_visualProlog(),
%         sciLexer_ctl:styleSetBack( sciLexer::style_default, 0xFFFFFF ),
% DIT WERKT 1-11-2025 we willen nu hetzefde in de message form doen
%          sciLexer_ctl:styleSetBack( 32, 0xFFFFFF ),
           sciLexer_ctl:styleSetBack( sciLexer_native::style_lineNumber , 0x000000 ),
% style_lineNumber = 33
% Tahoma Verdana
   Fo_ed = vpi::fontCreateByName("Tahoma", 12),
  sciLexer_ctl:setFont( Fo_ed ) ,

  Fn = "metta_programs\\stdlib_mettalog.metta" ,
  Txa00 = file::readString( Fn ,  _ ) , ! ,
            Txa0 = string::replaceAll( Txa00 , "%Undefined%" , "xUndefinedx" ),
            Txa02 = string::replaceAll( Txa0 , ";" , "%" ),
            Txa03 = string::replaceAll( Txa02 , "cdr-atom" , "cdr_atom" ),
            Txa04 = string::replaceAll( Txa03 , "car-atom" , "car_atom" ),
            Txa05 = string::replaceAll( Txa04 , "let*" , "let_star" ),
            Txa06 = string::replaceAll( Txa05 , "add-atom-a" , "add_atom_a" ),
            Txa07 = string::replaceAll( Txa06 , "add-atom-z" , "add_atom_z" ),
            Txa08 = string::replaceAll( Txa07 , "remove-all-atoms" , "remove_all_atoms" ),
            Txa09 = string::replaceAll( Txa08 , "remove-atom" , "remove_atom" ),
            Txa10 = string::replaceAll( Txa09 , "add-atom" , "add_atom" ),
            Txa11 = string::replaceAll( Txa10 , "remove-atom" , "remove_atom" ),
            Txa12 = string::replaceAll( Txa11 , "cons-atom" , "cons_atom" ),
            Txa13 = string::replaceAll( Txa12 , "size-atom" , "size_atom" ),
       sciLexer_ctl:text := Txa13 ,

%    safePie::tokenize_x( Txa13 , TOKENS ) ,
%    safePie::parse_metta1_x( TOKENS , Cmp_clause , _Rest_toks ) ,

                      %  Tw = applicationWindow::get(), Tw:
                      setText( Fn ), !.



onShow(_Source, _Data).

predicates
    onPushButtonClick : button::clickResponder.
clauses
    onPushButtonClick(_Source) = button::defaultAction.
% Search button voor  tags  default direct help search

% This code is maintained automatically, do not update it manually.
%  12:37:21-24.12.2025

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    sciLexer_ctl : scilexer.
    pushButton_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("popup_win"),
        setRect(rct(50, 40, 405, 372)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setBorder(sizeBorder()),
        setState([wsf_NoClipSiblings]),
        addShowListener(onShow),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(148, 312),
        ok_ctl:setSize(56, 16),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(4, 312),
        cancel_ctl:setSize(56, 16),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(80, 312),
        help_ctl:setSize(56, 16),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        sciLexer_ctl := scilexer::new(This),
        sciLexer_ctl:setPosition(4, 4),
        sciLexer_ctl:setSize(348, 304),
        pushButton_ctl := button::new(This),
        pushButton_ctl:setText("Next"),
        pushButton_ctl:setPosition(248, 312),
        pushButton_ctl:setWidth(84),
        pushButton_ctl:defaultHeight := true,
        pushButton_ctl:setClickResponder(onPushButtonClick).
% end of automatic code

end implement popup_win
