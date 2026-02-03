% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement goalWindow inherits messageForm

delegate
    interface sciLexerBase to sciLexer

class facts - temp
meswin : (messageControl ) determ.
meswin_height : (unsigned ) determ.

constructors
    new : (applicationWindow Parent).
clauses
    new(Parent) :-
        messageForm::new(Parent),
        MsgCtl = getMessageControl(),
%        retractall( meswin(_)),assert(meswin(MsgCtl ) ),
%        MsgCtl:setVerticalSize( fixed( 30 ) ),
    get_message_height( Hx ),
         messageForm::setVerticalSize( fixed( Hx ) ),
%0x525254
 %        fixed(unsigned PixelHeight);
%    parentRelative(ratio Ratio).
        MsgCtl:setLines(1000),
        stdio::outputStream := outputStream_processEvents::new(MsgCtl:outputStream),
        setText("CONSOLE"),
%        addSizeListener(onResize),
%        addMoveListener(onMove),
        MsgCtl:postAction(
            {  :-
                readOnly := false,
                assignCmdKey(sciLexerBase::key_return, sciLexer_native::sci_null),
%                        caretLineVisible := true,
        caretLineBack := 0x000000
% sci_styleSetFont
%                      styleSetBack( 32, 0xFFFFFF )
% dit staat ie wel toe , alleen niet alles wordt wit
%                      styleSetFore( 32, 0x000000 )
                % we handle newline in eventHandler
            }),
%  MsgCtl:setFont
%     Fo_ed = vpi::fontCreateByName("Tahoma", 14),     MsgCtl:setFont( Fo_ed ) ,
        MsgCtl:addHandler(eventHandler).

clauses
    create(Parent) = MsgWin :-
        MsgWin = goalWindow::new(Parent),
% 1-11-2025
%   MsgWin:setForeColor(vpiDomains::color_White),
        MsgWin:show().

class predicates
    onResize : sizeListener.
clauses
% Witgh new buttons we have to destroy the message window and re create
% it with another size  how we want it
%set_message():- !.
get_message_height( H ):-   meswin_height( H ) , !.
get_message_height( 30 ):-   !.

set_message( H ):-
 retractall( meswin_height( _ ) ),
 assert( meswin_height( H ) ).
 %       messageForm::setVerticalSize( messageForm::fixed( 30 ) ), !.
 %        fixed(unsigned PixelHeight);
%    parentRelative(ratio Ratio).

%        meswin( MsgCtl ),
 %       MsgCtl:setVerticalSize( fixed( 30 ) ), !.
%        MsgCtl:setLines(1000),


    onResize(Wind) :-!.
    onResize(Wind) :-
        configuration::set_msg_pos(Wind).

class predicates
    onMove : moveListener.
clauses
    onMove(Wind) :- !.
    onMove(Wind) :-
        configuration::set_msg_pos(Wind).

predicates
    eventHandler : vpiDomains::ehandler.
clauses
    eventHandler(_Win, vpidomains::e_Char(13, _)) = nullHandle :-        !,
        MyGoal = string::replaceAll(getLine(lineFromPosition(currentPos)), "\n", ""),
        gotoPos(lastPos),
        if 1 < getColumn(currentPos) then
            newline()
        end if,
        replaceSel(MyGoal),
        newline(),
        try_run_goal(MyGoal).

%    eventHandler(_Win, vpidomains::e_Char(Char, Ctl)) = nullHandle :-
%        %retract(readChar_fact()),
%        %!,
%        Char >= 0x20,
%        !,
%        if 0 = bit::bitAnd(Ctl, vpiDomains::c_Control + vpiDomains::c_Alt) then
%        C = uncheckedConvert(char, convert(integer16, Char)),
%            stdio::write(C)
%        end if.
class predicates
    try_run_goal : (string).
clauses
    try_run_goal(TheGoal0) :-
        safePie::convert_metta_commandline( TheGoal0 , TheGoal ), ! ,
        Status = string::format("Run goal: %s", TheGoal),
        taskWindow::system_status(Status),
        safePie::run(TheGoal),
        taskWindow::system_status("Ready").
   try_run_goal( _TheGoal0) :-  !.

end implement goalWindow
