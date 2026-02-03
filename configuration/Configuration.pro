% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement configuration
    open core, list, vpiDomains

constants
    config_file_name = "pie32.ini".

class facts - start_error
    start_error : (string) determ.

class facts - config
    src_dir : (string) determ.
    msg_font : (vpiDomains::font) determ.
    msg_pos : (vpiDomains::rct) determ.
    edit_font : (vpiDomains::font) determ.
    edit_opts : (integer*) determ.
    task_pos : (vpiDomains::rct) determ.
    task_max : boolean := false.
    lru_file_pos : (string, vpiDomains::rct).
    open_editors : (string Filename, vpiDomains::rct, core::charCount Pos).

% 30-9-2025
 editor_pos : (vpiDomains::rct) determ.
 metta_dia_pos : (vpiDomains::rct) determ.


/***************************************************************************
    Locate the windows directory
***************************************************************************/
class predicates
    is_WindowMaximized : (window TaskWind) -> boolean IsMaxiimized.

class predicates
    save_init_file : (string).
    errorMessagePrompt : (string).
    consult_systemINIfile : (string FILENAME).
    load_defaults : ().

class predicates
    resize_Task : (window Wind).

class predicates
    resize_Msg : (window Wind).

class predicates
    get_win_restore_rct : (window Wind) -> vpiDomains::rct RECT.
clauses
 get_msg_height( He ) :- msg_pos( Rct ) , Rct = rct( X1, Y1, X2, Y2) , He = Y2 - Y1, He < 500 , ! .
 get_msg_height( 400 ) :- !.


    save_cfg() :-
        mainExe::getFileName(Path, _Name),
        FullName = fileName::createPath(Path, config_file_name),
        save_init_file(FullName),
        !.

clauses
    save_init_file(FullName) :-
        try
            file::save(FullName, config)
        catch _ do
            fail
        end try,
        !,
        stdio::writef("\nFile % saved", config_file_name).

    save_init_file(_) :-
        stdio::write("\nCan't save file ", config_file_name).

clauses
    consult_systemINIfile(FILENAME) :-
        file::existExactFile(FILENAME),
        MSG = string::format("Can not load %s. Ignored and defaults been set.", FILENAME),
        try
            file::consult(FILENAME, config)
        catch _ do
            errorMessagePrompt(MSG),
            fail()
        end try,
        !.

    consult_systemINIfile(FILENAME) :-
        MSG = string::format("There is no %s file. Defaults been set.", FILENAME),
        errorMessagePrompt(MSG),
        load_defaults().

clauses
    load_cfg() :-
        mainExe::getFileName(Path, _Name),
        FullName = fileName::createPath(Path, config_file_name),
        consult_systemINIfile(FullName).

clauses
    errorMessagePrompt(Err) :-
        retractFactDB(start_error),
        assert(start_error(Err)).

clauses
    get_start_error(Err) :-
        start_error(Err),
        !.

    get_start_error("Ready").

clauses
    load_defaults() :-
        retractFactDB(config),
        assert(src_dir("")).

%
%  Particular options settings
%
class predicates
    get_msg_font : (vpiDomains::font [out]).
clauses
    get_msg_font(FONT) :-
        msg_font(FONT),
        !.

    get_msg_font(FONT) :-
        FONT = vpi::fontCreateByName("Verdana", 12).

class predicates
set_msg_font : (vpiDomains::font).
clauses
set_msg_font(FONT) :-
        retractall(msg_font(_)),
        assert(msg_font(FONT)).

set_editor_pos( RCT ):-  retractall( editor_pos(_) ), assert( editor_pos(RCT) ).
set_metta_dia_pos( RCT ):-  retractall( metta_dia_pos(_) ), assert( metta_dia_pos(RCT) ).
% metta_dia_pos : (vpiDomains::rct) determ.

get_editor_pos( RCT ):-   editor_pos(RCT) , !.
get_editor_pos( rct(50,50,500,400) ):- !.

get_metta_dia_pos( RCT ):-   metta_dia_pos(RCT) ,!.

get_metta_dia_pos( rct(50,50,500,400) ):- !.



clauses
    get_edit_font(FONT) :-
        edit_font(FONT),
        !.

    get_edit_font(FONT) :-
        FONT = vpi::fontCreateByName("Verdana", 10).

class predicates
    set_edit_font : (vpiDomains::font).
clauses
    set_edit_font(FONT) :-
        retractall(edit_font(_)),
        assert(edit_font(FONT)).

clauses
    init_ui(TaskWin, MsgWin) :-
        resize_Task(TaskWin),
        resize_Msg(MsgWin),
        get_msg_font(MFONT),
        get_edit_font(EFONT),
        set_msg_font(MFONT),
        set_edit_font(EFONT),
        MsgWin:setFont(MFONT)
%        MsgWin:setCo
 %       MsgWin:setState( [ wsf_
        .

clauses
    resize_Task(Wind) :-
        Max = is_WindowMaximized(Wind),
        Max = true,
        !.

    resize_Task(Wind) :-
        task_pos(RECT),
        !,
        Wind:setOuterRect(RECT).

    resize_Task(Wind) :-
        set_task_pos(Wind).

clauses
    set_task_pos(Wind) :-
        RECT = Wind:getOuterRect(),
        retractall(task_pos(_)),
        assert(task_pos(RECT)),
        Max = is_WindowMaximized(Wind),
        task_max := Max.

clauses
    resize_Msg_rct(Wind, RECT ) :-
%        msg_pos(RECT),
 retractall( msg_pos(_) ),
         assert( msg_pos(RECT) ),
        !,
        Wind:setClientRect(RECT).

    resize_Msg(Wind) :- !.
    resize_Msg(Wind) :-
        msg_pos(RECT),
        !,
        Wind:setClientRect(RECT).

    resize_Msg(Wind) :-
        set_msg_pos(Wind).

clauses
set_msg_pos_rct( RCT ):-
        retractall(msg_pos(_)),
        assert(msg_pos(RCT)).

    set_msg_pos(Wind) :- !.
    set_msg_pos(Wind) :-
        RECT = get_win_restore_rct(Wind),
        retractall(msg_pos(_)),
        assert(msg_pos(RECT)).

clauses
    get_src_dir(Dir) :-
        src_dir(Dir),
        Dir <> "",
        !.

    get_src_dir(directory::getCurrentDirectory()).

clauses
    set_src_dir(FN) :-
%        Dir = fileName::getPath(FN),
        retractall(src_dir(_)),
    %23-6-2025
        assert(src_dir("example_prolog_programs\\")).

clauses
    set_lru_pos(FN, Rect, Pos) :-
        retractall(lru_file_pos(FN, _)),
        retractall(open_editors(FN, _, _)),
        asserta(lru_file_pos(FN, Rect)),
        asserta(open_editors(FN, Rect, Pos)).

clauses
    get_lru_list(LRU) :-
        LRU = [ FN || lru_file_pos(FN, _) ].

clauses
    get_lru_pos(_, FN, RCT, POS) :-
        open_editors(FN, RCT, POS),
        !.

    get_lru_pos(_, FN, RCT, 1) :-
        lru_file_pos(FN, RCT),
        !.

    get_lru_pos(WIN, _FN, RCT, 1) :-
        CRCT = vpi::winGetClientRect(WIN),
        CRCT = vpidomains::rct(L, T, R, B),
        L1 = L + 50,
        T1 = T + 50,
        R1 = 2 * R div 3,
        B1 = 2 * B div 3,
        RCT = vpidomains::rct(L1, T1, R1, B1).

clauses
    get_open_editors_list(LRU) :-
        LRU = [ FN || open_editors(FN, _, _) ].

clauses
    retract_open_editor(FN) :-
        retractall(open_editors(FN, _, _)).

clauses
    get_win_restore_rct(Wind) = RECT :-
        OuterRECT = Wind:getOuterRect(),
        RECT = Wind:adjustRectEx(OuterRECT).

clauses
    is_WindowMaximized(Wind) = true :-
        State = Wind:getState(),
        isMember(wsf_Maximized, State),
        !.

    is_WindowMaximized(_) = false.

end implement configuration