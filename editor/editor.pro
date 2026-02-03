% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

namespace pie

implement editor inherits formWindow
    open core, vpiDomains, resourceIdentifiers, pfc\pie

class facts - edit_window
    edit_window : (editor This, string FileName) nondeterm.
    app_close : () determ.

class facts - editx
edx_win : ( window ).
position_metta_dialog : ( vpiDomains::rct ) determ.

class predicates
%set_size : () procedure().
get_editor_position : ( vpiDomains::rct  ) determ( o ).

clauses

set_position_custom( RCT ) :-  retractall( position_metta_dialog( _ ) ) ,
 assert( position_metta_dialog( RCT ) ) .

remove_position_custom( RCT ) :-  retractall( position_metta_dialog( _ ) ) , !.

get_editor_position( RECT2 ):-
  % vpiCommonDialogs::note( "Try get succes", "succe3" ),
  % position_custom( RECT2 )  ,
 % vpiCommonDialogs::note( "succes", "succe3" ),
% 19-5-2025
      position_metta_dialog( _ ),

      configuration::get_msg_height( He ),
      Tw = vpi::getTaskWindow(),
      RECT = vpi::winGetClientRect( Tw ),
      RECT = rct( X1 , Y1 , X2 , Y2 ),
      RECT2 = rct( X1 + 30 , Y2 - He , X2 - 30 , Y2 - 65 ),

 ! .

get_editor_position( RECT2 ):-
      configuration::get_msg_height( He ),
      Tw = vpi::getTaskWindow(),
      RECT = vpi::winGetClientRect( Tw ),
      RECT = rct( X1 , Y1 , X2 , Y2 ),
      RECT2 = rct( X1 + 20 ,Y1 + 35 ,X2 - 20 ,Y2 - He - 65   ), !.


set_size_rct( RECT ):-
 edx_win( Formx ) ,

%      get_editor_position( RECT ),
%      configuration::get_msg_height( He ),
%      Tw = vpi::getTaskWindow(),
%      RECT = vpi::winGetClientRect( Tw ),
%      RECT = rct( X1 , Y1 , X2 , Y2 ),
%      RECT2 = rct( X1 + 20 ,Y1 + 35 ,X2 - 20 ,Y2 - He - 65   ),
      Formx:setClientRect( RECT ) , ! .


      %  setOuterRect( RECT2 ) , !.
set_size_rct( _ ):- !.

% 30-9-2025
set_size():-
%             configuration::set_metta_dia_pos( RECT2 ),
      configuration::get_editor_pos( RECT3 ),
    edx_win( Formx ) ,
    Formx:setClientRect( RECT3 ) ,
!.

set_size():-
 edx_win( Formx ) ,

      get_editor_position( RECT2 ),
%      configuration::get_msg_height( He ),
%      Tw = vpi::getTaskWindow(),
%      RECT = vpi::winGetClientRect( Tw ),
%      RECT = rct( X1 , Y1 , X2 , Y2 ),
%      RECT2 = rct( X1 + 20 ,Y1 + 35 ,X2 - 20 ,Y2 - He - 65   ),
      Formx:setClientRect( RECT2 ) , ! .


      %  setOuterRect( RECT2 ) , !.
set_size():- !.

class predicates
    display : (window Parent, string FileName, string Text) -> editor Editor.
clauses
    display(Parent, FileName, Text) = Form :-
        Form = new(Parent, FileName, Text),
        retractall( edx_win( _ ) ), assert( edx_win( Form ) ),
        Form:show().

class predicates
    get_file_data : (string, string [out], boolean) determ.
clauses
    get_file_data(FileName, Text, _) :-
        FileName <> "",
        file::existExactFile(FileName),
        !,
        try
            Text = file::readString(FileName, _)
        catch E do
            error_handler(string::format("Unable to load file '%s'", FileName), E),
            fail
        end try.

    get_file_data(FN, "", true) :-
        !,
        Msg = string::format("File '%s' does not exist. Create?", FN),
        1 =
            vpiCommonDialogs::messageBox("Prolog Inference Engine", Msg, mesbox_iconQuestion, mesbox_buttonsYesNo, mesbox_defaultFirst,
                mesbox_suspendApplication).

    get_file_data(_, "", _).

constructors
    new : (window Parent, string FileName, string Text).
clauses
    new(Parent, FileName, Text) :-
        formWindow::new(Parent),
        generatedInitialize(),
        configuration::get_edit_font(_Font),
        % 18-5-2025
         setFont(vpi::fontCreateByName("Tahoma", 12)),
        % setFont(Font),
        TW = Parent:getVpiWindow(),
        editorControl_ctl:dockStyle := control::dockFill,

         configuration::get_lru_pos(TW, Filename, Rct, Pos),
        if file::existExactFile(FileName) then
            ReadOnly =
                toBoolean(
                    fileSystem_native::file_attribute_readonly =
                        bit::bitand(fileSystem_native::getFileAttributes(FileName), fileSystem_native::file_attribute_readonly))
        else
            ReadOnly = false
        end if,
        Title0 = get_title(FileName),
        if true = ReadOnly then
            Title = string::format("%s [ReadOnly]", Title0)
        else
            Title = Title0
        end if,
        setText(Title),
        editorControl_ctl:whenCreated(
            {  :-
                editorControl_ctl:text := Text,
                editorControl_ctl:lexerDefault_visualProlog(),
% 1-11-2025
% sciLexer_native::style_lineNumber = 33

% editorControl_ctl:styleSetBack( 33, 0x000000 ),
 editorControl_ctl:styleSetBack( sciLexer_native::style_lineNumber, 0x000000 ),
                editorControl_ctl:gotoPos(Pos),
                editorControl_ctl:readOnly := ReadOnly,
                editorControl_ctl:useTabs := false,
                editorControl_ctl:setSavepoint(),
                editorControl_ctl:emptyUndoBuffer()
            }),
        show_menu_with_filelists(id_TaskMenu, NewMenu),
        menuSet(NewMenu),
        addMenuItemListener(id_edit_font, onEditFont),
        addMenuItemListener(id_Change_Case_set_lower, onChangeCaseSetLower),
        addMenuItemListener(id_Change_Case_set_upper, onChangeCaseSetUpper),
        addMenuItemListener(id_Change_Case_toggle, onChangeCaseToggle),
        addMenuItemListener(id_edit_autoindent, onEditAutoindent),
        addMenuItemListener(id_file_save, onFileSave),
        addMenuItemListener(id_file_save_as, onFileSaveAs),
        addMenuItemListener(id_edit_cut, onEditCut),
        addMenuItemListener(id_edit_copy, onEditCopy),
        addMenuItemListener(id_edit_paste, onEditPaste),
        addMenuItemListener(id_edit_undo, onEditUndo),
        addMenuItemListener(id_edit_redo, onEditRedo),
        addMenuItemListener(id_edit_delete, onEditDelete),
        addMenuItemListener(id_edit_search, onEditSearch),
        addMenuItemListener(id_edit_replace, onEditReplace),
        addMenuItemListener(id_edit_search_again, onEditSearchAgain),
        addMenuItemListener(id_edit_goto_line, onEditGotoLine),
        addMenuItemListener(id_edit_insert, onEditInsert),
        addMenuItemListener(id_edit_goto_clause, onEditGotoClause),
        addMenuItemListener(id_Engine_reconsult, onEngineReconsult),
        addMenuItemListener(id_help_local, onHelpLocal),
        frameWindow:addMenuItemListener({ (_, MenuItem) :- _ = applicationWindow::get():sendEvent(e_Menu(MenuItem, 0)) }),
        assert(edit_window(This, FileName)).

class predicates
    error_handler : (string, exception::traceId).
clauses
    error_handler(S, E) :-
        STR = string::format("% - Error code = %", S, exception::getErrorCode(E)),
        vpiCommonDialogs::error(STR).

clauses
    show_menu_with_filelists(MENU, NewMenu) :-
        OldMenu = vpi::menuGetRes(MENU),
        OldMenu = dynMenu([txt(F1, F2, F3, F4, F5, OldFSubmenu) | REST]),
        !,
        build_file_submenu(OldFSubmenu, NewFSubMenu),
        NewMenu = dynMenu([txt(F1, F2, F3, F4, F5, NewFSubmenu) | REST]).

    show_menu_with_filelists(MENU, _) :-
        exception::raise_error("Obtained menu is not a dynMenu. ResID = ", MENU).

class predicates
    get_title : (string) -> string.
clauses
    get_title(FileName) = Title :-
        Path = directory::getCurrentDirectory(),
        fileName::getPathAndName(FileName, FPath, Title),
        string::equalIgnoreCase(Path, FPath),
        !.

    get_title(FileName) = FileName.

class predicates
    replace_menu_in_all_windows : ().
clauses
    replace_menu_in_all_windows() :-
        Task_win = vpi::getTaskWindow(),
        show_menu_with_filelists(id_TaskMenu, NewMenu),
        vpi::menuSet(Task_win, NewMenu).

class predicates
    build_file_submenu : (menuItem*, menuItem* [out]).
clauses
    build_file_submenu(InSubmenu, OutSubmenu) :-
        configuration::get_lru_list(SList),
        not(Slist = []),
        !,
        stringlist2itemlist(filemenutagbase, filemenutagbase, SList, IList),
        OutSubmenu = list::append(InSubMenu, [separator | IList]).

    build_file_submenu(Submenu, Submenu).

predicates
    check_true_item : (menuTag [out]) nondeterm.
clauses
    check_true_item(id_edit_undo) :-
        %     if undo buffer is not empty
        true = editorControl_ctl:canUndo.

    check_true_item(id_edit_redo) :-
        %     if undo buffer is not empty
        true = editorControl_ctl:canRedo.

    check_true_item(Item) :-
        % if block marked
        editorControl_ctl:selectionStart <> editorControl_ctl:selectionEnd,
        Item = list::getMember_nd([id_edit_cut, id_edit_copy, id_edit_delete]).

    check_true_item(id_edit_paste) :-
        vpi::cbStringAvailable.

    check_true_item(Item) :-
        Item =
            list::getMember_nd(
                [
                    id_file_save,
                    id_file_save_as,
                    id_Engine_reconsult,
                    id_edit_goto_line,
                    id_edit_goto_clause,
                    id_edit_insert,
                    id_edit_search,
                    id_edit_replace,
                    id_edit_search_again
                ]).

predicates
    check_false_item : (menuTag [out]) nondeterm.
clauses
    check_false_item(id_edit_undo) :-
        %     if undo buffer is empty
        false = editorControl_ctl:canUndo.

    check_false_item(id_edit_redo) :-
        %     if undo buffer is empty
        false = editorControl_ctl:canRedo.

    check_false_item(Item) :-
        %     block is not marked
        editorControl_ctl:selectionStart = editorControl_ctl:selectionEnd,
        Item = list::getMember_nd([id_edit_cut, id_edit_copy, id_edit_delete]).

    check_false_item(Item) :-
        true = editorControl_ctl:readOnly,
        Item =
            list::getMember_nd(
                [
                    id_edit_cut,
                    id_edit_paste,
                    id_edit_delete,
                    id_edit_insert,
                    id_change_case,
                    id_change_case_set_upper,
                    id_change_case_set_lower,
                    id_change_case_toggle,
                    id_edit_replace
                ]).

    check_false_item(id_edit_paste) :-
        not(vpi::cbStringAvailable).

predicates
    save_as : (string).
clauses
    save_as(NewName) :-
        edit_window(_, NewName),
        !,
        vpiCommonDialogs::note("Can't write(process active)").

    save_as(NewName) :-
        file::existExactFile(NewName),
        true =
            toBoolean(
                fileSystem_native::file_attribute_readonly =
                    bit::bitand(fileSystem_native::getFileAttributes(NewName), fileSystem_native::file_attribute_readonly)),
        !,
        vpiCommonDialogs::note("Can't write (file - Read Only)").

    save_as(NewName) :-
        file::existExactFile(NewName),
        Prompt = string::format("Rewrite '%s'?", NewName),
        %    Resp = vpiCommonDialogs::ask("Prolog Inference Engine", Prompt, ["Yes", "No", ""]),
        %    Resp <> vpiCommonDialogs::resp_default,
        1
            <> vpiCommonDialogs::messageBox("Prolog Inference Engine", Prompt, mesbox_iconQuestion, mesbox_buttonsYesNoCancel, mesbox_defaultFirst,
                mesbox_suspendApplication),
        !.

    save_as(NewName) :-
        retractall(edit_window(This, _)),
        assert(edit_window(This, NewName)),
        Title = get_title(NewName),
        setText(Title),
        Text = editorControl_ctl:text,
        file::writeString(NewName, Text),
        editorControl_ctl:setSavePoint().

predicates
    menuitem_switch : (menuTag*, boolean).
clauses
    menuitem_switch([], _) :-
        menuUpdate(),
        !.

    menuitem_switch([First | Rest], Boolean) :-
        menuEnable(First, Boolean),
        menuitem_switch(Rest, Boolean).

class predicates
    menuPopup_Switch : (menuItem*, menuTag*, booleanInt, menuItem* [out]).
clauses
    menuPopup_Switch([], _, _, []) :-
        !.

    menuPopup_Switch([txt(TAG, TEXT, CHR, _, CHCK, ChildMNU) | T], TAGLIST, Bool, [txt(TAG, TEXT, CHR, Bool, CHCK, NewChildMNU) | MNU]) :-
        menuPopup_Switch(ChildMNU, TAGLIST, Bool, NewChildMNU),
        TAG in TAGLIST,
        !,
        menuPopup_Switch(T, TAGLIST, Bool, MNU).

    menuPopup_Switch([txt(TAG, TEXT, CHR, Enable, CHCK, ChildMNU) | T], TAGLIST, Bool, [txt(TAG, TEXT, CHR, Enable, CHCK, NewChildMNU) | MNU]) :-
        menuPopup_Switch(ChildMNU, TAGLIST, Bool, NewChildMNU),
        !,
        menuPopup_Switch(T, TAGLIST, Bool, MNU).

    menuPopup_Switch([H | T], TAGLIST, Bool, [H | MNU]) :-
        menuPopup_Switch(T, TAGLIST, Bool, MNU).

predicates
    insert_clause : ().
clauses
    insert_clause() :-
        SLIST = stdPredicate::all(),
        Title = "Select Clause",
        PreSel = 0,
        _ = vpiCommonDialogs::listSelect(Title, SLIST, PreSel, StrSel, _),
        if "" <> StrSel then
            Prototype = stdPredicate::getPrototype(StrSel),
            StrToInsert = string::format("\t%s, \n", Prototype),
            Pos = editorControl_ctl:currentPos,
            insert_predicate(Pos, StrToInsert)
        end if.

clauses
    clean_up_data(Close) :-
        edit_window(This, FileName),
        editorControl_ctl:isModified(),
        !,
        Title = get_title(FileName),
        Prompt = string::format("Save '%s'?", Title),
        Resp =
            vpiCommonDialogs::messageBox("Prolog Inference Engine", Prompt, mesbox_iconQuestion, mesbox_buttonsYesNoCancel, mesbox_defaultFirst,
                    mesbox_suspendApplication) - 1,
        NewData = editorControl_ctl:text,
        save_no_cancel(Resp, FileName, NewData, Close).

    clean_up_data(b_True) :-
        retract(edit_window(This, FileName)),
        !,
        retract_editor(FileName).

class predicates
    stringlist2itemlist : (menuTag, menuTag, string*, menuItem* [out]).
clauses
    stringlist2itemlist(_, _, [], []) :-
        !.
    stringlist2itemlist(Base, Tag, [Str | RestS], [txt(Tag, Line, noAccelerator(), b_true, b_false, []) | RestI]) :-
        Tag1 = Tag + 1,
        Number = Tag1 - Base,
        reduce_(Str, StrReduced, 45),
        %YI
        Line = string::format("&% %", Number, StrReduced),
        stringlist2itemlist(Base, Tag1, RestS, RestI).

predicates
    insert_predicate : (charCount, string).
clauses
    insert_predicate(Pos, What) :-
        editorControl_ctl:home(),
        BeginPos = editorControl_ctl:currentPos,
        editorControl_ctl:lineEnd(),
        EndPos = editorControl_ctl:currentPos,
        BeginPos = EndPos,
        !,
        del_last_enter(What, What1),
        editorControl_ctl:insertText(Pos, What1).

    insert_predicate(Pos, What) :-
        editorControl_ctl:lineEnd(),
        EndPos = editorControl_ctl:currentPos,
        Pos = EndPos,
        !,
        editorControl_ctl:home(),
        editorControl_ctl:insertText(editorControl_ctl:currentPos, What).

    insert_predicate(Pos, What) :-
        editorControl_ctl:home,
        BeginPos = editorControl_ctl:currentPos,
        Text = getEntireText(),
        NBytes = Pos - BeginPos,
        NBytes > 0,
        !,
        SubString = string::subString(Text, BeginPos, NBytes),
        insert_predicate_in_line(SubString, What, BeginPos, Pos).

    insert_predicate(Pos, What) :-
        editorControl_ctl:insertText(Pos, What).

predicates
    insert_predicate_in_line : (string, string, charCount, charCount).
clauses
    insert_predicate_in_line(SubString, What, _, Pos) :-
        is_space_and_tab(SubString),
        !,
        del_last_enter(What, What1),
        del_first_tab(What1, What2),
        editorControl_ctl:insertText(Pos, What2).

    insert_predicate_in_line(_, What, BeginPos, _) :-
        !,
        editorControl_ctl:insertText(BeginPos, What).

class predicates
    save_no_cancel : (integer, string, string, booleanInt [out]).
clauses
    save_no_cancel(vpiCommonDialogs::resp_default, FN, Data, b_True) :-
        %        trap(file5x::file_str(FN, Data), C, (error_handler("Failed writing", C), fail)), !,
        saveIfExist(FN),
        try
            file::writeString(FN, Data)
        catch C do
            error_handler("Failed writing", C),
            fail
        end try,
        !,
        retractall(edit_window(_, FN)),
        retract_editor(FN).

    save_no_cancel(vpiCommonDialogs::resp_2, FN, _, b_True) :-
        !,
        retractall(edit_window(_, FN)),
        !,
        retract_editor(FN).

    save_no_cancel(_, _, _, b_False).

class predicates
    retract_editor : (string).
clauses
    retract_editor(_) :-
        app_close,
        !.

    retract_editor(FN) :-
        configuration::retract_open_editor(FN).

class predicates
    reduce_ : (string, string [out], integer).
clauses
    reduce_(Str, Str, MLen) :-
        Len = string::length(Str),
        Len < MLen,
        !.

    reduce_(Str, StrReduced, _) :-
        find_head(Str, Head),
        find_tail(0, Str, Tail),
        !,
        StrReduced = string::format("%s...%s", Head, Tail),
        !.

    reduce_(Str, Str, _).

class predicates
    del_last_enter : (string, string [out]).
clauses
    del_last_enter(What, What1) :-
        string::front(What, string::length(What) - 1, What1, SH),
        SH = "\n",
        !.

    del_last_enter(What, What) :-
        !.

class predicates
    is_space_and_tab : (string) determ.
clauses
    is_space_and_tab("") :-
        !.

    is_space_and_tab(S) :-
        string::frontChar(S, CH, S1),
        CH = ' ',
        !,
        is_space_and_tab(S1).

    is_space_and_tab(S) :-
        string::frontChar(S, CH1, S1),
        CH1 = '\t',
        !,
        is_space_and_tab(S1).

class predicates
    find_head : (string, string [out]) determ.
clauses
    find_head(Str, Head) :-
        Pos1 = string::searchChar(Str, '\\'),
        string::front(Str, Pos1 + 1, Start1, Rest),
        Pos2 = string::searchChar(Rest, '\\'),
        string::front(Rest, Pos2 + 1, Start2, _),
        Head = string::concat(Start1, Start2),
        !.

class predicates
    find_tail : (integer, string, string [out]) determ.
clauses
    find_tail(2, _, "") :-
        !.

    find_tail(I, Str, Tail) :-
        Len = string::length(Str),
        Pos = Len - 1,
        string::front(Str, Pos, Start, Rest),
        Rest = @"\",
        !,
        I1 = I + 1,
        find_tail(I1, Start, Tail1),
        Tail = string::concat(Tail1, Rest).

    find_tail(I, Str, Tail) :-
        !,
        Len = string::length(Str),
        Pos = Len - 1,
        string::front(Str, Pos, Start, Rest),
        find_tail(I, Start, Tail1),
        Tail = string::concat(Tail1, Rest).

class predicates
    del_first_tab : (string, string [out]).
clauses
    del_first_tab(S, S1) :-
        string::frontchar(S, CH, S1),
        CH = '\t',
        !.

    del_first_tab(S, S) :-
        !.

clauses
    close_editors() = false :-
        retractall(app_close),
        assert(app_close),
        edit_window(W, _),
        W:clean_up_data(Continue),
        Continue = b_False,
        % CANCEL is Pressed
        !,
        retractall(app_close).

    close_editors() = true :-
        retractall(app_close).

clauses
    open_file(FileName, _) :-
        edit_window(EW, FileName),
        !,
        if EW:isMinimized() then
            EW:setMinimized(false)
        end if,
        EW:setFocus().

    open_file(FileName, Prompt) :-
        get_file_data(FileName, Text, Prompt),
% 18-5-2025
%          setFont(vpi::fontCreateByName("Tahoma", 10)),
        _ = display(applicationWindow::get(), FileName, Text).

clauses
    getText(FileName) = Text :-
        edit_window(EW, FileName),
        !,
        Text = EW:getEntireText().

clauses
    getEntireText() = editorControl_ctl:text.

predicates
    set_lru_pos : ().
clauses
    set_lru_pos() :-
        if edit_window(This, Filename) and ! and editorControl_ctl:isShown() then
            configuration::set_lru_pos(FileName, getOuterRect(), editorControl_ctl:currentPos)
        end if.

predicates
    initMenu : documentWindow::initMenuListener.
clauses
    initMenu(_) :-
        menuitem_switch([ TrueItem || check_true_item(TrueItem) ], true),
        menuitem_switch([ FalseItem || check_false_item(FalseItem) ], false).

predicates
    onShow : window::showListener.
clauses
    onShow(_, _Data) :-
        vpiToolbar::mesRedirect(vpi::getTaskWindow(), getVpiWindow()),
%        set_lru_pos(),

        set_size(),

%       configuration::get_msg_height( He ),
 %     Tw = vpi::getTaskWindow(),
  %    RECT = vpi::winGetClientRect( Tw ),
   %   RECT = rct( X1 , Y1 , X2 , Y2 ),
    %  RECT2 = rct( X1 + 10 ,Y1 + 0 ,X2 - 10 ,Y2 - He - 55   ),

     %   setOuterRect(RECT2),

        replace_menu_in_all_windows(),
        Taskwin = vpi::getTaskWindow(),
        vpiToolbar::mesRedirect(Taskwin, getVpiWindow()).

predicates
    onSizeChanged : window::sizeListener.
clauses
    onSizeChanged(_) :-
        set_lru_pos().
        % set_size().

predicates
    onContextMenu : window::contextMenuResponder.
clauses
    onContextMenu(_Source, _Input) = window::defaultContextMenuHandling :-
        contextMenu(cursorGetPos()).

predicates
    contextMenu : (pnt Point).
clauses
    contextMenu(Point) :-
        MNU = vpi::menuGetRes(idr_popup),
        MNU == dynMenu(MLIST1),
        ItemList1 = [ Item1 || check_True_Item(Item1) ],
        ItemList2 = [ Item2 || check_False_Item(Item2) ],
        menuPopup_Switch(MLIST1, ItemList1, b_True, MLIST2),
        menuPopup_Switch(MLIST2, ItemList2, b_False, MLIST3),
        MNU2 = dynMenu(MLIST3),
        frameWindow:setFocus(),
        vpi::menuPopUp(frameWindow:getVpiWindow(), MNU2, Point, align_Left).

predicates
    onGetFocus : window::getFocusListener.
clauses
    onGetFocus(_) :-
        Taskwin = vpi::getTaskWindow(),
        if VPI = tryGetVpiWindow() then
            vpiToolbar::mesRedirect(Taskwin, VPI)
        end if.

predicates
    onMove : window::moveListener.
clauses
    onMove(_) :-
        set_lru_pos().

predicates
    onFileSave : window::menuItemListener.
clauses
    onFileSave(_, _) :-
        fileSave().

class predicates
    saveIfExist : (string FileName).
clauses
    saveIfExist(FileName) :-
        if file::existExactFile(FileName) then
            NewName = fileName::setExtension(FileName, "bak"),
            file::move(FileName, NewName, file::overwrite())
        end if.

predicates
    fileSave : ().
clauses
    fileSave() :-
        editorControl_ctl:isModified(),
        edit_window(This, FileName),
        !,
        Text = getEntireText(),
        saveIfExist(FileName),
        file::writeString(FileName, Text),
        editorControl_ctl:setSavePoint().

    fileSave().

predicates
    onFileSaveAs : window::menuItemListener.
clauses
    onFileSaveAs(_, _) :-
%23-6-2025
 SavedDir = "example_prolog_programs\\",
%        configuration::get_src_dir(SavedDir),
        NewName =
            vpiCommonDialogs::getFileName("*.pro", ["Prolog files(*.pro)", "*.pro", "PIE Files(*.pie)", "*.pie", "All files(*.*)", "*, *"],
                "Save file as...", [dlgfn_Save], SavedDir, _),
        fileName::getPathAndName(NewName, _Path, _Name),
%23-6-2025
%        configuration::set_src_dir(Path),
        configuration::set_src_dir( "example_prolog_programs\\" ),
        edit_window(This, OldName),
        OldName <> NewName,
        !,
        save_as(NewName).

    onFileSaveAs(_, _).

predicates
    onEditCut : window::menuItemListener.
clauses
    onEditCut(_, _) :-
        editorControl_ctl:cut().

predicates
    onEditCopy : window::menuItemListener.
clauses
    onEditCopy(_, _) :-
        editorControl_ctl:copy().

predicates
    onEditPaste : window::menuItemListener.
clauses
    onEditPaste(_, _) :-
        editorControl_ctl:paste().

predicates
    onEditUndo : window::menuItemListener.
clauses
    onEditUndo(_, _) :-
        editorControl_ctl:undo().

predicates
    onEditRedo : window::menuItemListener.
clauses
    onEditRedo(_, _) :-
        editorControl_ctl:redo().

predicates
    onEditDelete : window::menuItemListener.
clauses
    onEditDelete(_, _) :-
        editorControl_ctl:clear().

predicates
    onEditSearch : window::menuItemListener.
clauses
    onEditSearch(_, _) :-
        _ = editorControl_ctl:displayFindAndReplace_modal().

predicates
    onEditReplace : window::menuItemListener.
clauses
    onEditReplace(_, _) :-
        _ = editorControl_ctl:displayFindAndReplace_modal().

predicates
    onEditSearchAgain : window::menuItemListener.
clauses
    onEditSearchAgain(_, _) :-
        _ = editorControl_ctl:displayIncrementalFind_modal().

predicates
    onEditGotoLine : window::menuItemListener.
clauses
    onEditGotoLine(_, _) :-
        _ = editorControl_ctl:displayGotoLine_modal().

predicates
    onEditInsert : window::menuItemListener.
clauses
    onEditInsert(_, _) :-
        insert_clause().

predicates
    onEditGotoClause : window::menuItemListener.
clauses
    onEditGotoClause(_, _) :-
        PredicateNames = safePie::getPredicateNames(),
        not(PredicateNames = []),
        Title = "Select clause",
        PreSel = 1,
        _ = vpiCommonDialogs::listSelect(Title, PredicateNames, PreSel, StrSel, _),
        Text = getEntireText(),
        not(StrSel = ""),
        FoundPos = string::search(Text, StrSel) + 1,
        FoundPos > 0,
        editorControl_ctl:currentPos := FoundPos,
        !.

    onEditGotoClause(_, _).

predicates
    onEngineReconsult : window::menuItemListener.
clauses
    onEngineReconsult(_, _) :-
        fileSave(),
        edit_window(This, FileName),
        !,
        Text = getEntireText(),
        safePie::reconsult_text(Text),
        Status = string::format("Reconsult '%s'", FileName),
        taskWindow::system_status(Status),
        stdio::write("Reconsulted from: ", FileName),
        stdio::nl,
        taskWindow::system_status("Ready").

    onEngineReconsult(_, _) :-
        exception::raise_error("change file name to fact variable").

predicates
    onHelpLocal : window::menuItemListener.
clauses
    onHelpLocal(_, _) :-
        Sel1 = editorControl_ctl:selectionStart,
        Sel2 = editorControl_ctl:selectionEnd,
        not(Sel1 = Sel2),
        !,
        Key = editorControl_ctl:getTextRange(Sel1, Sel2),
        htmlHelp::invoke(getVpiWindow(), @"..\help\pie.chm", htmlhelp::hh_display_index, uncheckedConvert(unsignedNative, Key)).

    onHelpLocal(_, _) :-
        htmlHelp::invoke(getVpiWindow(), @"..\help\pie.chm", 0, 0).

predicates
    onCloseRequest : frameDecoration::closeResponder.
clauses
    onCloseRequest(_) = frameDecoration::denyClose :-
        clean_up_data(Close),
        TASKWIN = vpi::getTaskWindow(),
        vpiToolbar::mesRedirect(TASKWIN, TASKWIN),
        Close = b_False,
        !.

    onCloseRequest(_) = frameDecoration::acceptClose.

predicates
    onEditFont : window::menuItemListener.
clauses
    onEditFont(_, _) :-
        Font = vpiCommonDialogs::getFont(editorControl_ctl:getFont()),
        !,
        editorControl_ctl:setFont(Font).

    onEditFont(_, _).

predicates
    onChangeCaseSetLower : window::menuItemListener.
clauses
    onChangeCaseSetLower(_, _) :-
        editorControl_ctl:lowerCase().

predicates
    onChangeCaseSetUpper : window::menuItemListener.
clauses
    onChangeCaseSetUpper(_, _) :-
        editorControl_ctl:upperCase().

class predicates
    onChangeCaseToggle : window::menuItemListener.
clauses
    onChangeCaseToggle(_, _) :-
        %editorControl_ctl:reverseCase().
        succeed.

predicates
    onEditAutoindent : window::menuItemListener.
clauses
    onEditAutoindent(_, _) :-
        editorControl_ctl:useTabs := boolean::logicalNot(editorControl_ctl:useTabs).

% This code is maintained automatically, do not update it manually. 15:19:50-6.9.2012
facts
    editorControl_ctl : scilexer.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setFont(vpi::fontCreateByName("Tahoma", 12)),
        setText("editor"),
        setRect(rct(50, 40, 430, 291)),
        setDecoration(titlebar([closebutton(), maximizebutton(), minimizebutton()])),
        setBorder(sizeBorder()),
        setState([wsf_ClipSiblings, wsf_ClipChildren]),
        menuSet(noMenu),
        addGetFocusListener(onGetFocus),
        addMoveListener(onMove),
        addShowListener(onShow),
        addSizeListener(onSizeChanged),
        setContextMenuResponder(onContextMenu),
        setCloseResponder(onCloseRequest),
        addInitMenuListener(initMenu),
        editorControl_ctl := scilexer::new(This),
        editorControl_ctl:setPosition(60, 4),
        editorControl_ctl:setSize(108, 112),
        editorControl_ctl:usePopup := false().
    % end of automatic code

end implement editor
