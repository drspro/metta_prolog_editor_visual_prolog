% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement taskWindow inherits applicationWindow
    open core, vpiDomains

constants
    mdiProperty : boolean = true.

layoutx1 = 100.
layoutx2 = 102.
layoutx3 = 103.
layoutx4 = 104.
layoutx5 = 105.

clauses
    new() :-
        applicationWindow::new(),
        generatedInitialize().

class facts - bigpos
bigstr_dia : ( window ).

class facts - edit_window
    edit_count : integer := 0.

class facts
    msgWin : goalWindow := erroneous.

class predicates
    open_editors : (string*).

layout1 : ( integer, integer, integer , integer , integer , unsigned , vpiDomains::rct, vpiDomains::rct ) determ(i, i,i,i,i,i,o,o).

clauses

layout1( layoutx1,  X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ):-
      Bot_marge = 125,
      Height = Y2 - Y1 - He_messwin - Bot_marge,
      Half = math::round( Height / 2 ),
      Ystart_metta = Y1 + 55, Yend_metta = Ystart_metta + Half,
      RECT2 = rct( X1 + 10 ,Ystart_metta + 10 ,X2 - 10 ,  Yend_metta   ),
     RECT3 = rct( X1 + 17 ,Yend_metta + 10, X2 - 17 , Yend_metta + Half  ),!.


layout1( layoutx2,  X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ):-
      Bot_marge = 125,
      Height = Y2 - Y1 - He_messwin - Bot_marge,
      Upper = math::round( Height * 0.25 ) ,
      Lower = math::round( Height * 0.75 ) ,
      Ystart_metta = Y1 + 55,
      Yend_metta = Ystart_metta + Upper,
      RECT2 = rct( X1 + 10 , Ystart_metta  + 10, X2 - 10 ,  Yend_metta   ),
      RECT3 = rct( X1 + 17 , Yend_metta + 10, X2 - 17 , Yend_metta + Lower  ),!.


layout1( layoutx3,  X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ):-
      Bot_marge = 125,
      Height = Y2 - Y1 - He_messwin - Bot_marge,
      Upper = math::round( Height * 0.75 ) ,
      Lower = math::round( Height * 0.25 ) ,
      Ystart_metta = Y1 + 55,
      Yend_metta = Ystart_metta + Upper,
      RECT2 = rct( X1 + 10 , Ystart_metta + 10 , X2 - 10 ,  Yend_metta   ),
      RECT3 = rct( X1 + 17 , Yend_metta + 10, X2 - 17 , Yend_metta + Lower  ),!.

layout1( layoutx4,  X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ):-
      Bot_marge = 125,
      Height = Y2 - Y1 - He_messwin - Bot_marge,
      Wid = X2 - X1,
      Half = math::round( Wid * 0.5 ) ,
      Halfy = math::round( Height * 1 ) ,

      Ystart_metta = Y1 + 55,
      Yend_metta = Ystart_metta + Halfy,
      RECT2 = rct( X1 + 12 , Ystart_metta + 10, X2 - Half - 5 ,  Yend_metta  + 31 ),
      RECT3 = rct( X2 - Half + 5 , Ystart_metta - 15 , X2 - 17 , Yend_metta  ),!.


    open_editors([]).

    open_editors([H | T]) :-
        pie\editor::open_file(H, true),
        !,
        open_editors(T).

    open_editors([H | T]) :-
        configuration::retract_open_editor(H),
        !,
        open_editors(T).

class predicates
    next_filename : () -> string FileName.
clauses
    next_filename() = FileName :-
        Path = directory::getCurrentDirectory(),
        FileName0 = string::format(@"%s\FILE%u.PRO", Path, edit_count),
        edit_count := edit_count + 1,
        FileName = next_filename(FileName0).

class predicates
    next_filename : (string FileName0) -> string FileName1.
clauses
    next_filename(FileName0) = FileName1 :-
        file::existExactFile(FileName0),
        !,
        FileName1 = next_filename().

    next_filename(FileName) = FileName.

predicates
    onShow : window::showListener.
clauses
    onShow(TaskWind, _CreationData) :-
        configuration::load_cfg(),
         goalWindow::set_message( 100 ),
        msgWin := goalWindow::create(This),
%      Fo_ed = vpi::fontCreateByName("Tahoma", 18),
%     msgWin:setFont( Fo_ed ) ,
%  sciLexer:setFont( Fo_ed ) ,
         configuration::init_ui(TaskWind, msgWin),

%         configuration::resize_Msg_rct( msgWin , rct(10,10,200,200) ) ,
        configuration::get_open_editors_list(LRU),
        TaskWind:setState([wsf_Visible]),
        TaskWind:setText( "Metta" ),
        pie\editor::show_menu_with_filelists(resourceIdentifiers::id_TaskMenu, NewMenu),
        This:menuSet(NewMenu),
        system_status("Ready"),
        configuration::get_start_error(Err),
        stdio::write(Err, "\n"),
%        stdio::write("Use OnlineHelp to see descriptions ...\n"),
%        stdio::write("To load example files choose File\\Consult menu entry ...\n"),
        stdio::write("Type your predicate calls here like\nwrite(\"Hello world!\"). <- Set Caret here and press Enter\n"),
        open_editors(LRU),
        addMenuItemListener(onFileTag),
         Dia = bigstr::display(TaskWind),
         retractall( bigstr_dia( _ ) ), assert( bigstr_dia( Dia ) ).

        % postEvent( e_menu( resourceIdentifiers::id_file_text_to_sterm , 0 ) ).

class predicates
    onDestroy : window::destroyListener.
clauses
    onDestroy(_).

class predicates
    onHelpAbout : window::menuItemListener.
clauses
    onHelpAbout(TaskWin, _MenuTag) :-
        AboutDialog = aboutDialog::new(TaskWin),
        AboutDialog:show().

predicates
    onFileExit : window::menuItemListener.
clauses
    onFileExit(_, _MenuTag) :-
        close().

predicates
    onSizeChanged : window::sizeListener.
clauses
    onSizeChanged(Wind) :-
        configuration::set_task_pos(Wind),
        vpiToolbar::resize(getVPIWindow()),
        bigstr_dia( Dia ),
        Tw = vpi::getTaskWindow(),   RECT = vpi::winGetClientRect(Tw),
      RECT = rct( X1, Y1, X2, Y2 ),
      goalWindow::get_message_height( He_messwin ),
      layout1( layoutx1 , X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ),
%      Bot_marge = 125,
%      Height = Y2 - Y1 - He_messwin - Bot_marge,
%      Half = math::round( Height / 2 ),
%      Ystart_metta = Y1 + 55, Yend_metta = Ystart_metta + Half,
%      RECT2 = rct( X1 + 10 ,Ystart_metta ,X2 - 10 ,  Yend_metta   ),
     bigstr::set_dia_size_rct( Dia , RECT2 ),
%     RECT3 = rct( X1 + 17 ,Yend_metta + 10, X2 - 17 , Yend_metta + Half  ),

         pie\editor::set_size_rct( RECT3 ),
%      goalWindow::set_message( 20 ),


   %      configuration::resize_Msg_rct( msgWin , RECT4 ) ,
  % stdio::write("lalalla"),
%   messageForm::setVerticalSize( fixed( 30 ) ),

    %        fixed(unsigned PixelHeight);
%    parentRelative(ratio Ratio).

%          MsgCtl = getMessageControl(),
 %       MsgCtl:setVerticalSize( 5 ),


%          vpiCommonDialogs::note("aaa"),
% orginal
%        bigstr::set_dia_size( Dia ) ,
%        pie\editor::set_size(),
        ! .

 onSizeChanged(_Wind) :- !.

/******************************************************
*        Display system information                            *
******************************************************/
clauses
    system_status(STATUS) :-
        Task = vpi::getTaskWindow(),
        UsedStack = memory::getUsedStack(),
        UsedHeap = memory::getUsedHeap(),
        StackTxt = string::format("%", UsedStack),
        MEMTxt = string::format("%", UsedHeap),
        get_trace_txt(TT),
        vpiToolbar::setValue(Task, resourceIdentifiers::idt_help_stack, vpitoolbar::text_value(StackTxt)),
        vpiToolbar::setValue(Task, resourceIdentifiers::idt_help_memory, vpitoolbar::text_value(MEMTxt)),
        vpiToolbar::setValue(Task, resourceIdentifiers::idt_help_line, vpitoolbar::text_value(STATUS)),
        vpiToolbar::setValue(Task, resourceIdentifiers::idt_help_pause, vpitoolbar::text_value("")),
        vpiToolbar::setValue(Task, resourceIdentifiers::idt_help_trace, vpitoolbar::text_value(TT)).

class predicates
    get_trace_txt : (string [out]).
clauses
    get_trace_txt("TRACE") :-
        true = safePie::traceOn,
        !.

    get_trace_txt("").

class predicates
    onFileNew : window::menuItemListener.
clauses
    onFileNew(_Source, _MenuTag) :-
        FileName = next_filename(),
        pie\editor::open_file(FileName, false),
        !.

    onFileNew(_Source, _MenuTag).

class predicates
    onFileOpen : window::menuItemListener.
clauses
    onFileOpen(_Source, _MenuTag) :-
%        configuration::get_src_dir(SavedDir),
        Fpro = mainExe::getFileName(), Pa0 = fileName::getPath(Fpro), SavedDir = string::concat( Pa0, "example_prolog_programs\\" ),
        % SavedDir = "example_prolog_programs\\",
        FileName =
            vpiCommonDialogs::getFileName("*.pro", ["Prolog files(*.pro)", "*.pro", "PIE Files(*.pie)", "*.pie", "All Files(*.*)", "*.*"],
                "Open file", [], SavedDir, _),
        % configuration::set_src_dir(SavedDir),
        pie\editor::open_file(FileName, true),
        !.

    onFileOpen(_Source, _MenuTag).

class predicates
    onFileConsult : window::menuItemListener.
clauses
    onFileConsult(_Source, _MenuTag) :-
        % configuration::get_src_dir(SavedDir),
        Fpro = mainExe::getFileName(), Pa0 = fileName::getPath(Fpro), SavedDir = string::concat( Pa0, "example_prolog_programs\\" ),
        % SavedDir = "example_prolog_programs\\",
        FileName =
            vpiCommonDialogs::getFileName("*.pro", ["Prolog files(*.pro)", "*.pro", "PIE Files(*.pie)", "*.pie", "All Files(*.*)", "*.*"],
                "Open file", [], SavedDir, _),
        fileName::getPathAndName(FileName, Path, _Name),
        % configuration::set_src_dir(Path),
        pie\editor::open_file(FileName, true),
        Text = pie\editor::getText(FileName),
        !,
          %3-9-2025
          safePie::retractAll(),

        safePie::reconsult_text(Text),
        Status = string::format("Reconsult %s", FileName),
        system_status(Status),
        stdio::write("Consulted from: ", FileName, "\n"),
        system_status("Ready").

    onFileConsult(_Source, _MenuTag).

class predicates
    onEngineReset : window::menuItemListener.
clauses
    onEngineReset(_Source, _MenuTag) :-
        safePie::retractAll(),
        vpiCommonDialogs::note("The Inference Engine is reset."),
        system_status("Ready").

class predicates
    onEngineTraceCalls : window::menuItemListener.
clauses
    onEngineTraceCalls(_Source, _MenuTag) :-
        true = safePie::traceOn,
        !,
        safePie::traceOn := false,
        stdio::write("Trace is OFF\n"),
        system_status("Ready").

    onEngineTraceCalls(_Source, _MenuTag) :-
        safePie::traceOn := true,
        stdio::write("Trace is On\n"),
        system_status("Ready").

class predicates
    onEngineStopExecution : window::menuItemListener.
clauses
    onEngineStopExecution(_Source, _MenuTag) :-
        safePie::terminateExecution(),
        stdio::write("System stopped via user break\n").

class predicates
    onClose : frameDecoration::closeResponder.
clauses
    onClose(_Source) = frameDecoration::acceptClose() :-
        configuration::save_cfg(),
        Close = pie\editor::close_editors(),
        clean_the_application(Close),
        Close = false,
        !.

    onClose(_Source) = frameDecoration::acceptClose().

class predicates
    clean_the_application : (boolean).
clauses
    clean_the_application(true) :-
        !,
        safePie::terminateExecution().

    clean_the_application(_).

predicates
    onHelpContents : window::menuItemListener.
clauses
    onHelpContents(_Source, _MenuTag) :-
        htmlHelp::invoke(This:getVpiWindow(), @"..\help\pie.chm", 0, 0).

/******************************************************
*    File menu handling                                *
******************************************************/
class predicates
    get_file : (integer, integer, core::string_list ListNames) -> string FileName determ.
clauses
    get_file(_, _, []) = _ :-
        !,
        fail().

    get_file(N, I, _) = _ :-
        N < I,
        !,
        fail().

    get_file(N, N, [Name | _]) = Name :-
        !.

    get_file(N, I, [_ | ListNames]) = Name :-
        !,
        I1 = I + 1,
        Name = get_file(N, I1, ListNames).

class predicates
    onFileTag : window::menuItemListener.
clauses
    onFileTag(_Source, FileListTag) :-
        FileListTag >= pie\editor::filemenutagbase,
        FileListTag < pie\editor::filemenutagbase + 10,
        N = FileListTag - pie\editor::filemenutagbase,
        configuration::get_lru_list(FNLIST),
        Name = get_file(N, 0, FNLIST),
        pie\editor::open_file(Name, true),
        !.

    onFileTag(_Source, _FileListTag).

class predicates
    onMove : window::moveListener.
clauses

onMove(Wind) :-
        configuration::set_task_pos(Wind),
%        vpiToolbar::resize(getVPIWindow()),
        bigstr_dia( Dia ),
        Tw = vpi::getTaskWindow(),   RECT = vpi::winGetClientRect(Tw),
      RECT = rct( X1, Y1, X2, Y2 ),
      goalWindow::get_message_height( He_messwin ),
      layout1( layoutx1 , X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ),
     bigstr::set_dia_size_rct( Dia , RECT2 ),
         pie\editor::set_size_rct( RECT3 ),
        ! .


 %       configuration::set_task_pos(Wind),
  %           bigstr_dia( Dia ),
   %     bigstr::set_dia_size( Dia ) , ! .
 onMove(_Wind) :- !.

predicates
    onFileTextToSterm : window::menuItemListener.
clauses
onFileTextToSterm( Source, _MenuTag):-
 Dia = bigstr::display( Source ) ,
 retractall( bigstr_dia( _ ) ), assert( bigstr_dia( Dia ) ),
 !.


onFileTextToSterm(_Source, _MenuTag).


predicates
    onFileMettaForm : window::menuItemListener.
clauses
    onFileMettaForm(_Source, _MenuTag).
predicates
    onPaint : window::paintResponder.
clauses
    onPaint(_Source, _Rectangle, _GDIObject).
predicates
    onNative : window::nativeMessageHandler.
clauses
    onNative(_Source, _Message, _WParam, _LParam) = window::defaultNativeHandling.
predicates
    onFileLayout1 : window::menuItemListener.
clauses
onFileLayout1(_Source, _MenuTag):-
     bigstr_dia( Dia ),
     Tw = vpi::getTaskWindow(),   RECT = vpi::winGetClientRect(Tw),
      RECT = rct( X1, Y1, X2, Y2 ),
      goalWindow::get_message_height( He_messwin ),
      layout1( layoutx1 , X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ),
      configuration::set_metta_dia_pos( RECT2 ),
      configuration::set_editor_pos( RECT3 ),

     bigstr::set_dia_size_rct( Dia , RECT2 ),
     pie\editor::set_size_rct( RECT3 ), !.
onFileLayout1(_Source, _MenuTag).

predicates
onFileLayout2 : window::menuItemListener.
clauses
onFileLayout2(_Source, _MenuTag):-
     bigstr_dia( Dia ),
     Tw = vpi::getTaskWindow(),   RECT = vpi::winGetClientRect(Tw),
      RECT = rct( X1, Y1, X2, Y2 ),
      goalWindow::get_message_height( He_messwin ),
      layout1( layoutx2 , X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ),
            configuration::set_metta_dia_pos( RECT2 ),
      configuration::set_editor_pos( RECT3 ),

     bigstr::set_dia_size_rct( Dia , RECT2 ),
     pie\editor::set_size_rct( RECT3 ), !.
onFileLayout2(_Source, _MenuTag).
predicates
onFileLayout3 : window::menuItemListener.
clauses
onFileLayout3(_Source, _MenuTag):-
     bigstr_dia( Dia ),
     Tw = vpi::getTaskWindow(),   RECT = vpi::winGetClientRect(Tw),
      RECT = rct( X1, Y1, X2, Y2 ),
      goalWindow::get_message_height( He_messwin ),
      layout1( layoutx3 , X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ),
            configuration::set_metta_dia_pos( RECT2 ),
      configuration::set_editor_pos( RECT3 ),

     bigstr::set_dia_size_rct( Dia , RECT2 ),
     pie\editor::set_size_rct( RECT3 ), !.
onFileLayout3(_Source, _MenuTag).
predicates
onFileLayout4 : window::menuItemListener.
clauses
onFileLayout4(_Source, _MenuTag):-
     bigstr_dia( Dia ),
     Tw = vpi::getTaskWindow(),   RECT = vpi::winGetClientRect(Tw),
      RECT = rct( X1, Y1, X2, Y2 ),
      goalWindow::get_message_height( He_messwin ),
      layout1( layoutx4 , X1, Y1 , X2 , Y2 , He_messwin , RECT2, RECT3 ),
            configuration::set_metta_dia_pos( RECT2 ),
      configuration::set_editor_pos( RECT3 ),

     bigstr::set_dia_size_rct( Dia , RECT2 ),
     pie\editor::set_size_rct( RECT3 ), !.
onFileLayout4(_Source, _MenuTag).
predicates
    on : window::menuItemListener.
clauses
    on(_Source, _MenuTag).
predicates
    onEditSearch : window::menuItemListener.
clauses
    onEditSearch(_Source, _MenuTag).
predicates
    onEdit : window::menuItemListener.
clauses
    onEdit(_Source, _MenuTag).
% This code is maintained automatically, do not update it manually.
%  13:17:28-13.12.2025

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("PIE"),
        setDecoration(titlebar([closeButton, maximizeButton, minimizeButton])),
        setBorder(sizeBorder()),
        setState([wsf_ClipSiblings]),
        whenCreated(
            {  :-
                projectToolbar::create(getVpiWindow()),
                statusLine::create(getVpiWindow())
            }),
        addSizeListener({  :- vpiToolbar::resize(getVpiWindow()) }),
        setMdiProperty(mdiProperty),
        menuSet(resMenu(resourceIdentifiers::id_TaskMenu)),
        addShowListener(onShow),
        setPaintResponder(onPaint),
        addSizeListener(onSizeChanged),
        addMoveListener(onMove),
        setCloseResponder(onClose),
        addDestroyListener(onDestroy),
        addNativeMessageHandler(onNative),
        addMenuItemListener(resourceIdentifiers::id_help_about, onHelpAbout),
        addMenuItemListener(resourceIdentifiers::id_file_exit, onFileExit),
        addMenuItemListener(resourceIdentifiers::id_file_new, onFileNew),
        addMenuItemListener(resourceIdentifiers::id_file_open, onFileOpen),
        addMenuItemListener(resourceIdentifiers::id_file_consult, onFileConsult),
        addMenuItemListener(resourceIdentifiers::id_engine_reset, onEngineReset),
        addMenuItemListener(resourceIdentifiers::id_Engine_trace_calls, onEngineTraceCalls),
        addMenuItemListener(resourceIdentifiers::id_Engine_stop_execution, onEngineStopExecution),
        addMenuItemListener(resourceIdentifiers::id_help_contents, onHelpContents),
        addMenuItemListener(resourceIdentifiers::id_file_text_to_sterm, onFileTextToSterm),
        addMenuItemListener(resourceIdentifiers::id_file_metta_form, onFileMettaForm),
        addMenuItemListener(resourceIdentifiers::id_file_layout1, onFileLayout1),
        addMenuItemListener(resourceIdentifiers::id_file_layout2, onFileLayout2),
        addMenuItemListener(resourceIdentifiers::id_file_layout3, onFileLayout3),
        addMenuItemListener(resourceIdentifiers::id_file_layout4, onFileLayout4),
        addMenuItemListener(resourceIdentifiers::idt_5, on),
        addMenuItemListener(resourceIdentifiers::idt_8, on),
        addMenuItemListener(resourceIdentifiers::idt_9, on),
        addMenuItemListener(resourceIdentifiers::idt_10, on),
        addMenuItemListener(resourceIdentifiers::id_edit_search, onEditSearch).
    % end of automatic code

end implement taskWindow
