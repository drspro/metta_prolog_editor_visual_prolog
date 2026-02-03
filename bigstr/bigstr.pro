% Copyright ©

implement bigstr inherits dialog
    open core, vpiDomains

%predicates
%set_dia_size : (window) procedure(i).
class facts - bedo
no_debug : ( boolean ) determ.
current_example : ( string ) determ.
mous_dbl : () determ.
mous_down : () determ.

%current_lexer : ( sciLexer ) determ.

predicates

find_varaiabel_names : ( string, string*, string* ) procedure( i , i , o ).
replace_all_variabel_names2 : ( string , string* , string ) determ( i,i,o).
replace_all_variabel_names : ( string , string* , string ) procedure( i,i,o).

if_there_is_an_error_goto_pos_and_fail : ( integer , integer, integer ) determ( i ,i,i).

assist_syntax : ( pfc\pie\pie::sterm Term [in], pfc\pie\pie::sterm Term [out] ) procedure( i,o ).

functie_voor_selected_text : ( window , integer , integer , integer , pfc\pie\pie::source  ) determ( i, i,i,i,i ).

clauses

functie_voor_selected_text( _Win , Start, End, Leng , Selected ) :-
  vpiCommonDialogs::note( "selection is ", Selected ) ,
    _Pos = string::search( Selected , "(" ),

    safePie::tokenize_x( Selected , TOKENS ) ,
     vpiCommonDialogs::note( " toks is  is ", toString( TOKENS ) ) ,
    safePie::parse_metta1_x( TOKENS , Cmp_clause , _Rest_toks ) ,
 vpiCommonDialogs::note( "cmp clauxx", toString( Cmp_clause ) ) ,
%    safePie::parse_metta1_x( Selected , Cmp_clause ) ,
    assist_syntax( Cmp_clause, Cmp_clause2 ) ,
    safePie::cmp_clause_to_metta_x( Cmp_clause2 , Metta_str ) ,

   sciLexer_ctl:deleteRange( Start , Leng ) ,

    sciLexer_ctl:insertText( Metta_str ) , ! .

functie_voor_selected_text( Par , _Start, _End, _Leng , _Selected ) :-
 _Dia = popup_win:: display( Par ) ,

  !.


assist_syntax( Cmp_clause, Cmp_clause2 ) :-
 Cmp_clause = pfc\pie\pie::cmp( Pred, Ato_list ) ,
  Cmp_clause2 =  pfc\pie\pie::cmp( "aaa", [ pfc\pie\pie::cmp( Pred, Ato_list ) ] ),
 !.

    % pfc\pie\pie::sterm Term [out]
assist_syntax( Cmp_clause, Cmp_clause ) :- !.



% goto
if_there_is_an_error_goto_pos_and_fail( 0 , _, _ ):- !.

if_there_is_an_error_goto_pos_and_fail( Result , Line , Linepos ):-
  Xs = string::format( "Position: % line: % position in line: % ", Result , Line , Linepos ),
  vpiCommonDialogs::note( " result parse pos ", Xs ) ,
 goto_sci_edit_pos( Result ), !,
% goto_sci_edit_pos( 10 ), !,
 fail.




find_varaiabel_names( Txa0, HL, Names_list0 ):-
 Pos = string::search( Txa0, "$" ),
 string::tryFront( Txa0, Pos + 1, _, Rest ),
 string5x::frontToken( Rest, Tok , Rest2 ), !,
% string::tryFront( Tok, 1, Fc, Rest_tok ),
% Fc2 = string::toUpperCase( Fc ),
 %Tok_up = string::concat( Fc2, Rest_tok ),
  Var_s = string::concat("$", Tok ),
  find_varaiabel_names( Rest2, [Var_s|HL], Names_list0 ).
find_varaiabel_names( _Txa0, HL, HL ):- !.

%---
replace_all_variabel_names2( Tx , [], Tx ) :- !.
replace_all_variabel_names2( Txa02 , [ Name | Names_list ], Txar ) :-
  string::tryFront( Name, 2, Fc, Rest_tok ),
  Fc2 = string::toUpperCase( Fc ),
  Tok_up = string::concat( Fc2, Rest_tok ), !,
  Txz = string::replaceAll( Txa02 , Name, Tok_up ) ,
 replace_all_variabel_names2( Txz , Names_list , Txar ) .
%---
replace_all_variabel_names( Txz , Names_list , Txar ):- replace_all_variabel_names2( Txz , Names_list , Txar ) , ! .
replace_all_variabel_names( Txz , _Names_list , Txz ):- !.


%---

% set_dia_size0():- set_dia_size().
current_example_get( W ) :- current_example( W ) , ! .
current_example_get( "Default" ) :- !.

set_dia_size( Wi ):-
 configuration::get_msg_height( He ),
   Tw = vpi::getTaskWindow(),
   RECT = vpi::winGetClientRect( Tw ) ,
   RECT = rct( X1 , Y1 , X2 , Y2 ),
   RECT2 = rct( X1 + 10 , Y1 + 55 , X2 - 10 , Y2 - He - 45   ),
   Wi:setClientRect( RECT2 ),!.


set_dia_size_rct( Wi , RECT ):-
 configuration::get_msg_height( He ),
%   Tw = vpi::getTaskWindow(),
%   RECT = vpi::winGetClientRect(Tw),
%   RECT = rct( X1,Y1,X2,Y2 ),
%   RECT2 = rct( X1 + 10 ,Y1 + 55 ,X2 - 10 ,Y2 - He - 45   ),
   Wi:setClientRect( RECT ),!.


predicates
onEditCut : window::menuItemListener.
clauses
onEditCut(_, _) :-   sciLexer_ctl:cut().

predicates
onEditPaste : window::menuItemListener.
clauses
onEditPaste(_, _) :-  sciLexer_ctl:paste().

predicates
onEditUndo : window::menuItemListener.
clauses
onEditUndo(_, _) :-   sciLexer_ctl:undo().



predicates
onEditCopy : window::menuItemListener.
clauses
onEditCopy(_, _) :-
        sciLexer_ctl:copy().


predicates
onEditSearch : window::menuItemListener.
clauses
onEditSearch(_, _) :-
       _ = sciLexer_ctl:displayFindAndReplace_modal().

predicates
onEditInsert : window::menuItemListener.
clauses
onEditInsert( Win , _ ) :-
  Start = sciLexer_ctl:selectionStart,  End = sciLexer_ctl:selectionEnd,   Leng = End - Start ,
%        S <> E,
%        S = wordStartPosition(E),
 %       E = wordEndPosition(S),
        Selected = sciLexer_ctl:selText,
% Ok dit werkt , nu dus de selectie kunnen parsen  en omschrijven indien nodig
% Via express_position
% evt hier weer submenu OF  conditioneel op hetgeen geselecteerd is
    functie_voor_selected_text( Win, Start, End, Leng , Selected ) ,
%    sciLexer_ctl:deleteRange( Start , Leng ) ,
%    safePie::parse_metta1_x( Selected , Cmp_clause ) ,
%    assist_syntax( Cmp_clause, Cmp_clause2 ) ,
%    safePie::cmp_clause_to_metta_x( Cmp_clause2 , Metta_str ) ,
%    sciLexer_ctl:insertText( Metta_str ) ,
        ! .

%    Tx2 = toString( Cmp_clause2 ) ,
%     vpiCommonDialogs::note( "result back to string in ", Pz ),
 % sciLexer_ctl:text
 % shadow_pfc\
 % pfc\pie\pie::sterm Term [out]
%        sciLexer_ctl:insertText( "let_starrrr" ) ,
 % Tx2 = string::concat( "\n ( ", Selected , " ) \n " ) ,
 %   vpiCommonDialogs::note( "selected is  ", Selected ),
%    safePie::try_string_to_stermx( Selected, Ster_z ),

onEditInsert(_, _) :- !.

       % displayFindAndReplace_modal().

 % sciLexer_ctl:displaydisplayFindAndReplace_modaless().
% id_edit_insert

%predicates
%    onContextMenu : window::contextMenuResponder.
%clauses
%    onContextMenu(_Source, _Input) = window::defaultContextMenuHandling :-
%        contextMenu(cursorGetPos()).

predicates
    contextMenu : (pnt Point).
clauses
    contextMenu(Point) :-
  %   vpiCommonDialogs::note( "pafefe" ),
        MNU = vpi::menuGetRes(resourceIdentifiers::idr_popup),
%        MNU == dynMenu(MLIST1),
 %       ItemList1 = [ Item1 || check_True_Item(Item1) ],
  %      ItemList2 = [ Item2 || check_False_Item(Item2) ],
   %     menuPopup_Switch(MLIST1, ItemList1, b_True, MLIST2),
    %    menuPopup_Switch(MLIST2, ItemList2, b_False, MLIST3),
     %   MNU2 = dynMenu(MLIST3),
      %  frameWindow:setFocus(),
%        vpi::menuPopUp(frameWindow:getVpiWindow(), MNU, Point, align_Left).
%        vpi::menuPopUp(getVpiWindow(), MNU, Point, align_Left).
        vpi::menuPopUp(This:getVpiWindow(), MNU, Point, align_Left).


clauses
display(Parent) = Dialog :-
        Dialog = new(Parent),
        Dialog:show().

clauses
new(Parent) :-
        dialog::new(Parent),
        generatedInitialize(),
%        setContextMenuResponder(onContextMenu),
        addMenuItemListener( resourceIdentifiers::id_edit_search, onEditSearch),
        addMenuItemListener( resourceIdentifiers::id_edit_insert, onEditInsert),
% 13-12-2025
        addMenuItemListener( resourceIdentifiers::id_edit_copy, onEditCopy),

        addMenuItemListener(resourceIdentifiers::id_edit_cut, onEditCut),
        addMenuItemListener(resourceIdentifiers::id_edit_paste, onEditPaste),
        addMenuItemListener(resourceIdentifiers::id_edit_undo, onEditUndo).




predicates
    onOkClick : button::clickResponder.
clauses

get_debug( Sta ) :- no_debug( Sta ) , !.
get_debug( false ) :- !.

onOkClick(_Source) = button::defaultAction :-
 % Txp = edit_ctl:getText() ,
   Txp = sciLexer_ctl:text,
 % pfc\pie\pie::
           % Pie = pfc\pie\pie::new(),
           %   Pie:try_string_to_sterm( Txp, _Ster ),
    safePie::try_string_to_stermx( Txp, Ster_z ),
    Pz = toString( Ster_z ),

     vpiCommonDialogs::note( "result back to string in ", Pz ),
   safePie::recons_newclausex( Ster_z  ),
      vpiCommonDialogs::note( " Term consulted  ", "" ),
    % try_string_to_sterm : ( string [in] , sterm Term [out] ) determ.
 ! .

onOkClick(_Source) = button::defaultAction.

predicates
onCancelClick : button::clickResponder.
clauses
onCancelClick(_Source) = button::defaultAction:-
 pie\editor::remove_position_custom( rct(0,0,0,0) ), !.

predicates
onPushButtonClick : button::clickResponder.
clauses

onPushButtonClick( _Source ) = button::defaultAction :-
   %  Txp = edit_ctl:getText() ,

   Txp = sciLexer_ctl:text,
  % applicationWindow::get(),


% 24-5-2025
%   safePie::start_parse_x( Txp , Metta_parsed ) ,
   safePie::start_parse_x( Txp , Result , Line, Line_pos ) ,
 %  vpiCommonDialogs::note( " result parse pos ", toString( Result ) ) ,
   % 1 = no errors
   if_there_is_an_error_goto_pos_and_fail( Result , Line, Line_pos ),
%   Result = 0,
% 1111   Zx = toString( Metta_parsed ) ,   clipboard::putString( Zx ),

%   safePie::inventarise_metta_sub( 0, 0, Metta_parsed ),
%   stdio::write( "end_result" , Zx2 , "\n" ),
% is done in pie
      %  safePie::transpile_metta_sub( 0, 0, Metta_parsed,  Metta_transpiled ),
% 222   Zx2 = toString( Metta_transpiled ) ,   clipboard::putString( Zx2 ),
%   vpiCommonDialogs::note( " Metta Term transpiled cb Saved pro", Zx2 ) ,
   safePie::memory_to_prolog_filex() ,
   programControl::sleep( 600 ) ,
% 19-5-2025
 %     configuration::get_msg_height( He ),
  %    Tw = vpi::getTaskWindow(),
   %   RECT = vpi::winGetClientRect( Tw ),
    %  RECT = rct( X1 , Y1 , X2 , Y2 ),
     % RECT2 = rct( X1 + 30 , Y2 - He , X2 - 30 , Y2 - 65 ),
%   pie\editor::set_position_custom( RECT2 ),
   pie\editor::open_file( "results\\metta_to_prolog_transpiled.pro" , true ) ,
%    This:destroy(),
   ! .

onPushButtonClick( _Source ) = button::defaultAction :-  !.

predicates
onEditModified : editControl::modifiedListener.
clauses
onEditModified(_Source).

predicates
onPushButton1Click : button::clickResponder.
clauses
onPushButton1Click(_Source) = button::defaultAction:-
  % Txp = edit_ctl:getText() ,
   Txp = sciLexer_ctl:text,

   safePie::start_parse_pro( Txp , Pro_parsed ) ,
   Zx = toString( Pro_parsed ) ,
    clipboard::putString( Zx ),
   vpiCommonDialogs::note( " Prolog consulted cb ", Zx ) ,   ! .


onPushButton1Click(_Source) = button::defaultAction.

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data):-
     % assert( current_lexer( sciLexer_ctl ) ),
      pie\editor::set_position_custom( rct(0,0,0,0) ),
%  (: simple-deduction-strength-formula (-> Number Number Number Number Number Number))
 %      Txa = "(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",
% THIS BEST TEST ---
%      Txa = "\n\n (equal (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",

      Txa = "\n\n (= (func_calc $As $Bs) \n (/ $As $Bs)) ",
      %   Txa =  "\n\n (= (get_cell_state $row $column)    (match &self (sudoku_puzzle_state $row $column $quad $state)    $state    )   )",
%      Txa =  "\n\n (= (get_cell_state $row $column)  (erer $er )  (nesmathc $jh (match &self (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",
%       Txa =  "\n\n (= (get_cell_state $row $column)   (nesmathc $jh (erer $er ) (match (lapred $weed $ui ) &self (erer $er ) \n (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",

       sciLexer_ctl:text := Txa ,
       sciLexer_ctl:lexerDefault_visualProlog(),
%         sciLexer_ctl:styleSetBack( sciLexer::style_default, 0xFFFFFF ),
% DIT WERKT 1-11-2025 we willen nu hetzefde in de message form doen
%          sciLexer_ctl:styleSetBack( 32, 0xFFFFFF ),
           sciLexer_ctl:styleSetBack( sciLexer_native::style_lineNumber , 0x000000 ),
% style_lineNumber = 33

% Tahoma Verdana
   Fo_ed = vpi::fontCreateByName("Tahoma", 14),
sciLexer_ctl:setFont( Fo_ed ) ,
       %       editorControl_ctl:whenCreated(
        %    {  :-
        %        editorControl_ctl:text := Text,
        %        editorControl_ctl:lexerDefault_visualProlog(),
        %        editorControl_ctl:gotoPos(Pos),
        %        editorControl_ctl:readOnly := ReadOnly,
        %        editorControl_ctl:useTabs := false,
        %        editorControl_ctl:setSavepoint(),
        %        editorControl_ctl:emptyUndoBuffer()
        %    }),

         sciLexerStatusBar_ctl:attach(sciLexer_ctl),

    % addMenuItemListener(id_edit_search, onEditSearch),

         set_dia_size( _Source ),
%   Tw = vpi::getTaskWindow(),
 %  RECT = vpi::winGetClientRect(Tw),
 %  RECT = rct( X1,Y1,X2,Y2 ),
 %  RECT2 = rct( X1 + 10 ,Y1 + 60 ,X2 - 10 ,Y2 - 400  ),
 %  setClientRect( RECT2 ),

  % RECT = Tw:getOuterRect(),
%   Wi = _Source:getVpiWindow(),
 %   getwin
  %    TW = Parent:getVpiWindow(),
   %     editorControl_ctl:dockStyle := control::dockFill,
    %    configuration::get_lru_pos(TW, Filename, Rct, Pos),
        % setOuterRect(RECT),



     %   Wi:setClientRect(RECT),
         %setClientRect : (vpiDomains::rct ClientRectangle).
% vpi::
     Is_che = boolean::fromBooleanInt(  b_true ),
     checkBox_ctl:setChecked( Is_che ),
     Stax = checkBox_ctl:getChecked(),
%      pfc\pie\pie::set_debug( Sta ),
      retractall( no_debug( _ ) ),  assert( no_debug( Stax ) ),

      ! .



predicates
onPushButton2Click : button::clickResponder.
clauses
onPushButton2Click(_Source) = button::defaultAction:-
 %  (: simple-deduction-strength-formula (-> Number Number Number Number Number Number))
 %      Txa = "(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",
% THIS BEST TEST ---
%      Txa = "\n\n (equal (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",
      Txa = "\n\n (= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (< 0.99 $Bs)        $Cs    \n    (+ (* $ABs $BCs) (/ (* \n (- 1 $ABs) (- $Cs (* $Bs $BCs))) (- 1 $Bs))))  \n   0)) ",

%      Txa = "\n\n (equal (func_calc $As $Bs) \n (division $As $Bs)) ",
      %   Txa =  "\n\n (= (get_cell_state $row $column)    (match &self (sudoku_puzzle_state $row $column $quad $state)    $state    )   )",
%      Txa =  "\n\n (= (get_cell_state $row $column)  (erer $er )  (nesmathc $jh (match &self (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",
%       Txa =  "\n\n (= (get_cell_state $row $column)   (nesmathc $jh (erer $er ) (match (lapred $weed $ui ) &self (erer $er ) \n (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",
       sciLexer_ctl:text := Txa ,
       sciLexer_ctl:lexerDefault_visualProlog(),
%       application
 retractall( current_example( _ ) ),  assert( current_example( "Example-1" ) ),
               Tw = applicationWindow::get(), Tw:setText( "Metta - Example-1" ).


%        file::existExactFile(FileName),
 %           Text = file::readString(FileName, _)


predicates
onPushButton3Click : button::clickResponder.
clauses
onPushButton3Click(_Source) = button::defaultAction:-
%  (: simple-deduction-strength-formula (-> Number Number Number Number Number Number))
 %      Txa = "(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",
% THIS BEST TEST ---
%      Txa = "\n\n (equal (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",
%      Txa = "\n\n (equal (func_calc $As $Bs) \n (division $As $Bs)) ",
      %   Txa =  "\n\n (= (get_cell_state $row $column)    (match &self (sudoku_puzzle_state $row $column $quad $state)    $state    )   )",
%      Txa =  "\n\n (= (get_cell_state $row $column)  (erer $er )  (nesmathc $jh (match &self (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",

% bad trivial example
%       Txa =  "\n\n (= (get_cell_state $row $column)   (nesmathc $jh (erer $er ) (match (lapred $weed $ui ) &self (erer $er ) \n (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",

% Txa =  "\n\n (= (exist_temp_num $num) \n   (if (==  \n   (collapse (match &self (tempnum $num) True))          ())       False    True     )    \n  ) ",
     Txa = "\n\n (= (add_tmp_candidates $expr)  \n (if (== $expr ()) ()  \n    (let (($head (car-atom $expr)) \n    ($tail (cdr-atom $expr)) \n    ($head_new (add_tmp_candidate $head))             ($tail_new (add_tmp_candidates $tail))   \n  )  \n  (cons-atom $head_new $tail_new)   )   ))  ",
       sciLexer_ctl:text := Txa ,
       sciLexer_ctl:lexerDefault_visualProlog(),
        retractall( current_example( _ ) ),  assert( current_example( "Example-2" ) ),
                      Tw = applicationWindow::get(), Tw:setText( "Metta - Example-2" ),
       !.


predicates
    onPushButton4Click : button::clickResponder.
clauses
onPushButton4Click(_Source) = button::defaultAction:-
%  (: simple-deduction-strength-formula (-> Number Number Number Number Number Number))
 %      Txa = "(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",
% THIS BEST TEST ---
%      Txa = "\n\n (equal (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs) \n (if  \n   (and   \n     (conditional_probability_consistency $As $Bs $ABs)  \n      (conditional_probability_consistency $Bs $Cs $BCs))   \n  (if (smaller_than 0.99 $Bs)        $Cs    \n    (plus (multiplication $ABs $BCs) (division (multiplication \n (minus 1 $ABs) (minus $Cs (multiplication $Bs $BCs))) (minus 1 $Bs))))  \n   0)) ",
%  Txa = "   " ,
      Txa = "\n\n (= (func_calc $As $Bs) \n (/ $As $Bs)) ",
      %   Txa =  "\n\n (= (get_cell_state $row $column)    (match &self (sudoku_puzzle_state $row $column $quad $state)    $state    )   )",
%      Txa =  "\n\n (= (get_cell_state $row $column)  (erer $er )  (nesmathc $jh (match &self (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",
%       Txa =  "\n\n (= (get_cell_state $row $column)   (nesmathc $jh (erer $er ) (match (lapred $weed $ui ) &self (erer $er ) \n (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",
  sciLexer_ctl:text := Txa ,

%     sciLexer_ctl:getSelectionN

       sciLexer_ctl:lexerDefault_visualProlog(),
        retractall( current_example( _ ) ),  assert( current_example( "Example-3" ) ),
                      Tw = applicationWindow::get(), Tw:setText( "Metta - Example-3" ),
       !.


% (equal (init_a_stack )    (match &self (sudoku_puzzle_state $r $c $quad $state)
%         )    )

predicates
    onPushButton5Click : button::clickResponder.
clauses
onPushButton5Click(_Source) = button::defaultAction:-
   Fpro = mainExe::getFileName(), Pa0 = fileName::getPath(Fpro), Zz = string::concat( Pa0, "metta_programs\\" ),

   % Dir = "metta_programs\\" ,
   Fn = vpiCommonDialogs::getFileName("*.metta", ["Metta files(*.metta)", "*.metta", "PIE Files(*.pie)", "*.pie", "All files(*.*)", "*, *"],
              "Open file ...", [dlgfn_filemustexist], Zz , _),

          file::existExactFile( Fn ),
            Txa00 = file::readString( Fn ,  _ ) , ! ,

%  find_varaiabel_names( Txa0, [], Names_list0 ) , Names_list = list::removeDuplicates( Names_list0 ),

%             vpiCommonDialogs::note( "vars extracted", toString( Names_list ) ) ,
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
%            replace_all_variabel_names( Txa02 , Names_list, Txa ) ,
       sciLexer_ctl:text := Txa13 ,
       sciLexer_ctl:lexerDefault_visualProlog(),

                      Tw = applicationWindow::get(), Tw:setText( Fn ),
       !.
onPushButton5Click(_Source) = button::defaultAction:- ! .


predicates
onPushButton6Click : button::clickResponder.
clauses
onPushButton6Click(_Source) = button::defaultAction:-
 %
%         )    )
  Txa = string::concatList( [
%      "\n\n (= (init_a_stack )  \n   (match &self (sudoku_puzzle_state $r $c $quad $state)   \n    )    )",
          "\n\n",

%          "(sudoku_number 1 )\n",
%          "(sudoku_number 2 )\n",
%          "(sudoku_number 3 )\n",
%          "(sudoku_number 4 )\n",
%          "(sudoku_number 5 )\n",
%          "(sudoku_number 6 )\n",
%          "(sudoku_number 7 )\n",
%          "(sudoku_number 8 )\n",
%         "(sudoku_number 9 )\n\n",

         "(sudoku_puzzle_state 1 7 3 0  )\n",
         "(sudoku_puzzle_state 1 2 3 0  )\n",
         "(sudoku_puzzle_state 1 9 3 0  )\n",
         "(sudoku_puzzle_state 2 1 1 6  )\n",
         "(sudoku_puzzle_state 2 2 1 0  )\n",
         "(sudoku_puzzle_state 2 3 1 0  )\n",
         "(sudoku_puzzle_state 2 4 2 1  )\n\n",


 %         "(= (init_get_candidates )   ",
%  "( match &self   (sudoku_number $num   )      (cell_candidatex $num )  )",
%  "(= (init_get_candidates )   ( match &self   (sudoku_number $num $a $b  )      (cell_candidatex $b $num )  ))",
% test with: init_get_candidates 2
   "(= (init_get_candidates $col )   ( match_all &self   (sudoku_puzzle_state $row $col $quad $state  )      (cell_candidatex $quad $state )  ))",
  % ")",

        "\n  " ] ),


      %   Txa =  "\n\n (= (get_cell_state $row $column)    (match &self (sudoku_puzzle_state $row $column $quad $state)    $state    )   )",
%      Txa =  "\n\n (= (get_cell_state $row $column)  (erer $er )  (nesmathc $jh (match &self (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",
%       Txa =  "\n\n (= (get_cell_state $row $column)   (nesmathc $jh (erer $er ) (match (lapred $weed $ui ) &self (erer $er ) \n (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",



  sciLexer_ctl:text := Txa ,
       sciLexer_ctl:lexerDefault_visualProlog(),
        retractall( current_example( _ ) ),  assert( current_example( "Example-4" ) ),
                      Tw = applicationWindow::get(), Tw:setText( "Metta - Example-4" ),
       !.






% Button7 to implement
% (= (exist_temp_num $num)
 %       (let $m (collapse (match self (tempnum $num) True))
 %       (if (check_equal $m ())
 %           False
 %           True
  %      )
  %  )
  %           )


predicates
    onPushButton7Click : button::clickResponder.
clauses
    onPushButton7Click(_Source) = button::defaultAction:-

      %   Txa =  "\n\n (= (get_cell_state $row $column)    (match &self (sudoku_puzzle_state $row $column $quad $state)    $state    )   )",
%      Txa =  "\n\n (= (get_cell_state $row $column)  (erer $er )  (nesmathc $jh (match &self (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",
%       Txa =  "\n\n (= (get_cell_state $row $column)   (nesmathc $jh (erer $er ) (match (lapred $weed $ui ) &self (erer $er ) \n (extra $fd (sudoku_puzzle_state $row $column $quad $state) )   $state    )  )  )",

  Txa = string::concatList( [
      "\n\n   (add-atom &self",
          "\n  (board-state ((1 8 g r) (2 8 g n) (3 8 g b) (4 8 g q) (5 8 g k) (6 8 g b) (7 8 g n) (8 8 g r)",
                        "\n  (1 7 g p) (2 7 g p) (3 7 g p) (4 7 g p) (5 7 g p) (6 7 g p) (7 7 g p) (8 7 g p)",
                        "\n  (1 6) (2 6) (3 6) (4 6) (5 6) (6 6) (7 6) (8 6)",
                        "\n  (1 5) (2 5) (3 5) (4 5) (5 5) (6 5) (7 5) (8 5)",
                        "\n  (1 4) (2 4) (3 4) (4 4) (5 4) (6 4) (7 4) (8 4)",
                        "\n  (1 3) (2 3) (3 3) (4 3) (5 3) (6 3) (7 3) (8 3)",
                        "\n  (1 2 s p) (2 2 s p) (3 2 s p) (4 2 s p) (5 2 s p) (6 2 s p) (7 2 s p) (8 2 s p)",
                        "\n  (1 1 s r) (2 1 s n) (3 1 s b) (4 1 s q) (5 1 s k) (6 1 s b) (7 1 s n) (8 1 s r)))",
        "\n  )" ] ),


  sciLexer_ctl:text := Txa ,
       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-5" ) ),
                      Tw = applicationWindow::get(), Tw:setText( "Metta - Example-5" ),
       !.


predicates
    onPushButton8Click : button::clickResponder.
clauses
    onPushButton8Click(_Source) = button::defaultAction:-
% "\n\n  (: contains_symbol (-> Expression Atom Bool))",

 Txa = string::concatList( [
% added to make this predicate working , otherwise it never succeeds with empty list
% contains_symbol( [] , _ , "False" ):- !.

% "\n\n  (= (contains_symbol $list $sym)",
% "\n  (if (== $list ()) False True ) )",
% this one should be compiled to:  cmp(":-",[cmp("contains_symbol",[nill,var("_"),str("False")]),atom("!")])
% cmp("equal",[cmp("sub",[cmp("contains_symbol",[atom("empty_atom"),var("$sym")])]),atom("False")])

 "\n\n  (= (contains_symbol () $sym )  False )",


 "\n\n  (= (contains_symbol $list $sym)",
    "\n  (if (== $list ())",
   "\n  False",
   "\n  (if (== (car-atom $list) $sym)",
   "\n  True",
   "\n  (contains_symbol (cdr-atom $list) $sym))))" ] ),

% manually added this, then it sort of works
% count_list( [] , 0 ):- !.
%"\n\n  (= (count_list $x) ",
%"\n     (if (== $x ()) ",
%"\n       0 ",
%"\n       (+ (count_list (cdr-atom $x)) 1))) " ] ),

  sciLexer_ctl:text := Txa ,
       sciLexer_ctl:lexerDefault_visualProlog(),
retractall( current_example( _ ) ),  assert( current_example( "Example-6" ) ),
                      Tw = applicationWindow::get(), Tw:setText( "Metta - Example-6" ),
       !.


predicates
    onPushButton9Click : button::clickResponder.
clauses
onPushButton9Click(_Source) = button::defaultAction:-
% Txa = string::concatList( [
% "\n\n (= (loop_sudoku_until_complete (coninu $yesno ) ) ",
% "\n  (collapse ",    "\n (let  (", "\n ($qq (get_ycount) )",
% "\n  ($e1 (remove_violations) )", "\n ($tr (add-atom &self (violated 0 0 ) ) )",
% "\n  ($e3 (apply_action_from_stack (collapse  (get_all_actions)  ) ) )",
% "\n  ($e4 (score_cells  (collapse (all_empty_cells)  ) )  )",
% "\n  ($e5 (if_exist_violation_backtrack_stack ) )", "\n ($e6 (puzzle_is_complete) )   ",
% "\n  ($e7 (+ $yesno 1) )   ",   "\n )   ", "\n ",
%     "\n  (if (== $e6 True) () (loop_sudoku_until_complete (coninu $e7 )) )  ",
%    "\n ) ",    " )  ", " )" ] ),

 Txa = string::concatList( [
% "\n\n (= (loop_sudoku_until_complete  $yesno  ) ",
% "\n  (let ",
% "\n  ($qq (get_ycount $yesno) )",
% "\n  ($zz (get_xcount) )",
%    "\n   ($pp (get_zcount $zz) ) " ,
%  "\n  ) ",
%    "\n  )" ] ),
%   this head should be  parsed to:   cmp(":-",[cmp("add_xnum",
%   [cmp("cell_xstate",[var("R"),var("C"),var("Sta")])]),
%   cmp("add_atom",[str("self"),var("Sta")])])
% and currently it is parsed initially as :
% ( diffrent names but the idea / structure is the same )
%   cmp("loop_sudoku_until_complete",[cmp("sub",[cmp("coninu",[var("$yesno")])])])

"\n\n (= (loop_sudoku_until_complete (coninu $yesno ) )" ,
"\n  (collapse " ,
"\n     (let*  (" ,
% "\n  ($qq (get_ycount) )" ,
% "\n  ($e1 (remove_violations) )" ,
% "\n  ($tr (add-atom &self (violated 0 0 ) ) )" ,
% "\n  ($e3 (apply_action_from_stack (collapse  (get_all_actions)  ) ) )" ,
% "\n  ($e4 (score_cells  (collapse (all_empty_cells)  ) )  )" ,
% "\n  ($e5 (if_exist_violation_backtrack_stack ) )" ,
"\n  ($e6 (puzzle_is_complete) )   " ,
"\n  ($e7 (+ $yesno 1) )   " ,
"\n    )   " ,

"\n      (if (== $e6 True) () (loop_sudoku_until_complete (coninu $e7 )) )  " ,
"\n     ) " ,
"\n     )  " ,
"\n  )"  ] ),


 sciLexer_ctl:text := Txa ,
       sciLexer_ctl:lexerDefault_visualProlog(),
        retractall( current_example( _ ) ),  assert( current_example( "Example-7" ) ),
                      Tw = applicationWindow::get(), Tw:setText( "Metta - Example-7" ),
       !.



predicates
onPushButton10Click : button::clickResponder.
clauses
onPushButton10Click(_Source) = button::defaultAction:-
  Txa = string::concatList( [
 "\n\n (sudoku_puzzle_state 1 1 1 5  ) \n\n" ] ),
 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
 retractall( current_example( _ ) ),  assert( current_example( "Example-8" ) ),
                   Tw = applicationWindow::get(), Tw:setText( "Metta - Example-8" ),
 !.

predicates
onPushButton11Click : button::clickResponder.
clauses
onPushButton11Click(_Source) = button::defaultAction:-
 Txa = string::concatList( [
 "\n\n  !(init_ycount)",
 "\n  !(init_action_stack)",
 "\n  !(loop_sudoku_until_complete (coninu 1 ) )"  ] ),
 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
 retractall( current_example( _ ) ),  assert( current_example( "Example-9" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-9" ),

 !.

predicates
onPushButton12Click : button::clickResponder.
clauses
onPushButton12Click(_Source) = button::defaultAction:-
% 5-6-2025  this one should possibly be written as below, because it seems to transpile with the default well

% (= (exist_temp_num $num)
%    (let  ($m (collapse (match &self (tempnum $num) True))  )
%      (if (== $m ())
%   False
%    True
%    )
%    )
%    )
          Txa = " \n\n (= (exist_temp_num $num) \n    (let $m (collapse (match &self (tempnum $num) True))  \n      (if (== $m ())    \n   False \n   True \n    ) \n    ) \n    )",

 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-10" ) ),
                Tw = applicationWindow::get(), Tw:setText( "Metta - Example-10" ),
 ! .


predicates
onPushButton13Click : button::clickResponder.
clauses
onPushButton13Click(_Source) = button::defaultAction:-

 Txa =  "\n\n (= (exist_temp_num $num) \n   (if (==  \n   (collapse (match &self (tempnum $num) True))          ())       False    True     )    \n  ) ",


 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
   retractall( current_example( _ ) ),  assert( current_example( "Example-11" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-11" ),
 ! .


predicates
onCheckBoxStateChanged : checkButton::stateChangedListener.
clauses
onCheckBoxStateChanged(_Source, _OldState, _NewState):-
     Sta = checkBox_ctl:getChecked(),
%      pfc\pie\pie::set_debug( Sta ),
      retractall( no_debug( _ ) ),  assert( no_debug( Sta ) ),
      % pfc\pie\pie::set_debug( true ),
%     getState(),
     stdio::write( toString( Sta ) ).

predicates
    onPushButton14Click : button::clickResponder.
clauses
    onPushButton14Click(_Source) = button::defaultAction:-
     Txa = string::concatList( [
         "\n       (= (exist_temp_num $num) ",
    "\n  (($m (collapse (match &self (tempnum $num) True))      ) ",
      "\n  ($xm (if (== $m ())       False    True     ) )",
 "\n       )",
     "\n  )" ] ),

 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-12" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-12" ),
 ! .


predicates
    onPushButton15Click : button::clickResponder.
clauses
  %     (= (backtrack_to_level_where_there_were_alternatives)
   %     (collapse (let*  (
   %      ($levelpp  (get_ycount)   )
   %      ($e4  (exist_alternatives_on_level $levelpp)  )
   %      ($ex5 (set_ycount $e4 ) )
   %      )
   %      True
   %      )  )
   %        )


onPushButton15Click(_Source) = button::defaultAction:-      Txa = string::concatList( [
         "\n       (= (backtrack_to_level_where_there_were_alternatives) ",
         "\n        (collapse (let*  (",
%         "\n         ($ew   (remove_violations) )",
%         "\n         ($tr   (add-atom &self (violated 0 0 ) ) )",
%         "\n         ($eq34 (set_current_action_as_false ) )",
         "\n         ($levelpp  (get_ycount)   )",
         "\n         ($e4  (exist_alternatives_on_level $levelpp)  )",
         "\n         ($ex5 (set_ycount $e4 ) )",
 %        "\n         ($e6  (backtrack_to_level $e4 ) )",
 %        "\n         ($e31 (re_add_to_action_stack $e4 ) )",
 %        "\n         ($e9  (remove_trace $e4 ) )",
         "\n         ) ",
         "\n         True",
         "\n         )  )",
         "\n           )" ] ),

 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-13" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-13" ),
 ! .



predicates
onPushButton16Click : button::clickResponder.
clauses
onPushButton16Click(_Source) = button::defaultAction:-
     current_example( Name ) ,
%    safePie::try_string_to_stermx( Txp, Ster_z ),
      safePie::try_set_consult_modelx( Name ),
      Qw = trans_profile::display( This ), !.

onPushButton16Click(_Source) = button::defaultAction:-
      Qw = trans_profile::display( This ), !.


onPushButton16Click(_Source) = button::defaultAction:- !.

predicates
    onHelpClick : button::clickResponder.
clauses
    onHelpClick(_Source) = button::defaultAction:-
file::existExactFile("EXPLANATION.txt"),
 Tx = file::readString( "EXPLANATION.txt", _ ),
  vpiCommonDialogs::note("Explanation", Tx ),!.

        onHelpClick(_Source) = button::defaultAction.

predicates
onPushButton17Click : button::clickResponder.
clauses
onPushButton17Click(_Source) = button::defaultAction:-
  Txa = string::concatList( [
% added to make this predicate working , otherwise it never succeeds with empty list
% contains_symbol( [] , _ , "False" ):- !.

% "\n\n  (= (contains_symbol $list $sym)",
% "\n  (if (== $list ()) False True ) )",

% "\n  (= (contains_symbol $list $sym)",
%    "\n  (if (== $list ())",
%   "\n  False",
%   "\n  (if (== (car-atom $list) $sym)",
%   "\n  True",
%   "\n  (contains_symbol (cdr-atom $list) $sym))))" ] ),

% manually added this, then it sort of works
% count_list( [] , 0 ):- !.
"\n\n  (= (count_list () )  0 )",

"\n\n  (= (count_list $x) ",
"\n     (if (== $x ()) ",
"\n       0 ",
"\n       (+ (count_list (cdr-atom $x)) 1))) " ] ),

sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-14" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-14" ), !.


predicates
onPushButton18Click : button::clickResponder.
clauses
onPushButton18Click(_Source) = button::defaultAction:-
  Txa = string::concatList( [
"\n\n  (= (get_tmp_candidates_not_false $r $c $levelx )   (match &self   (cell_candidate $r $c $num  )",
"\n        (if (== (exist_as_false_candidate $levelx $r $c $num) True )    empty $num    )",
"\n         )  )" ] ),

sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-15" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-15" ), !.





predicates
onPushButton19Click : button::clickResponder.
clauses
onPushButton19Click(_Source) = button::defaultAction:-
   Txa = string::concatList( [
"\n\n  (= (get_tmp_candidates_not_false $r $c $levelx )   (match &self   ",
"\n (komma (cell_candidate $r $c $num)  (not_exist_as_false_candidate $levelx $r $c $num) ) $num )  )" ] ) ,

sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-16" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-16" ), !.


predicates
onPushButton110Click : button::clickResponder.
clauses
onPushButton110Click(_Source) = button::defaultAction:-
   Txa = string::concatList( [
"\n\n  (= (get_tmp_candidates_not_false $r $c $levelx )   (match &self   ",
"\n (komma (cell_candidate $r $c $num)  (not_exist_as_false_candidate $levelx $r $c $num) ) ",
  "\n (if (== (exist_as_false_candidate $levelx $r $c $num) True )    empty $num    )",
 "\n )  )" ] ) ,

sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-17" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-17" ), !.

predicates
onPushButton111Click : button::clickResponder.
clauses
onPushButton111Click(_Source) = button::defaultAction:-   Txa = string::concatList( [
"\n\n  (= (get_tmp_candidates_not_false $r $c $levelx )   (match &self   ",
"\n (komma (cell_candidate $r $c $num)  (not_exist_as_false_candidate $levelx $r $c $num) ) ",
  "\n (add_atom (candidx $r $c)    )",
 "\n )  )" ] ) ,

sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-18" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-18" ), !.



predicates
onPushButton112Click : button::clickResponder.
clauses
onPushButton112Click(_Source) = button::defaultAction:-
    Txa = string::concatList( [
"\n\n % It goes wrong if you change the last ycount in xcount because it then takes the wrong variabels of the first xcount! todo solve this. is already solved  ",

"\n\n (= (increment_count )   ( ",
 "\n (let*  (",
   "\n    ($was (get_count ) )",
   "\n    ($we  (remove-atom &self (xcount $was)) )",
  "\n     ($cx  (+ $was 1) )",
   "\n    ($re (add-atom &self (xcount $cx ))   )",

    "\n )",
     "\n$cx ",
  "\n)  ) )" ] ) ,
 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-19" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-19" ), !.


predicates
onPushButton113Click : button::clickResponder.
clauses
onPushButton113Click(_Source) = button::defaultAction:-      Txa = string::concatList( [
 "\n\n(= (score_cell $va)  (plus $va 1) ) ",

"\n\n(= (score_cells $expr)",
 "\n (if (== $expr ()) ()",
 "\n       (let* (($head (car-atom $expr))",
   "\n           ($tail (cdr-atom $expr))",
      "\n        ($head_new (score_cell $head))",
         "\n     ($tail_new (score_cells $tail))",
             "\n)",
         "\n(cons-atom $head_new $tail_new)",
       "\n)",
   "\n)",
 "\n)"    ] ) ,

 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-20" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-20" ), !.






predicates
onPushButton114Click : button::clickResponder.
clauses
onPushButton114Click(_Source) = button::defaultAction:-
 Txa = string::concatList( [
  "\n(= (get_column_stateyz_add_nums $rowx $colx)",
 "\n(match_all &self  (komma ( sudoku_puzzle_state $row $colx $quad $state  )  (is_not_zero $state))   ",
"\n(add_atom &self (tempnum $state ) )",
"\n     )",
"\n)"

%"\n\n(= (get_column_stateyz_add_nums $rowx $colx)",
%    "\n (match_all &self   ( sudoku_puzzle_state $row $colx $quad $state  )     (",
%      "\n(if (> $state 0)",
%       "\n(add_atom &self (tempnum $state ) )",
%       "\n(empty)",
%    "\n)  )     )",
%  "\n)"
 ] ) ,

% must be this
 %(= (get_column_stateyz_add_nums $rowx $colx)
 %(match_all &self  (komma ( sudoku_puzzle_state $row $colx $quad $state  )  (is_not_zero $state))   (
%(add_atom &self (tempnum $state ) )
%)     )
%)
% test this
% (= (remove_a_stack)   (match_all &self   ( act_rc $r $c  )   (remove_atom &self (act_rc $r $c )  )    )  )
% (= (remove_a_stack)   (match_all &self   ( act_rc $r $c  )   (remove_atom &self (act_rc $r $c )  )    )  )
 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-21" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-21" ), !.

predicates
    onPushButton115Click : button::clickResponder.
clauses
    onPushButton115Click(_Source) = button::defaultAction:-      Txa = string::concatList( [
 "\n\n(= (remove_a_stack)   (match_all &self   ( act_rc $r $c  )   (remove_atom &self (act_rc $r $c )  )    )  )"

   ] ) ,

 sciLexer_ctl:text := Txa ,       sciLexer_ctl:lexerDefault_visualProlog(),
  retractall( current_example( _ ) ),  assert( current_example( "Example-22" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-22" ), !.


predicates
onPushButton116Click : button::clickResponder.
clauses
onPushButton116Click(_Source) = button::defaultAction:-
   Txa = string::concatList( [
 "\n(= (execute_sudoku) ",
    "\n    (let*  (",
       "\n  ($row_sta  (get_row_state 3)   )",
      "\n   (println $row_sta)",
     "\n    ($sta2  (filter_elems $row_sta)  )",
      "\n   (println $sta2)",

      "\n   ) ",
"\n         True",
         "\n) ",

           "\n)"

   ] ) ,

 sciLexer_ctl:text := Txa ,
  sciLexer_ctl:lexerDefault_visualProlog(),
%   sciLexer_ctl:lexerDefault_Metta,
  % sciLexer_ctl:lexerDefault_html(),
  %sciLexer_ctl:lexerDefault_cpp(),
  %sciLexer_ctl:lexerDefault_

  retractall( current_example( _ ) ),  assert( current_example( "Example-23" ) ),
                 Tw = applicationWindow::get(), Tw:setText( "Metta - Example-23" ), !.



predicates
onPushButton117Click : button::clickResponder.
clauses
onPushButton117Click(_Source) = button::defaultAction:-
          _Fo_ed = vpi::fontCreateByName("Courier", 12),
          Oldfo = sciLexer_ctl:getFont(),
%          getFont( Fo_ed ),
  NewFo = vpiCommonDialogs::getFont( Oldfo , _Newf , _Size ),
    sciLexer_ctl:setFont( Newfo ) , !.
onPushButton117Click(_Source) = button::defaultAction :- !.

predicates
    onCheckBox1StateChanged : checkButton::stateChangedListener.
clauses
onCheckBox1StateChanged(_Source, _OldState, _NewState):-
 Sta = checkBox1_ctl:getChecked(),
  Sta = true ,
  stdio::write( toString( Sta ) ) , !,
 pushButton2_ctl:setVisible( false ),
 pushButton3_ctl:setVisible( false ),
 pushButton4_ctl:setVisible( false ),
 pushButton5_ctl:setVisible( false ),
 pushButton6_ctl:setVisible( false ),
 pushButton7_ctl:setVisible( false ),
 pushButton8_ctl:setVisible( false ),
 pushButton9_ctl:setVisible( false ),
 pushButton10_ctl:setVisible( false ),
 pushButton11_ctl:setVisible( false ),
 pushButton12_ctl:setVisible( false ),
 pushButton13_ctl:setVisible( false ),
 pushButton14_ctl:setVisible( false ),

  pushButton15_ctl:setVisible( false ),
  pushButton17_ctl:setVisible( false ),
  pushButton18_ctl:setVisible( false ),
  pushButton19_ctl:setVisible( false ),
  pushButton110_ctl:setVisible( false ),
  pushButton111_ctl:setVisible( false ),
  pushButton112_ctl:setVisible( false ),
  pushButton113_ctl:setVisible( false ),
  pushButton114_ctl:setVisible( false ),
  pushButton115_ctl:setVisible( false ),
  pushButton116_ctl:setVisible( false ),

  RCT = sciLexer_ctl:getClientRect(),
  RCT = rct( X , Y , X2 , Y2 ) ,
  Y3 = Y2 + 80,
  RCT3 = rct( X , Y , X2 , Y3 ) ,
   sciLexer_ctl:setClientRect( RCT3 )
 .

onCheckBox1StateChanged(_Source, _OldState, _NewState):-
 Sta = checkBox1_ctl:getChecked(),
  stdio::write( toString( Sta ) ) ,
  Sta = false ,
   !,
 pushButton2_ctl:setVisible( true ),
 pushButton3_ctl:setVisible( true ),
 pushButton4_ctl:setVisible( true ),
 pushButton5_ctl:setVisible( true ),
 pushButton6_ctl:setVisible( true ),
 pushButton7_ctl:setVisible( true ),
 pushButton8_ctl:setVisible( true ),
 pushButton9_ctl:setVisible( true ),
 pushButton10_ctl:setVisible( true ),
 pushButton11_ctl:setVisible( true),
 pushButton12_ctl:setVisible( true ),
 pushButton13_ctl:setVisible( true ),
 pushButton14_ctl:setVisible( true ),

  pushButton15_ctl:setVisible( true ),
  pushButton17_ctl:setVisible( true ),
  pushButton18_ctl:setVisible( true ),
  pushButton19_ctl:setVisible( true ),
  pushButton110_ctl:setVisible( true ),
  pushButton111_ctl:setVisible( true ),
  pushButton112_ctl:setVisible( true ),
  pushButton113_ctl:setVisible( true ),
  pushButton114_ctl:setVisible( true ),
  pushButton115_ctl:setVisible( true ),
  pushButton116_ctl:setVisible( true ),

  RCT = sciLexer_ctl:getClientRect(),
  RCT = rct( X , Y , X2 , Y2 ) ,
  Y3 = Y2 - 80,
  RCT3 = rct( X , Y , X2 , Y3 ) ,
   sciLexer_ctl:setClientRect( RCT3 )
 .


onCheckBox1StateChanged(_Source, _OldState, _NewState).

predicates
onSciLexerContextMenu : window::contextMenuResponder.
clauses

onSciLexerContextMenu(_Source, _Input) = window::defaultContextMenuHandling:-
       contextMenu(cursorGetPos()).
%    vpiCommonDialogs::note("qasxzcdde").

predicates
onSciLexerMenuItem : window::menuItemListener.
clauses
onSciLexerMenuItem(_Source, _MenuTag).

predicates
onSciLexerChar : window::charResponder.
clauses
onSciLexerChar(_Source, Char, ShiftControlAlt) = window::defaultCharHandling:-
 % Sx = string::format( " pa  % % ", Char, ShiftControlAlt ), vpiCommonDialogs::note( Sx ),
 % this is : Ctrl F
 Char = 6, ShiftControlAlt = 2, !,
 _ = sciLexer_ctl:displayFindAndReplace_modal().

onSciLexerChar(_Source, Char, ShiftControlAlt) = window::defaultCharHandling:-
 Char = 3, ShiftControlAlt = 2, !,
 sciLexer_ctl:copy().

onSciLexerChar(_Source, Char, ShiftControlAlt) = window::defaultCharHandling:-
 Char = 24, ShiftControlAlt = 2, !,
 sciLexer_ctl:cut().

% begrijp het ook niet maar deze moet niet er in want anders doet hij dubbel paste
%onSciLexerChar(_Source, Char, ShiftControlAlt) = window::defaultCharHandling:-
% Char = 22, ShiftControlAlt = 2, !,
% sciLexer_ctl:paste().


onSciLexerChar(_Source, Char, ShiftControlAlt) = window::defaultCharHandling.

predicates
goto_sci_edit_pos : ( integer ) procedure( i ).

clauses

goto_sci_edit_pos( Pos_i ):-
  % current_lexer( Sci_lex ),
%  Sci_lex:gotoPos( Pos_i ) ,

  sciLexer_ctl:setFocus(),
  Ps = Pos_i - 1, Pe = Pos_i + 1,
  sciLexer_ctl:gotoPos( Pe ) ,
%  sciLexer_ctl:setPosition( pnt( I,O) ),

%  sciLexer_ctl:setSelectionNStart( 1, Pos_i ),
%  sciLexer_ctl:setSelectionNEnd( 1, Pe ),
% TESTTT
%  sciLexer_ctl:selectionStart = Pos_i,
%  sciLexer_ctl:selectionEnd = Pe ,


%  DIT IS dus assignment  en het werkt / it works !!
% we willen nu weten
% USE THIS
%  vpiCommonDialogs::note( "start set sel", "aappqq" ),
%  DIT IS dus assignment  en het werkt / it works !!
%  sciLexer_ctl:selectionStart := 10,
%  sciLexer_ctl:selectionEnd := 15 ,

%  sciLexer_ctl:selectionStart( 10 ),
%  sciLexer_ctl:selectionEnd( 15 ),

  ! .
goto_sci_edit_pos( _Pos_i ):- !.

% modal
% stdio::write( "key: ", toString( _Char ), " ", toString( _ShiftControlAlt ) , "\n").

%onSciLexerChar(_Source, _Char, _ShiftControlAlt) = window::defaultCharHandling:- !,
% stdio::write( "key: ", toString( _Char ), " ", toString( _ShiftControlAlt ) , "\n").


predicates
onSciLexerMouseMove : window::mouseMoveListener.
clauses

onSciLexerMouseMove(_Source, _Point, _ShiftControlAlt, _Buttons):-   mous_dbl() , ! .
onSciLexerMouseMove(_Source, _Point, _ShiftControlAlt, _Buttons):-   mous_down() , ! .
% THis works !!!
onSciLexerMouseMove(_Source, Point, _ShiftControlAlt, _Buttons):-
 Point = pnt( X, Y ),
%  sciLexer_ctl:get
% Selected = sciLexer_ctl:selText, Lx_sel = string::length( Selected ) ,  Lx_sel = 0 ,

   Txp = sciLexer_ctl:text,   Le = string::length( Txp ) ,
% twijfel of we string length erbij moeten betrekken
% dat is om de selectie uit te zetten als de muis erbuiten ligt
% anders is het irritant dat de selectie blijft
 Posx = sciLexer_ctl:positionFromPoint( X , Y ) ,
   Posx < Le ,
% xyxyxy
   stdio::write( "\nMouse movepp ", toString( Posx ) ) ,
 safePie::get_clause_positionx( Posx , Pred , Start, End ) ,
  End > 0 ,
% stdio::write( "\nMouse move ", toString( Posx ) ) , !.
 stdio::write( "\nFound position ", Pred , toString( Start ) , " " , toString( End ) ) ,
   sciLexer_ctl:selectionStart := Start ,
   sciLexer_ctl:selectionEnd := End ,

 !.

onSciLexerMouseMove(_Source, Point, _ShiftControlAlt, _Buttons):-
 Point = pnt( X, Y ),
 Posx = sciLexer_ctl:positionFromPoint( X , Y ) ,
   sciLexer_ctl:setEmptySelection(Posx),
% sciLexer_ctl:gotoPos( Pe ) ,
!.
%   sciLexer_ctl:selectionStart := 1 ,
%   sciLexer_ctl:selectionEnd := 2 , !.


predicates
onSciLexerMouseDbl : window::mouseDblListener.
clauses
onSciLexerMouseDbl(_Source, _Point, _ShiftControlAlt, _Button):- retract( mous_dbl() ), !.

onSciLexerMouseDbl(_Source, _Point, _ShiftControlAlt, _Button):-  assert( mous_dbl() ),!.

onSciLexerMouseDbl(_Source, _Point, _ShiftControlAlt, _Button).

predicates
onSciLexerMouseDown : window::mouseDownListener.
clauses

% 13-12-2025
% we kunnen deze posities  dus gaan gebruiken om metta code gedeeltes te arceren
% arceren via select start end etc
% en evt te laten wijzigen , opties geven etc
% eerst ook kleur van selected anders zetten

onSciLexerMouseDown(_Source, Point, _ShiftControlAlt, Button):-  retract( mous_down() ), !.
onSciLexerMouseDown(_Source, Point, _ShiftControlAlt, Button):-  assert( mous_down() ), !.

onSciLexerMouseDown(_Source, Point, _ShiftControlAlt, Button):-
 % button 0 is linker button
 Point = pnt( X, Y ), Button = 0 ,
% sciLexer_ctl:positionFromPoint(X, Y) = sciLexer_api::positionFromPoint(native, X, Y).
 Posx = sciLexer_ctl:positionFromPoint( X , Y ),
 stdio::write( "\nMouse down " , toString( Button ), " ", toString( Posx ) ) , !.

onSciLexerMouseDown(_Source, _Point, _ShiftControlAlt, _Button).

predicates
onPushButton118Click : button::clickResponder.
clauses
onPushButton118Click(_Source) = button::defaultAction:-
    Txp = sciLexer_ctl:text,
% 24-5-2025
%   safePie::start_parse_x( Txp , Result , Line, Line_pos ) ,
   safePie::start_only_parse_x( Txp , Result , Line, Line_pos ) ,
   if_there_is_an_error_goto_pos_and_fail( Result , Line, Line_pos ), !.

onPushButton118Click(_Source) = button::defaultAction:- !.

predicates
onSciLexerGetFocus : window::getFocusListener.
clauses
onSciLexerGetFocus(_Source):-
  stdio::write( "getfocus\n").

predicates
onSciLexerLoseFocus : window::loseFocusListener.
clauses
onSciLexerLoseFocus(_Source):-
 stdio::write( "losefocus\n").

predicates
onSciLexerMouseEnter : window::mouseEnterListener.
clauses
onSciLexerMouseEnter(_Window, _Point, _Flags):-
 stdio::write( "mousenter\n").

% This code is maintained automatically, do not update it manually.
%  11:27:01-23.12.2025

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    edit_ctl : editControl.
    pushButton_ctl : button.
    pushButton1_ctl : button.
    sciLexer_ctl : scilexer.
    pushButton2_ctl : button.
    pushButton3_ctl : button.
    pushButton4_ctl : button.
    pushButton5_ctl : button.
    pushButton6_ctl : button.
    pushButton7_ctl : button.
    pushButton8_ctl : button.
    pushButton9_ctl : button.
    sciLexerStatusBar_ctl : scilexerstatusbar.
    pushButton10_ctl : button.
    pushButton11_ctl : button.
    pushButton12_ctl : button.
    pushButton13_ctl : button.
    checkBox_ctl : checkButton.
    pushButton14_ctl : button.
    pushButton15_ctl : button.
    pushButton16_ctl : button.
    pushButton17_ctl : button.
    pushButton18_ctl : button.
    pushButton19_ctl : button.
    pushButton110_ctl : button.
    pushButton111_ctl : button.
    pushButton112_ctl : button.
    pushButton113_ctl : button.
    pushButton114_ctl : button.
    pushButton115_ctl : button.
    pushButton116_ctl : button.
    pushButton117_ctl : button.
    checkBox1_ctl : checkButton.
    pushButton118_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Metta language"),
        setRect(rct(50, 40, 650, 234)),
        setModal(false),
        setDecoration(titlebar([])),
        setBorder(sizeBorder()),
        setState([wsf_NoClipSiblings]),
        addShowListener(onShow),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("Prolog subclause"),
        ok_ctl:setPosition(500, 72),
        ok_ctl:setSize(88, 10),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::top, control::right]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Close"),
        cancel_ctl:setPosition(4, 170),
        cancel_ctl:setSize(40, 16),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::left, control::bottom]),
        cancel_ctl:setClickResponder(onCancelClick),
        help_ctl := button::new(This),
        help_ctl:setText("Explanation"),
        help_ctl:setPosition(500, 84),
        help_ctl:setSize(88, 10),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::top, control::right]),
        help_ctl:setClickResponder(onHelpClick),
        edit_ctl := editControl::new(This),
        edit_ctl:setText(""),
        edit_ctl:setPosition(256, 170),
        edit_ctl:setWidth(20),
        edit_ctl:setHeight(18),
        edit_ctl:setVisible(false),
        edit_ctl:setMultiLine(),
        edit_ctl:setWantReturn(),
        edit_ctl:setAutoVScroll(true),
        edit_ctl:addModifiedListener(onEditModified),
        pushButton_ctl := button::new(This),
        pushButton_ctl:setText("Parse - Transpile - Metta"),
        pushButton_ctl:setPosition(502, 20),
        pushButton_ctl:setSize(88, 10),
        pushButton_ctl:defaultHeight := false,
        pushButton_ctl:setAnchors([control::top, control::right]),
        pushButton_ctl:setBorder(false),
        pushButton_ctl:setClickResponder(onPushButtonClick),
        pushButton1_ctl := button::new(This),
        pushButton1_ctl:setText("Parse prolog"),
        pushButton1_ctl:setPosition(500, 60),
        pushButton1_ctl:setSize(88, 10),
        pushButton1_ctl:defaultHeight := false,
        pushButton1_ctl:setAnchors([control::top, control::right]),
        pushButton1_ctl:setClickResponder(onPushButton1Click),
        sciLexer_ctl := scilexer::new(This),
        sciLexer_ctl:setPosition(4, 4),
        sciLexer_ctl:setSize(488, 124),
        sciLexer_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        sciLexer_ctl:setBorder(true),
        sciLexer_ctl:addGetFocusListener(onSciLexerGetFocus),
        sciLexer_ctl:addLoseFocusListener(onSciLexerLoseFocus),
        sciLexer_ctl:addMenuItemListener(onSciLexerMenuItem),
        sciLexer_ctl:addMouseDblListener(onSciLexerMouseDbl),
        sciLexer_ctl:addMouseDownListener(onSciLexerMouseDown),
        sciLexer_ctl:addMouseEnterListener(onSciLexerMouseEnter),
        sciLexer_ctl:addMouseMoveListener(onSciLexerMouseMove),
        sciLexer_ctl:setCharResponder(onSciLexerChar),
        sciLexer_ctl:setContextMenuResponder(onSciLexerContextMenu),
        pushButton2_ctl := button::new(This),
        pushButton2_ctl:setText("Examp1"),
        pushButton2_ctl:setPosition(4, 136),
        pushButton2_ctl:setSize(36, 10),
        pushButton2_ctl:defaultHeight := false,
        pushButton2_ctl:setAnchors([control::bottom]),
        pushButton2_ctl:setClickResponder(onPushButton2Click),
        pushButton3_ctl := button::new(This),
        pushButton3_ctl:setText("Examp2"),
        pushButton3_ctl:setPosition(44, 136),
        pushButton3_ctl:setSize(36, 10),
        pushButton3_ctl:defaultHeight := false,
        pushButton3_ctl:setAnchors([control::bottom]),
        pushButton3_ctl:setClickResponder(onPushButton3Click),
        pushButton4_ctl := button::new(This),
        pushButton4_ctl:setText("Examp3"),
        pushButton4_ctl:setPosition(84, 136),
        pushButton4_ctl:setSize(36, 10),
        pushButton4_ctl:defaultHeight := false,
        pushButton4_ctl:setAnchors([control::bottom]),
        pushButton4_ctl:setClickResponder(onPushButton4Click),
        pushButton5_ctl := button::new(This),
        pushButton5_ctl:setText("Load Metta-file"),
        pushButton5_ctl:setPosition(500, 48),
        pushButton5_ctl:setSize(88, 10),
        pushButton5_ctl:defaultHeight := false,
        pushButton5_ctl:setAnchors([control::top, control::right]),
        pushButton5_ctl:setClickResponder(onPushButton5Click),
        pushButton6_ctl := button::new(This),
        pushButton6_ctl:setText("Examp4"),
        pushButton6_ctl:setPosition(124, 136),
        pushButton6_ctl:setSize(36, 10),
        pushButton6_ctl:defaultHeight := false,
        pushButton6_ctl:setAnchors([control::bottom]),
        pushButton6_ctl:setClickResponder(onPushButton6Click),
        pushButton7_ctl := button::new(This),
        pushButton7_ctl:setText("Examp5"),
        pushButton7_ctl:setPosition(168, 136),
        pushButton7_ctl:setSize(36, 10),
        pushButton7_ctl:defaultHeight := false,
        pushButton7_ctl:setAnchors([control::bottom]),
        pushButton7_ctl:setClickResponder(onPushButton7Click),
        pushButton8_ctl := button::new(This),
        pushButton8_ctl:setText("Examp6"),
        pushButton8_ctl:setPosition(212, 136),
        pushButton8_ctl:setSize(36, 10),
        pushButton8_ctl:defaultHeight := false,
        pushButton8_ctl:setAnchors([control::bottom]),
        pushButton8_ctl:setClickResponder(onPushButton8Click),
        pushButton9_ctl := button::new(This),
        pushButton9_ctl:setText("Examp7"),
        pushButton9_ctl:setPosition(252, 136),
        pushButton9_ctl:setSize(36, 10),
        pushButton9_ctl:defaultHeight := false,
        pushButton9_ctl:setAnchors([control::bottom]),
        pushButton9_ctl:setClickResponder(onPushButton9Click),
        sciLexerStatusBar_ctl := scilexerstatusbar::new(This),
        sciLexerStatusBar_ctl:setPosition(148, 174),
        sciLexerStatusBar_ctl:setSize(68, 14),
        sciLexerStatusBar_ctl:setAnchors([control::bottom]),
        pushButton10_ctl := button::new(This),
        pushButton10_ctl:setText("Examp8"),
        pushButton10_ctl:setPosition(292, 136),
        pushButton10_ctl:setSize(36, 10),
        pushButton10_ctl:defaultHeight := false,
        pushButton10_ctl:setAnchors([control::bottom]),
        pushButton10_ctl:setClickResponder(onPushButton10Click),
        pushButton11_ctl := button::new(This),
        pushButton11_ctl:setText("Examp9"),
        pushButton11_ctl:setPosition(332, 136),
        pushButton11_ctl:setSize(36, 10),
        pushButton11_ctl:defaultHeight := false,
        pushButton11_ctl:setAnchors([control::bottom]),
        pushButton11_ctl:setClickResponder(onPushButton11Click),
        pushButton12_ctl := button::new(This),
        pushButton12_ctl:setText("Examp10"),
        pushButton12_ctl:setPosition(372, 136),
        pushButton12_ctl:setSize(36, 10),
        pushButton12_ctl:defaultHeight := false,
        pushButton12_ctl:setAnchors([control::bottom]),
        pushButton12_ctl:setClickResponder(onPushButton12Click),
        pushButton13_ctl := button::new(This),
        pushButton13_ctl:setText("Examp11"),
        pushButton13_ctl:setPosition(412, 136),
        pushButton13_ctl:setSize(36, 10),
        pushButton13_ctl:defaultHeight := false,
        pushButton13_ctl:setAnchors([control::bottom]),
        pushButton13_ctl:setClickResponder(onPushButton13Click),
        checkBox_ctl := checkButton::new(This),
        checkBox_ctl:setText("Dont show debug messages"),
        checkBox_ctl:setPosition(496, 108),
        checkBox_ctl:setWidth(94),
        checkBox_ctl:setAnchors([control::top, control::right]),
        checkBox_ctl:addStateChangedListener(onCheckBoxStateChanged),
        pushButton14_ctl := button::new(This),
        pushButton14_ctl:setText("Examp12"),
        pushButton14_ctl:setPosition(452, 136),
        pushButton14_ctl:setSize(36, 10),
        pushButton14_ctl:defaultHeight := false,
        pushButton14_ctl:setAnchors([control::bottom]),
        pushButton14_ctl:setClickResponder(onPushButton14Click),
        pushButton15_ctl := button::new(This),
        pushButton15_ctl:setText("Examp13"),
        pushButton15_ctl:setPosition(8, 154),
        pushButton15_ctl:setSize(36, 10),
        pushButton15_ctl:defaultHeight := false,
        pushButton15_ctl:setAnchors([control::bottom]),
        pushButton15_ctl:setClickResponder(onPushButton15Click),
        pushButton16_ctl := button::new(This),
        pushButton16_ctl:setText("Set transpile model"),
        pushButton16_ctl:setPosition(502, 6),
        pushButton16_ctl:setSize(88, 10),
        pushButton16_ctl:defaultHeight := false,
        pushButton16_ctl:setAnchors([control::top, control::right]),
        pushButton16_ctl:setClickResponder(onPushButton16Click),
        pushButton17_ctl := button::new(This),
        pushButton17_ctl:setText("Examp14"),
        pushButton17_ctl:setPosition(48, 154),
        pushButton17_ctl:setSize(36, 10),
        pushButton17_ctl:defaultHeight := false,
        pushButton17_ctl:setAnchors([control::bottom]),
        pushButton17_ctl:setClickResponder(onPushButton17Click),
        pushButton18_ctl := button::new(This),
        pushButton18_ctl:setText("Examp15"),
        pushButton18_ctl:setPosition(92, 153),
        pushButton18_ctl:setSize(36, 10),
        pushButton18_ctl:defaultHeight := false,
        pushButton18_ctl:setAnchors([control::bottom]),
        pushButton18_ctl:setClickResponder(onPushButton18Click),
        pushButton19_ctl := button::new(This),
        pushButton19_ctl:setText("Examp16"),
        pushButton19_ctl:setPosition(136, 153),
        pushButton19_ctl:setSize(36, 10),
        pushButton19_ctl:defaultHeight := false,
        pushButton19_ctl:setAnchors([control::bottom]),
        pushButton19_ctl:setClickResponder(onPushButton19Click),
        pushButton110_ctl := button::new(This),
        pushButton110_ctl:setText("Examp17"),
        pushButton110_ctl:setPosition(180, 153),
        pushButton110_ctl:setSize(36, 10),
        pushButton110_ctl:defaultHeight := false,
        pushButton110_ctl:setAnchors([control::bottom]),
        pushButton110_ctl:setClickResponder(onPushButton110Click),
        pushButton111_ctl := button::new(This),
        pushButton111_ctl:setText("Examp18"),
        pushButton111_ctl:setPosition(224, 153),
        pushButton111_ctl:setSize(36, 10),
        pushButton111_ctl:defaultHeight := false,
        pushButton111_ctl:setAnchors([control::bottom]),
        pushButton111_ctl:setClickResponder(onPushButton111Click),
        pushButton112_ctl := button::new(This),
        pushButton112_ctl:setText("Examp19"),
        pushButton112_ctl:setPosition(268, 153),
        pushButton112_ctl:setSize(36, 10),
        pushButton112_ctl:defaultHeight := false,
        pushButton112_ctl:setAnchors([control::bottom]),
        pushButton112_ctl:setClickResponder(onPushButton112Click),
        pushButton113_ctl := button::new(This),
        pushButton113_ctl:setText("Example 20"),
        pushButton113_ctl:setPosition(312, 153),
        pushButton113_ctl:setSize(36, 10),
        pushButton113_ctl:defaultHeight := false,
        pushButton113_ctl:setAnchors([control::bottom]),
        pushButton113_ctl:setClickResponder(onPushButton113Click),
        pushButton114_ctl := button::new(This),
        pushButton114_ctl:setText("Example 21"),
        pushButton114_ctl:setPosition(356, 153),
        pushButton114_ctl:setSize(36, 10),
        pushButton114_ctl:defaultHeight := false,
        pushButton114_ctl:setAnchors([control::bottom]),
        pushButton114_ctl:setClickResponder(onPushButton114Click),
        pushButton115_ctl := button::new(This),
        pushButton115_ctl:setText("Example 22"),
        pushButton115_ctl:setPosition(400, 152),
        pushButton115_ctl:setSize(36, 10),
        pushButton115_ctl:defaultHeight := false,
        pushButton115_ctl:setAnchors([control::bottom]),
        pushButton115_ctl:setClickResponder(onPushButton115Click),
        pushButton116_ctl := button::new(This),
        pushButton116_ctl:setText("example 23"),
        pushButton116_ctl:setPosition(448, 152),
        pushButton116_ctl:setSize(36, 10),
        pushButton116_ctl:defaultHeight := false,
        pushButton116_ctl:setAnchors([control::bottom]),
        pushButton116_ctl:setClickResponder(onPushButton116Click),
        pushButton117_ctl := button::new(This),
        pushButton117_ctl:setText("Font"),
        pushButton117_ctl:setPosition(500, 96),
        pushButton117_ctl:setSize(88, 10),
        pushButton117_ctl:defaultHeight := false,
        pushButton117_ctl:setAnchors([control::top, control::right]),
        pushButton117_ctl:setClickResponder(onPushButton117Click),
        checkBox1_ctl := checkButton::new(This),
        checkBox1_ctl:setText("without examples"),
        checkBox1_ctl:setPosition(496, 124),
        checkBox1_ctl:setWidth(96),
        checkBox1_ctl:setAnchors([control::top, control::right]),
        checkBox1_ctl:addStateChangedListener(onCheckBox1StateChanged),
        pushButton118_ctl := button::new(This),
        pushButton118_ctl:setText("Parse Metta"),
        pushButton118_ctl:setPosition(504, 34),
        pushButton118_ctl:setSize(88, 10),
        pushButton118_ctl:defaultHeight := false,
        pushButton118_ctl:setAnchors([control::top, control::right]),
        pushButton118_ctl:setClickResponder(onPushButton118Click).
% end of automatic code

end implement bigstr
