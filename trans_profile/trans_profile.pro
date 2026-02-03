% Copyright ©

implement trans_profile inherits dialog
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
onShow(_Source, _Data):-
     safePie::get_model_type_namesx( Lis ),
%     pie::
     % get_model_type_names
     bigstr::current_example_get( Example_name ), setText( string::concat("Transpile model for : ", Example_name ) ),

     listButton_ctl:addList( Lis ) ,     listButton1_ctl:addList( Lis ) ,     listButton2_ctl:addList( Lis ) ,
     listButton3_ctl:addList( Lis ) ,                    center(),

     safePie::get_model_profilex( Lis2 , Is_che2 ) ,
%       vpiCommondialogs::note("aa00","aa00"),
     Lis2 = [ Na1, Na2 , Na3 , Na4 ] ,
%     vpiCommondialogs::note("aa1", toString( Lis2 ) ),

  list::memberIndex_nd( El1, Ind1, Lis ) , El1 = Na1 ,   listButton_ctl:selectAt( Ind1, true ) ,
  list::memberIndex_nd( El2, Ind2, Lis ) , El2 = Na2 ,   listButton1_ctl:selectAt( Ind2, true ) ,
  list::memberIndex_nd( El3, Ind3, Lis ) , El3 = Na3 ,   listButton2_ctl:selectAt( Ind3, true ) ,
  list::memberIndex_nd( El4, Ind4, Lis ) , El4 = Na4 ,   listButton3_ctl:selectAt( Ind4, true ) ,

 %  vpiCommondialogs::note("aa2","aa2"),

%  Is_che = uncheckedConvert( boolean, Is_che2 ),
   Is_che = boolean::fromBooleanInt(  Is_che2 ),
  checkBox_ctl:setChecked( Is_che ),

  %  vpiCommondialogs::note("aa3","aa3"),

     ! .

onShow(_Source, _Data):- !.

predicates
onOkClick : button::clickResponder.
clauses
onOkClick(_Source) = button::defaultAction:-
 bigstr::current_example_get( Na ),
          Sel1 = listButton_ctl:getSelectedItems() ,  Sel1 = [S1],
          Sel2 = listButton1_ctl:getSelectedItems() , Sel2 = [S2],
          Sel3 = listButton2_ctl:getSelectedItems() , Sel3 = [S3],
          Sel4 = listButton3_ctl:getSelectedItems() , Sel4 = [S4],
          Is_che = checkBox_ctl:getChecked(),
%     Is_che_i = uncheckedConvert( integer, Is_che ),
      Is_che_i = boolean::toBooleanInt(Is_che),
                 safePie::set_model_profilex( Na, [ S1 , S2 , S3 , S4 ] , Is_che_i ) ,
                              ! .

onOkClick(_Source) = button::defaultAction:-!.

predicates
    onCancelClick : button::clickResponder.
clauses
    onCancelClick(_Source) = button::defaultAction.

predicates
    onListButtonCloseUp : listControl::closeUpListener.
clauses
    onListButtonCloseUp(_Source).

predicates
    onListButton1CloseUp : listControl::closeUpListener.
clauses
    onListButton1CloseUp(_Source).

predicates
    onListButton2CloseUp : listControl::closeUpListener.
clauses
    onListButton2CloseUp(_Source).

predicates
    onListButton3CloseUp : listControl::closeUpListener.
clauses
    onListButton3CloseUp(_Source).

predicates
    onCheckBoxStateChanged : checkButton::stateChangedListener.
clauses
    onCheckBoxStateChanged(_Source, _OldState, _NewState).

predicates
    onCheckBoxClick : checkButton::clickResponder.
clauses
    onCheckBoxClick(_Source).

predicates
onPushButtonClick : button::clickResponder.
clauses

onPushButtonClick(_Source) = button::defaultAction:-
  safePie::get_model_type_namesx( Lis ),

  % vpiCommondialogs::note( "start " , toString( Lis ) ),

%     pie::
     % get_model_type_names
  %   bigstr::current_example_get( Example_name ), setText( string::concat("Transpile model for : ", Example_name ) ),
 listButton_ctl:clearAll(),listButton1_ctl:clearAll(),listButton2_ctl:clearAll(),listButton3_ctl:clearAll(),

     listButton_ctl:addList( Lis ) ,     listButton1_ctl:addList( Lis ) ,     listButton2_ctl:addList( Lis ) ,
     listButton3_ctl:addList( Lis ) ,

 % center(),

 % current_transpile_model( 0, preserve_metta_vars , preserve_metta_output_var , keep_all, 0  )
% transpile_default = 0.
% preserve_metta_vars = 201.
% keep_all = 202.
% remove_metta_outpvar_and_created = 203.
% preserve_metta_output_var = 204.
% dont_allow_output_vars = 205.
%const_name( "transpile_default", transpile_default ).
%const_name( "preserve_metta_vars", preserve_metta_vars ).
%const_name( "keep_all", keep_all ).
%const_name( "remove_metta_outpvar_and_created", remove_metta_outpvar_and_created ).
%const_name( "preserve_metta_output_var ",  preserve_metta_output_var ).
%const_name( "dont_allow_output_vars ",  dont_allow_output_vars ).

  Lis2 = [ "transpile_default", "preserve_metta_vars" , "preserve_metta_output_var" , "keep_all" ],
%      safePie::get_model_profilex( Lis2 , Is_che2 ) ,
%       vpiCommondialogs::note("aa00","aa00"),
     Lis2 = [ Na1, Na2 , Na3 , Na4 ] ,
%     vpiCommondialogs::note("aa1", toString( Lis2 ) ),

  list::memberIndex_nd( El1, Ind1, Lis ) , El1 = Na1 ,   listButton_ctl:selectAt( Ind1, true ) ,
%    vpiCommondialogs::note( "Defaults set1" , "Defaults set1" ),
  list::memberIndex_nd( El2, Ind2, Lis ) , El2 = Na2 ,   listButton1_ctl:selectAt( Ind2, true ) ,
%   vpiCommondialogs::note( "Defaults set2" , "Defaults set2" ),

  list::memberIndex_nd( El3, Ind3, Lis ) , El3 = Na3 ,   listButton2_ctl:selectAt( Ind3, true ) ,
%   vpiCommondialogs::note( "Defaults set3" , "Defaults set3" ),
  list::memberIndex_nd( El4, Ind4, Lis ) , El4 = Na4 ,   listButton3_ctl:selectAt( Ind4, true ) ,
 % vpiCommondialogs::note( "Defaults set4" , "Defaults set4" ),


%  Is_che = uncheckedConvert( boolean, Is_che2 ),
    Is_che = boolean::fromBooleanInt(  1 ),
  checkBox_ctl:setChecked( Is_che ),
     vpiCommondialogs::note( "Defaults set" , "Defaults set" ),

  !.

onPushButtonClick(_Source) = button::defaultAction.

predicates
    onPushButton1Click : button::clickResponder.
clauses
    onPushButton1Click(_Source) = button::defaultAction.

predicates
onPushButton2Click : button::clickResponder.
clauses
onPushButton2Click(_Source) = button::defaultAction:-
           Sel1 = listButton_ctl:getSelectedItems() ,  Sel1 = [S1],
          Sel2 = listButton1_ctl:getSelectedItems() , Sel2 = [S2],
          Sel3 = listButton2_ctl:getSelectedItems() , Sel3 = [S3],
          Sel4 = listButton3_ctl:getSelectedItems() , Sel4 = [S4],
          Is_che = checkBox_ctl:getChecked(),
%     Is_che_i = uncheckedConvert( integer, Is_che ),
      Is_che_i = boolean::toBooleanInt(Is_che),
  bigstr::current_example_get( Name ),
      safePie::set_model_profilex( Name, [ S1 , S2 , S3 , S4 ] , Is_che_i ) , !.

onPushButton2Click(_Source) = button::defaultAction:- !.


% This code is maintained automatically, do not update it manually.
%  13:52:10-27.6.2025

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    listButton_ctl : listButton.
    listButton1_ctl : listButton.
    listButton2_ctl : listButton.
    listButton3_ctl : listButton.
    checkBox_ctl : checkButton.
    pushButton_ctl : button.
    pushButton1_ctl : button.
    pushButton2_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setFont(vpi::fontCreateByName("Courier New", 12)),
        setText("Transpile model"),
        setRect(rct(50, 40, 418, 316)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        addShowListener(onShow),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(308, 256),
        ok_ctl:setSize(56, 16),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(8, 254),
        cancel_ctl:setSize(56, 16),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        cancel_ctl:setClickResponder(onCancelClick),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(160, 254),
        help_ctl:setWidth(56),
        help_ctl:defaultHeight := true,
        help_ctl:setAnchors([control::right, control::bottom]),
        help_ctl:setVisible(false),
        listButton_ctl := listButton::new(This),
        listButton_ctl:setPosition(140, 66),
        listButton_ctl:setWidth(216),
        listButton_ctl:setMaxDropDownRows(5),
        listButton_ctl:setSort(false),
        listButton_ctl:setMaxDropDownRows(5),
        listButton_ctl:addCloseUpListener(onListButtonCloseUp),
        listButton1_ctl := listButton::new(This),
        listButton1_ctl:setPosition(140, 106),
        listButton1_ctl:setWidth(216),
        listButton1_ctl:setMaxDropDownRows(5),
        listButton1_ctl:setSort(false),
        listButton1_ctl:setEnabled(false),
        listButton1_ctl:addCloseUpListener(onListButton1CloseUp),
        listButton2_ctl := listButton::new(This),
        listButton2_ctl:setPosition(140, 146),
        listButton2_ctl:setWidth(216),
        listButton2_ctl:setMaxDropDownRows(5),
        listButton2_ctl:setSort(false),
        listButton2_ctl:addCloseUpListener(onListButton2CloseUp),
        listButton3_ctl := listButton::new(This),
        listButton3_ctl:setPosition(140, 186),
        listButton3_ctl:setWidth(216),
        listButton3_ctl:setMaxDropDownRows(5),
        listButton3_ctl:setSort(false),
        listButton3_ctl:addCloseUpListener(onListButton3CloseUp),
        StaticText_ctl = textControl::new(This),
        StaticText_ctl:setText("1. Main term variabels transpile type"),
        StaticText_ctl:setPosition(12, 46),
        StaticText_ctl:setSize(284, 14),
        StaticText1_ctl = textControl::new(This),
        StaticText1_ctl:setText("2. Predicate sequential variabels type, ( disabled because its the same for all )"),
        StaticText1_ctl:setPosition(12, 84),
        StaticText1_ctl:setSize(328, 16),
        StaticText2_ctl = textControl::new(This),
        StaticText2_ctl:setText("3. Subclause head variabels type"),
        StaticText2_ctl:setPosition(12, 125),
        StaticText2_ctl:setSize(180, 14),
        StaticText3_ctl = textControl::new(This),
        StaticText3_ctl:setText("4. Subclause body call variabels type"),
        StaticText3_ctl:setPosition(12, 164),
        StaticText3_ctl:setSize(160, 16),
        StaticText4_ctl = textControl::new(This),
        StaticText4_ctl:setText("Metta to prolog transpile model"),
        StaticText4_ctl:setPosition(88, 16),
        StaticText4_ctl:setSize(236, 12),
        checkBox_ctl := checkButton::new(This),
        checkBox_ctl:setText("Maximum depth of predicate variabels ,( temporarly disabled because its the same for all )"),
        checkBox_ctl:setPosition(8, 204),
        checkBox_ctl:setWidth(356),
        checkBox_ctl:setEnabled(false),
        checkBox_ctl:addStateChangedListener(onCheckBoxStateChanged),
        checkBox_ctl:setClickResponder(onCheckBoxClick),
        GroupBox_ctl = groupBox::new(This),
        GroupBox_ctl:setText("Metta transpile model"),
        GroupBox_ctl:setPosition(8, 4),
        GroupBox_ctl:setSize(352, 28),
        pushButton_ctl := button::new(This),
        pushButton_ctl:setText("Set defaults"),
        pushButton_ctl:setPosition(20, 220),
        pushButton_ctl:setSize(92, 14),
        pushButton_ctl:defaultHeight := false,
        pushButton_ctl:setClickResponder(onPushButtonClick),
        pushButton1_ctl := button::new(This),
        pushButton1_ctl:setText("Next config > "),
        pushButton1_ctl:setPosition(124, 220),
        pushButton1_ctl:setSize(80, 14),
        pushButton1_ctl:defaultHeight := false,
        pushButton1_ctl:setVisible(false),
        pushButton1_ctl:setClickResponder(onPushButton1Click),
        pushButton2_ctl := button::new(This),
        pushButton2_ctl:setText("Save config for this example"),
        pushButton2_ctl:setPosition(232, 236),
        pushButton2_ctl:setSize(128, 14),
        pushButton2_ctl:defaultHeight := false,
        pushButton2_ctl:setClickResponder(onPushButton2Click).
% end of automatic code

end implement trans_profile
