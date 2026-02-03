% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement projectToolbar
    open vpiDomains, vpiToolbar, resourceIdentifiers

clauses
    create(Parent) :-
        _ = vpiToolbar::create(style, Parent, controlList).

% This code is maintained automatically, do not update it manually.
%  16:29:55-16.9.2025

constants
    style : vpiToolbar::style = tb_top.
    controlList : vpiToolbar::control_list =
        [
            tb_ctrl(id_file_new, pushb, resId(idb_NewFileBitmap), "New;New file", 1, 1),
            tb_ctrl(id_file_open, pushb, resId(idb_OpenFileBitmap), "Open;Open file", 1, 1),
            tb_ctrl(id_file_consult, pushb, resId(idb_consult), "Consult;Consult File", 1, 1),
            tb_ctrl(id_file_save, pushb, resId(idb_SaveFileBitmap), "Save;File save", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_edit_undo, pushb, resId(idb_UndoBitmap), "Undo;Undo", 1, 1),
            tb_ctrl(id_edit_redo, pushb, resId(idb_RedoBitmap), "Redo;Redo", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_edit_cut, pushb, resId(idb_CutBitmap), "Cut;Cut to clipboard", 1, 1),
            tb_ctrl(id_edit_copy, pushb, resId(idb_CopyBitmap), "Copy;Copy to clipboard", 1, 1),
            tb_ctrl(id_edit_paste, pushb, resId(idb_PasteBitmap), "Paste;Paste from clipboard", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_Engine_reconsult, pushb, resId(idb_reconsult), "Reconsult;Reconsult this text", 1, 1),
            tb_ctrl(id_engine_reset, pushb, resId(idb_reset), "Reset;Reset the Engine", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_Engine_trace_calls, pushb, resId(idb_trace), "Trace;Trace Calls", 1, 1),
            tb_ctrl(id_Engine_stop_execution, pushb, resId(idb_stop), "Stop;Stop Execution", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_file_layout1, pushb, resId(idb_CopyBitmap), "", 1, 1),
            tb_ctrl(id_file_layout2, pushb, resId(idb_CopyBitmap), "", 1, 1),
            tb_ctrl(id_file_layout3, pushb, resId(idb_CopyBitmap), "", 1, 1),
            tb_ctrl(id_file_layout4, pushb, resId(idb_CopyBitmap), "", 1, 1),
            tb_ctrl(id_help_contents, pushb, resId(idb_HelpBitmap), "Help;Help", 1, 1)
        ].
    % end of automatic code

end implement projectToolbar