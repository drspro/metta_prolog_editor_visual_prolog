% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

class configuration

predicates
    save_cfg : ().
    load_cfg : ().

predicates
    init_ui : (window Task, window Msg).

predicates
    get_edit_font : (vpiDomains::font [out]).

predicates

resize_Msg_rct : (window Wind, vpiDomains::rct ) procedure( i,i).

    set_msg_pos : (window Wind).
    set_msg_pos_rct : ( vpiDomains::rct [in] ).
     % ,

set_editor_pos : ( vpiDomains::rct [in] ).
set_metta_dia_pos : ( vpiDomains::rct [in] ).

get_editor_pos : ( vpiDomains::rct [out] ).
get_metta_dia_pos : ( vpiDomains::rct [out] ).


    % vpiDomains::rct [out],
get_msg_height : ( integer [out]  ).

predicates
    set_task_pos : (window Wind).

predicates
    get_src_dir : (string [out]).
    set_src_dir : (string).

predicates
    get_lru_list : (string* [out]).

predicates
    set_lru_pos : (string FIlename, vpiDomains::rct Rect, core::charCount EditorPos).

predicates
    get_lru_pos : (vpiDomains::windowHandle, string, vpiDomains::rct [out], core::charCount [out]).
    get_open_editors_list : (string* [out]).
    retract_open_editor : (string).
    get_start_error : (string [out]).

end class configuration
