% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

namespace pie

class editor : editor
    [noDefaultConstructor]

constants
    fileMenuTagBase : vpiDomains::menuTag = 2000.

predicates
    show_menu_with_filelists : (vpiDomains::resId, vpiDomains::menu [out]).

predicates
    close_editors : () -> boolean.

predicates
    open_file : (string, boolean) determ.

predicates
    getText : (string FileName) -> string Text determ.

predicates
set_size : () procedure().
set_size_rct : ( vpiDomains::rct ) procedure( i ).

set_position_custom : ( vpiDomains::rct ) procedure( i ).
remove_position_custom : ( vpiDomains::rct ) procedure( i ).

end class editor
