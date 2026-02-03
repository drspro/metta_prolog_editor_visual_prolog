% Copyright ©

class bigstr : bigstr
    open core

predicates
    display : (window Parent) -> bigstr Dialog.


set_dia_size : (window) procedure(i).
set_dia_size_rct : (window, vpiDomains::rct) procedure(i,i).

get_debug : ( boolean )  procedure( o ).

current_example_get : ( string ) procedure( o ).



constructors
    new : (window Parent).

end class bigstr
