% Copyright ©

class popup_win : popup_win
    open core

predicates
    display : (window Parent) -> popup_win Dialog.

constructors
    new : (window Parent).

end class popup_win
