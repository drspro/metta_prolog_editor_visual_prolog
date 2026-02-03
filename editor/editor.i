% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

namespace pie

interface editor supports formWindow
    open core

predicates
    clean_up_data : (booleanInt Continue [out]) determ.

predicates
    getEntireText : () -> string Text.




end interface editor
