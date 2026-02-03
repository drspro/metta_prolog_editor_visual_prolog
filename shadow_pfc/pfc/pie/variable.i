% Copyright PDC

namespace pfc\pie

interface variable

predicates
    getTerm : () -> pie::term Term.
    % @short If the varaible is bound #Term is the term it is bound to, otherwise it is a term representing the free variable.
    % @end

predicates
    setTerm : (pie::term Term).
    setTerm : (pie::term Term, resetPoint Resetpoint).
    % @short Bind the variable to #Term, where #Resetpoint (dafault none) is the resetpoint for backtracking.
    % @end

predicates
    isFreeGetVariable : () -> variable FreeVariable determ.
    % @short If the variable is free (or bound transitively to a free variable) #FreeVariable is the truly free variable
    % @end

properties
    printName : string (o).
    % @short The print name of the variable.
    % @end

end interface variable
