% Copyright PDC

namespace pfc\pie

class scanner
    open core

domains
    tok =
        lbrack;
        rbrack;
        lpar;
        rpar;
        lcurly;
        rcurly;
        var(string);
        atom(string);
        int(integer64);
        real(real);
        str(string);
        char(char);
        comma;
        bar;
        dot.

domains
    cursortok = t(tok, charCount).
    tokl = cursortok*.

predicates
    tokl : (charCount Cursor, string Input, cursortok* TokenList [out]).
    atomNeedNotBeQuoted : (string Atom) determ.

end class scanner
