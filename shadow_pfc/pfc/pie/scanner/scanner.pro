% Copyright PDC

namespace pfc\pie

implement scanner
    open core

clauses
    tokl(Pos, Str, [t(Tok, Pos1) | TokL]) :-
        skipspaces(Pos, Str, Str1, NoOfSp),
        Pos1 = Pos + NoOfSp,
        string::frontToken(Str1, StrTok, Str2),
        !,
        maketok(Pos, StrTok, Tok, Str2, Str3, Len1),
        Len = string::length(StrTok),
        POS2 = Pos1 + Len + Len1,
        tokl(POS2, Str3, TokL).

    tokl(_, _, []).

clauses
    atomNeedNotBeQuoted(Atom) :-
        try
            tokl(0, Atom, R),
            R = [t(atom(Atom), _)]
        catch _ do
            fail
        end try.

class predicates
    skipspaces : (charCount Cursor, string, string [out], charCount [out]).
clauses
    skipspaces(Cursor, Str, Str2, N + 1) :-
        string::frontChar(Str, Ch, Str1),
        isSpace(Ch),
        !,
        skipspaces(Cursor + 1, Str1, Str2, N).

    skipspaces(Cursor, Str, Str3, 1 + N + 1 + N2) :-
        string::frontChar(Str, Ch1, Str1),
        Ch1 = '%',
        !,
        if N = string::searchChar(Str1, '\n') then
            Str2 = string::fromPosition(Str1, N + 1),
            skipspaces(Cursor + N + 2, Str2, Str3, N2)
        else
            pie::raiseSyntaxError(Cursor, @"<EOS> while scanning %% \n comment.")
        end if.

    skipspaces(Cursor, Str, Str4, NoOfSp) :-
        string::frontChar(Str, Ch1, Str1),
        Ch1 = '/',
        string::frontChar(Str1, Ch2, Str2),
        Ch2 = '*',
        !,
        if N = commentlength(Str2, 0) then
            Str3 = string::fromPosition(Str2, N),
            skipspaces(Cursor + N + 2, Str3, Str4, N1),
            NoOfSp = N1 + 2 + N
        else
            pie::raiseSyntaxError(Cursor, "<EOS> while scanning /* */ comment.")
        end if.

    skipspaces(_, Str, Str, 0).

class predicates
    isSpace : (char) determ.
clauses
    isSpace(' ').
    isSpace('\t').
    isSpace('\n').

class predicates
    white_follow : (string) determ.
clauses
    white_follow("") :-
        !.
    white_follow(S) :-
        Ch = string::frontChar(S),
        isSpace(Ch).

class predicates
    maketok : (charCount Cursor1, string First, tok Token [out], string S1, string S0 [out], charCount Cursor0 [out]).
clauses
    maketok(_, C, T, S, S, 0) :-
        T = isSimpleToken(C),
        !.

    maketok(_, C, atom(C), S, S, 0) :-
        C in ["+", "-", "*", "/", "="],
        '-' = string::frontChar(S),
        !.

    maketok(_, ".", dot, S, S, 0) :-
        white_follow(S),
        !.

    maketok(_, "`", char(T), S1, S2, 1) :-
        string::frontChar(S1, T, S2),
        !.

    maketok(Cursor, "\"", str(toTerm(string::concat("\"", Str, "\""))), S1, S2, Len) :-
        scan_str(Cursor, '\"', false, S1, S2, Str),
        !,
        Len = string::length(Str) + 1.

    maketok(Cursor, "@", str(Str), S1, S2, Len) :-
        string::frontchar(S1, QCh, S1B),
        QCh = '\"',
        % Unescaped String literal (escapes are left unprocessed)
        scan_str(Cursor, '\"', true, S1B, S2, Str),
        !,
        Len = string::length(Str) + 1.

    maketok(Cursor, "'", V, S1, S2, Len) :-
        scan_str(Cursor, '\'', false, S1, S2, Atom0),
        Atom = toTerm(string::concat("\"", Atom0, "\"")),
        !,
        V =
            if string::frontChar(Atom, Char, Rest) and Rest = "" then
                % one character atoms are chars
                char(Char)
            else
                atom(Atom)
            end if,
        Len = string::length(Atom0) + 1.

    maketok(_, String, atom(Symb), S, S1, Len) :-
        Ch = string::frontChar(String),
        is_symbchar(Ch),
        Len = scan_atom(S),
        !,
        string::front(S, Len, Symb1, S1),
        Symb = string::concat(String, Symb1).

    maketok(_, IntStr, int(Int), S, S, 0) :-
        Int = tryToTerm(integer64, IntStr),
        !.

    maketok(_, RealStr, real(Real), S, S, 0) :-
        Real = tryToTerm(real, RealStr),
        !.

    maketok(_, String, var(String), S, S, 0) :-
        Ch = string::frontChar(String),
        b_true = string_native::isCharUpper(Ch),
        string::isName(String),
        !.

    maketok(_, "_", var(Name), S1, S2, Len) :-
        Ch = string::frontChar(S1),
        ok_follow_uscore(Ch),
        string::frontToken(S1, Tok, S2),
        !,
        Len = string::length(Tok),
        Name = string::concat("_", string::toUpperCase(Tok)).

    maketok(_, "_", var("_"), S, S, 0) :-
        !.

    maketok(_, String, atom(Name), S, S, 0) :-
        string::isName(String),
        !,
        Name = string::toLowerCase(String).

    maketok(Cursor, Str, _, _, _, _) :-
        pie::raiseSyntaxError(Cursor, "Illegal token: '%s'", Str).

class predicates
    isSimpleToken : (string C) -> tok Token determ.
clauses
    isSimpleToken("(") = lpar.
    isSimpleToken(")") = rpar.
    isSimpleToken("[") = lbrack.
    isSimpleToken("]") = rbrack.
    isSimpleToken("{") = lcurly.
    isSimpleToken("}") = rcurly.
    isSimpleToken("|") = bar.
    isSimpleToken(",") = comma.
    isSimpleToken(";") = atom(";").
    isSimpleToken("!") = atom("!").

class predicates
    ok_follow_uscore : (char) determ.
clauses
    ok_follow_uscore('_') :-
        !.

    ok_follow_uscore(Ch) :-
        Ch >= '0',
        Ch <= '9',
        !.

    ok_follow_uscore(Ch) :-
        b_true = string_native::isCharLower(Ch),
        !.

    ok_follow_uscore(Ch) :-
        b_true = string_native::isCharUpper(Ch),
        !.

class predicates
    scan_str : (charCount, char ScanFor, boolean IgnoreEscape, string, string [out], string [out]) determ.
clauses
    scan_str(_, Ch, IgnoreEscape, In, Out, Str) :-
        N = if true = IgnoreEscape then search_ch(Ch, In, 0) else search_ch_nonescaped(Ch, In, 0) end if,
        string::front(In, N, Str, Out1),
        !,
        string::frontChar(Out1, _, Out).

    scan_str(Cursor, _, _, _, _, _) :-
        pie::raiseSyntaxError(Cursor, "String not terminated").

class predicates
    commentlength : (string, charCount Seen) -> charCount Length determ.
    % todo optimise don't use substring !!
clauses
    commentlength(Str, Seen) = Seen + 2 :-
        '*' = string::frontChar(Str),
        '/' = string::frontChar(string::fromPosition(Str, 1)),
        !.

    commentlength(Str, Seen) = Length :-
        '/' = string::frontChar(Str),
        '*' = string::frontChar(string::fromPosition(Str, 1)),
        !,
        % skip nested comment
        InnerCommentLength = commentLength(string::fromPosition(Str, 2), Seen + 2),
        % skip rest of this
        Length = commentlength(string::fromPosition(Str, InnerCommentLength - Seen), InnerCommentLength).

    commentlength(Str, Seen) = commentlength(Str1, Seen + 1) :-
        string::frontChar(Str, _, Str1).

class predicates
    search_ch : (char, string, charCount) -> charCount determ.
clauses
    % Escapes of search char are NOT processed!
    search_ch(Ch, Str, N) = N :-
        Ch = string::frontChar(Str),
        !.

    search_ch(Ch, Str, N) = N1 :-
        string::frontChar(Str, _, S1),
        N2 = N + 1,
        N1 = search_ch(Ch, S1, N2).

class predicates
    search_ch_nonescaped : (char Ch, string SearchIn, charCount Seen) -> charCount determ.
clauses
    % Escapes before search CHAR are processed and disregarded
    search_ch_nonescaped(Ch, Str, Seen) = Seen :-
        Ch = string::tryFrontChar(Str),
        !.

    search_ch_nonescaped(Ch, Str, Seen) = search_ch_nonescaped(Ch, string::fromPosition(Str, 2), Seen + 2) :-
        '\\' = string::frontChar(Str),
        _ = string::tryFrontChar(string::fromPosition(Str, 1)),
        !.

    search_ch_nonescaped(Ch, Str, Seen) = search_ch_nonescaped(Ch, string::fromPosition(Str, 1), Seen + 1) :-
        _ = string::tryFrontChar(Str),
        !.

class predicates
    scan_atom : (string) -> charCount determ.
clauses
    scan_atom(S1) = Len1 + 1 :-
        string::frontChar(S1, Ch, S2),
        is_symbchar(Ch),
        if Ch = '@' then
            % If @ is prefix of string then it is not part of an atom
            '\"' <> string::frontChar(S2)
        end if,
        !,
        Len1 = scan_atom(S2).

    scan_atom(_) = 0.

class predicates
    is_symbchar : (char) determ.
clauses
    is_symbchar('+').
    is_symbchar('-').
    is_symbchar('*').
    is_symbchar('/').
    is_symbchar('=').
    is_symbchar(':').
    is_symbchar('.').
    is_symbchar('&').
    is_symbchar('\\').
    is_symbchar('^').
    is_symbchar('<').
    is_symbchar('>').
    is_symbchar('?').
    is_symbchar('@').
    is_symbchar('#').
    is_symbchar('$').

end implement scanner
