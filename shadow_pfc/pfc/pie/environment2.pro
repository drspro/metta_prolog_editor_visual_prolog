% Copyright PDC

namespace pfc\pie

implement environment2
    open pie

facts
    env_fact : (string Name, term Term).

clauses
    new().

clauses
    copyOf(Env) :-
        foreach Env:get_nd(VarName, ValTerm) do
            bind(VarName, ValTerm)
        end foreach.

clauses
    fromString(EnvStr) :-
        LineList = string::split_delimiter(EnvStr, "\n"),
        foreach LineUT in LineList and Line = string::trim(LineUT) and "" <> Line do
            scanner::tokl(0, Line, TokL),
            % Parse tokens
            pieParser::s_term(TokL, _, STerm),
	%		vpiCommondialogs::note("icici", toString( STerm ) ),
%	 		  stdio::write( "icici" , toString( STerm ) , "\n" ),
            % Retrieve variable binding
            if STerm = cmp("=", [var(VarName), ValSTerm]) then
                ValTerm = mk_term(ValSTerm),
                % Assert variable binding
                bind(VarName, ValTerm)
            else
                raiseSyntaxError(0, "Expected V = <value>, got: %, Parsing: %", output::stermToString(display, STerm), Line)
            end if
        end foreach.

clauses
    toString() = outputStream_string::getString(write).

clauses
    bind(Name, Term) :-
        assert(env_fact(string::toUpperCase(Name), Term)).

clauses
    lookup(Name) = tryLookup(Name) otherwise exception::raise_error("Variable '", Name, "' not bound!").

clauses
    tryLookup(Name) = Term :-
        env_fact(string::toUpperCase(Name), Term),
        !.

clauses
    get_nd(Name, Term) :-
        env_fact(Name, Term).

clauses
    lookUpOrCreate("_") = variable::newAnonymous() :-
        !.
    lookUpOrCreate(Name) = lookUpOrCreate1(string::toUpperCase(Name)).

predicates
    lookUpOrCreate1 : (string Name) -> term Term.
clauses
    lookUpOrCreate1(Name) = Term :-
        env_fact(Name, Term),
        !.

    lookUpOrCreate1(Name) = Var :-
        Var = var(variable::new(Name)),
        assert(env_fact(Name, Var)).

clauses
    isEmpty() :-
        not(env_fact(_, _)),
        !.

clauses
    write(Stream) :-
        foreach env_fact(N, T1) do
            T2 = variable::normalize(T1),
            TermStr =
                if var(_) = T2 then
                    % write free variables as _ to make reading of environment possible
                    "_"
                else
                    output::termToString(display, T2)
                end if,
            Stream:writef("%s = %s.\n", N, TermStr)
        end foreach.

clauses
    mk_term(var(ID)) = lookUpOrCreate(ID).

    mk_term(int(I)) = int(I).

    mk_term(int64(I)) = int64(I).

    mk_term(real(R)) = real(R).

    mk_term(atom(A)) = atom(A).

    mk_term(str(S)) = str(S).

    mk_term(char(C)) = char(C).

    mk_term(object(C)) = object(C).

    mk_term(pointer(C)) = pointer(C).

    mk_term(list(H, T)) = list(mk_term(H), mk_term(T)).

    mk_term(nill) = nill.

    mk_term(cmp(ID, L)) = cmp(Id, mk_term_list(L)).

clauses
    mk_term_list(L) = [ mk_term(T) || T in L ].

end implement environment2
