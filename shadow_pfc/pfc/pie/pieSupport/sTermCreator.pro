% Copyright PDC

namespace pfc\pie

implement sTermCreator
    open pie

facts
    nextVarNum : integer := 0.
    variable_fact : (variable TermVar, sTerm STermVar).

clauses
    mk_sTerm(var(TermVar)) = mk_var(TermVar:getTerm()).

    mk_sTerm(cmp(Sym, TermList)) = cmp(Sym, mk_sTermList(TermList)).

    mk_sTerm(list(T1, T2)) = list(mk_sTerm(T1), mk_sTerm(T2)).

    mk_sTerm(nill) = nill.

    mk_sTerm(atom(Sym)) = atom(Sym).

    mk_sTerm(int(Val)) = int(Val).

    mk_sTerm(int64(Val)) = int64(Val).

    mk_sTerm(real(Val)) = real(Val).

    mk_sTerm(str(Val)) = str(Val).

    mk_sTerm(char(Val)) = char(Val).

    mk_sTerm(object(Val)) = object(Val).

    mk_sTerm(pointer(Val)) = pointer(Val).

predicates
    mk_sTermList : (term* TermList) -> sterm* STermList.
clauses
    mk_sTermList(TermList) = list::map(TermList, mk_sTerm).

predicates
    mk_var : (term Normalized) -> sterm STermVar.
clauses
    mk_var(var(TermVar)) = mk_var_var(TermVar) :-
        !.
    mk_var(Term) = mk_sTerm(Term).

predicates
    mk_var_var : (variable TermVar) -> sterm STermVar.
clauses
    mk_var_var(TermVar) = STermVar :-
        variable_fact(TermVar, STermVar),
        !.

    mk_var_var(TermVar) = STermVar :-
        Name = string::format("V$%d", nextVarNum),
        nextVarNum := nextVarNum + 1,
        STermVar = var(Name),
        assert(variable_fact(TermVar, STermVar)).

clauses
    toSTerm(Term) = VG:mk_sTerm(Term) :-
        VG = sTermCreator::new().

end implement sTermCreator
