% Copyright PDC

namespace pfc\pie

interface environment2
    open pie

predicates
    bind : (string Name, term Term).
    % @short Bind the variable with #Name to #Term.
    % @end

predicates
    lookup : (string Name) -> term Term.
    % @short Get the #Term that #Name is bound to, raise an exception otherwise.
    % @end

predicates
    tryLookup : (string Name) -> term determ.
    % @short Get the #Term that #Name is bound to, fail otherwise.
    % @end

predicates
    lookUpOrCreate : (string Name) -> term Term.
    % @short Get the #Term that #Name is bound to, bind it to a free variable otherwise.
    % @end

predicates
    get_nd : (string Name [out], term Term [out]) nondeterm.
    % @short Enumerate all #Name/#Term pairs in the environment.
    % @end

predicates
    isEmpty : () determ.
    % @short Succeed if the environment is empty.
    % @end

predicates
    write : (outputStream Stream).
    % @short Write the enrironment to #Stream
    % @end

predicates
    toString : () -> string String.
    % @short Write the enrironment to #String
    % @end

predicates
    mk_term : (sterm STerm) -> term Term.
    mk_term_list : (sterml STermList) -> terml TermList.
    % @short Convert syntactic terms to semantic terms using this environment.
    % @end

end interface environment2
