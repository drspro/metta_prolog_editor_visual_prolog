% Copyright PDC

namespace pfc\pie

class stdPredicate
    open core

predicates
    isStdPredicate : (string Name, positive Arity) -> pie::invokePredicate Invoker determ.
    isStdFunction : (string Name, positive Arity) -> pie::invokeFunction Invoker determ.

predicates
    all : () -> string* Predicates.

predicates
    getPrototype : (string Preicate) -> string Prototype.

end class stdPredicate
