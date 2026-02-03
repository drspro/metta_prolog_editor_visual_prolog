% Copyright PDC

namespace pfc\pie

class environment2 : environment2

constructors
    new : ().
    % @short Create an empty environment.
    % @end

constructors
    copyOf : (environment2).
    % @short Creates a copy of an existing environment. Bound variables and values are copied.
    % @end

constructors
    fromString : (string Environment).
    % @short Create an environment from #Environment, with form "<name1>=<term1>\n<name2>=<term2>\n...<nameN>=<termN>".
    % @end

end class environment2
