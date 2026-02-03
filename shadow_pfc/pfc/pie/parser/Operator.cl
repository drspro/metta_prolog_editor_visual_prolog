% Copyright PDC

namespace pfc\pie

class operator

domains
    op = string.
    % storing of operators
    xfy = string.
    % xfy; yfx; xfx; yfy; fx; fy; xf; yf
    prior = integer.
    % priority of operators

predicates
    getop : (prior, xfy, op) determ (o,o,i) (i,o,i).

predicates
    getop_nd : (prior [out], xfy [out], op [out]) nondeterm.

predicates
    isPrefixOp : (op FID, prior NEWPRIOR [out], xfy XFY [out]) determ.

predicates
    setop : (prior, xfy, op).

end class operator
