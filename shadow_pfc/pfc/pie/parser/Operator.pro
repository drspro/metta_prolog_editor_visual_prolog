% Copyright PDC

namespace pfc\pie

implement operator

class facts - operators
    op : (prior, xfy, op).

clauses
    op(1200, "xfx", ":-").
    op(1200, "fx", "?-").
    op(1100, "xfy", ";").
    op(1000, "xfy", ",").
    op(900, "fy", "not").
    op(700, "xfx", "=").
    op(700, "xfx", ":=").
    op(700, "xfx", @"\=").
    op(700, "xfx", "is").
    op(700, "xfx", "<").
    op(700, "xfx", "=<").
    op(700, "xfx", ">").
    op(700, "xfx", ">=").
    op(700, "xfx", "==").
    op(700, "xfx", @"\==").
    op(700, "xfx", "=..").
    op(700, "xfx", "<>").
    op(700, "xfx", "><").
    op(500, "yfx", "+").
    op(500, "yfx", "-").
    op(400, "yfx", "*").
    op(400, "yfx", "/").
    op(400, "yfx", "div").
    op(400, "yfx", "mod").
    op(200, "fx", "+").
    op(200, "fx", "-").

clauses
    getOp(PRIOR, XFY, OP) :-
        op(PRIOR, XFY, OP),
        !.

clauses
    getOp_nd(PRIOR, XFY, OP) :-
        op(PRIOR, XFY, OP).

clauses
    isPrefixOp(FID, NEWPRIOR, XFY) :-
        op(NEWPRIOR, XFY, FID),
        ("fx" = XFY or "fy" = XFY),
        !.

clauses
    setOp(PRIOR, XFY, OP) :-
        retractall(op(_, _, OP)),
        assert(op(PRIOR, XFY, OP)).

end implement operator
