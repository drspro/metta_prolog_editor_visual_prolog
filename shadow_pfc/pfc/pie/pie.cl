% Copyright PDC

namespace pfc\pie

class pie : pie
    open core

domains

gen_terms = pie::sterml.


constructors
    new : ().
    new : (outputStream OutputStream).

constants
    pie_exception : exception = exception(class_name(), constant_name(), "exception from PIE").
    % @short Exception during PIE evaluation.
    % @end

constants
    syntaxError_exception : exception = exception(class_name(), constant_name(), "PIE syntax error").
    % @short Exception during PIE evaluation.
    % @end

predicates
    raiseSyntaxError : (charCount Cursor, string Format [formatString], ...) erroneous [programPoint].
    % @short Raise a syntaxError_exception. #Cursor indicates where the syntax error occurred.
    % @end

constants
    cursor_parameter = "cursor".
    % @short paramater in syntaxError_exception that indicats where the syntax error was detected (charCount).
    % @end

predicates
    normalizeVar : (term Term) -> term Normalized.
    % @short If #Term is a bound variable #Normalized becomes the term it is bound to.
    % @end

predicates
    boundTerm : (term Term) determ.
    freeTerm : (term Term) determ.
    % @short Determine wheter #Term is bound/free.
    % @end

predicates
    tryGroundTerm : (term Term) -> term Grounded determ.
    tryGroundTermList : (term* TermL) -> term* GroundedL determ.
    % @short If all variables in the term are bound then return #Term without variables in.
    % @end

predicates
    presenter_termBase : presenter::presenter{termBase{VarRep}}.


end class pie
