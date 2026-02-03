

;  Takes a function F and one to three lists Returns the result of applying F to each item in the list(s). 
; The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).
; F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).
; The applications of F to the list items are processed in parallel. Because of the overhead of this approach, a speedup is only likely if F is expensive to evaluate.
; Use maplist! for a single-threaded, deterministic version.
; See Prolog's built-in predicate concurrent-maplist.


(@doc concurrent-maplist!
  (@desc "Takes a function F and one to three lists Returns the result of applying F to each item in the list(s). ")
  (@params (
    (@param "Function to be applied")
    (@param "List")))
  (@return "Result of applying Function to List(s)"))
