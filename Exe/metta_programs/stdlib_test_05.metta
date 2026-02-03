
(iz setup-call-cleanup! MeTTaLog)
(@doc setup-call-cleanup!
  (@desc "Executes Setup, then Form, then finally Cleanup. Setup is protected from interrupts (e.g. max-time!). In most uses, Setup will perform temporary side-effects required by Form that are finally undone by Cleanup. Cleanup is run even if Form raises an exception. For each result of Setup, Form is run to completion, then Cleanup is run.")
  (@params (
    (@param "Setup")
    (@param "Form")
    (@param "Cleanup")))
  (@return "Result of Form."))
(: setup-call-cleanup! (-> Atom Atom Atom Atom))

(iz predicate-arity MeTTaLog)
(@doc predicate-arity
  (@desc "Returns the arity of Function's predicate form, i.e. the function-arity + 1. (The additional argument being the function's result as an argument to the predicate.)")
  (@params (
    (@param "Function")))
  (@return "Arity of Function's predicate form."))

(iz function-arity MeTTaLog)
(@doc function-arity
  (@desc "Returns the arity of Function.")
  (@params (
    (@param "Function")))
  (@return "Arity of Function."))

(iz open! MeTTaLog)
(@doc open!
  (@desc "Opens Filename as a stream under Mode. Mode is one of read, write, append, or update. Mode append opens the file for writing, positioning the file pointer at the end. Mode update opens the file for writing, positioning the file pointer at the beginning of the file without truncating the file.")
  (@params (
    (@param "Filename")
    (@param "Mode")))
  (@return "Stream"))
(: open! (-> String Atom Stream))

(iz close! MeTTaLog)
(@doc close!
  (@desc "Closes Steam, returning True on success.")
  (@params (
    (@param "Stream")))
  (@return "Boolean"))
(: close! (-> Stream Boolean))

(iz with-output-to! MeTTaLog)
(@doc with-output-to!
  (@desc "Evaluates expression with all outupt (e.g. from print) redirected to Stream. See also open! and close!.")
  (@params (
    (@param "Stream")
    (@param "Expression")))
  (@return "Result of Expression"))
(: with-output-to! (-> Stream Expression Atom))

(iz load-file! MeTTaLog)
(@doc load-file!
  (@desc "Loads the contents of Filename into Space, returning () if successful. Can load Python code (.py) or MeTTa (.metta)% if ambiguous, assumes MeTTa. Like import! but favors MeTTa over Python when the file type is ambiguous.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: load-file! (-> hyperon::space::DynSpace String Expression))

(iz load-ascii MeTTa)
(@doc load-ascii
  (@desc "Loads the contents of Filename into Space, returning () if successful. Assumes the file is an ASCII file. Works like include!.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: load-ascii (-> hyperon::space::DynSpace String Expression))

(iz transfer! MeTTa)
(@doc transfer!
  (@desc "Loads the contents of Filename into &self, as include. Returns () if successful, throws an exception otherwise.")
  (@params (
    (@param "Filename")))
  (@return "Expression"))
(: transfer! (-> String (->)))

(iz save-space! MeTTa)
(@doc save-space!
  (@desc "Writes the contents of Space into Filename, returning () if successful.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: save-space! (-> hyperon::space::DynSpace String Expression))

(iz rtrace! MeTTaLog)
(@doc rtrace!
  (@desc "Fully evaluates input Atom, providing a complete trace of the evaluation.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "Result of evaluation"))
(: rtrace! (-> Atom Atom))

(iz rust MeTTaLog)
(@doc rust
  (@desc "Interface with the rust / Hyperon MeTTa implementation. Enters Atom into rust atomspace. If Atom is evaluated (i.e. by being of the form !<atom>), returns the result of evaluation. See also rust!.")
  (@params (
    (@param "Atom to be entered into the space")))
  (@return "Result of entering Atom into the space"))
(: rust (-> Atom Atom))

(iz rust! MeTTaLog)
(@doc rust!
  (@desc "Like rust but evaluates the atom rather than entering into the space. (rust! <atom>) and (rust !<atom>) are identical.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "Result of evaluation"))
(: rust! (-> Atom Atom))

(iz coerce MeTTaLog)
(@doc coerce
  (@desc "Cast (coerce) Value to be of Type. Supports the basic types Atom (no-op), Number, String, and Bool.  Number: Converts number strings and bools to numbers. True is 1, False is 0.  String: Coerced as if Value were printed.  Bool: False, 0, and () are False, all other values are True.")
  (@params (
    (@param "Type")
    (@param "Value")))
  (@return "Value cast to Type"))
(: coerce (-> Type Atom Atom))
