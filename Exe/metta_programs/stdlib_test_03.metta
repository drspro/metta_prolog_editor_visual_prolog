
(iz setup-call-cleanup! MeTTaLog)
(@doc setup-call-cleanup!
  (@desc "Executes Setup, then Form, then finally Cleanup. Setup is protected from interrupts (e.g. max-time!). In most uses, Setup will perform temporary side-effects required by Form that are finally undone by Cleanup. Cleanup is run even if Form raises an exception. For each result of Setup, Form is run to completion, then Cleanup is run.")
  (@params (
    (@param "Setup")
    (@param "Form")
    (@param "Cleanup")))
  (@return "Result of Form."))
(: setup-call-cleanup! (-> Atom Atom Atom Atom))
