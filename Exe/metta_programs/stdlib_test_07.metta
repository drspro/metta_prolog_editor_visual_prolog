(iz load-file! MeTTaLog)
(@doc load-file!
  (@desc "Loads the contents of Filename into Space, returning () if successful. Can load Python code (.py) or MeTTa (.metta)% if ambiguous, assumes MeTTa. Like import! but favors MeTTa over Python when the file type is ambiguous.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: load-file! (-> hyperon::space::DynSpace String Expression))
