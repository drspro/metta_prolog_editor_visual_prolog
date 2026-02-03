


(iz coerce MeTTaLog)
(@doc coerce
  (@desc "Cast (coerce) Value to be of Type. Supports the basic types Atom (no-op), Number, String, and Bool.
  Number: Converts number strings and bools to numbers. True is 1, False is 0.
  String: Coerced as if Value were printed.
  Bool: False, 0, and () are False, all other values are True.")
  (@params (
    (@param "Type")
    (@param "Value")))
  (@return "Value cast to Type"))
(: coerce (-> Type Atom Atom))

