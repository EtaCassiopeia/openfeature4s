package openfeature.model

opaque type FlagKey = String

object FlagKey:
  def apply(key: String): FlagKey          = key
  given Ordering[FlagKey]                  = Ordering.String.asInstanceOf[Ordering[FlagKey]]
  extension (k: FlagKey) def value: String = k
