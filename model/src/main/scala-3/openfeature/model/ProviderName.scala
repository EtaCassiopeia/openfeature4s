package openfeature.model

opaque type ProviderName = String

object ProviderName:
  def apply(name: String): ProviderName         = name
  extension (n: ProviderName) def value: String = n
