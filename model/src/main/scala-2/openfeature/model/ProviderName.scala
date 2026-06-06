package openfeature.model

final class ProviderName private (val value: String) extends AnyVal {
  override def toString: String = value
}

object ProviderName {
  def apply(name: String): ProviderName = new ProviderName(name)
}
