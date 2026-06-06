package openfeature.model

final class FlagKey private (val value: String) extends AnyVal {
  override def toString: String = value
}

object FlagKey {
  def apply(key: String): FlagKey           = new FlagKey(key)
  implicit val ordering: Ordering[FlagKey]  = Ordering.by(_.value)
}
