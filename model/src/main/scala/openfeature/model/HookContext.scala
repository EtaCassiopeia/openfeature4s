package openfeature.model

/** Type-safe key for storing and retrieving values from HookData and HookHints.
  *
  * Avoids unsafe asInstanceOf casts when storing arbitrary values across hook lifecycle stages.
  */
final case class TypedKey[A](name: String)

/** Per-hook mutable state that persists across hook stages within a single evaluation (spec 4.6.1).
  *
  * Unlike HookHints (read-only, shared), HookData is scoped to an individual hook instance.
  * A hook stores state in `before` and retrieves it in `after`, `error`, or `finallyAfter`.
  */
final class HookData {
  private val data = new java.util.concurrent.atomic.AtomicReference(Map.empty[String, Any])

  def set(key: String, value: Any): Unit = {
    data.updateAndGet(_ + (key -> value))
    ()
  }

  def get[A](key: String): Option[A] =
    data.get().get(key).map(_.asInstanceOf[A])

  def getOrElse[A](key: String, default: => A): A =
    get[A](key).getOrElse(default)

  def remove(key: String): Unit = {
    data.updateAndGet(_ - key)
    ()
  }

  def clear(): Unit =
    data.set(Map.empty)

  def set[A](key: TypedKey[A], value: A): Unit = {
    data.updateAndGet(_ + (key.name -> value))
    ()
  }

  def get[A](key: TypedKey[A]): Option[A] =
    data.get().get(key.name).map(_.asInstanceOf[A])

  def getOrElse[A](key: TypedKey[A], default: => A): A =
    get(key).getOrElse(default)

  def remove[A](key: TypedKey[A]): Unit = {
    data.updateAndGet(_ - key.name)
    ()
  }
}

object HookData {
  def empty: HookData = new HookData
}

final case class HookContext(
  flagKey: String,
  flagType: FlagValueType,
  defaultValue: Any,
  evaluationContext: EvaluationContext,
  clientMetadata: ClientMetadata,
  providerMetadata: ProviderMetadata,
  hookData: HookData = HookData.empty
)

final case class HookHints(values: Map[String, Any]) {
  def get[A](key: String): Option[A] =
    values.get(key).map(_.asInstanceOf[A])

  def getOrElse[A](key: String, default: => A): A =
    get[A](key).getOrElse(default)

  def +(entry: (String, Any)): HookHints =
    HookHints(values + entry)

  def ++(other: HookHints): HookHints =
    HookHints(values ++ other.values)

  def get[A](key: TypedKey[A]): Option[A] =
    values.get(key.name).map(_.asInstanceOf[A])

  def getOrElse[A](key: TypedKey[A], default: => A): A =
    get(key).getOrElse(default)

  def add[A](key: TypedKey[A], value: A): HookHints =
    HookHints(values + (key.name -> value))
}

object HookHints {
  val empty: HookHints = HookHints(Map.empty[String, Any])

  def apply(entries: (String, Any)*): HookHints =
    HookHints(entries.toMap)
}
