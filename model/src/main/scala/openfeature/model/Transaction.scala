package openfeature.model

import java.time.Instant

/** Result of a single flag evaluation within or outside a transaction. */
final case class FlagEvaluation[+A](
  key: String,
  value: A,
  resolution: FlagResolution[A],
  wasOverridden: Boolean,
  timestamp: Instant
) {
  def wasEvaluated: Boolean = !wasOverridden
}

/** Aggregate result of a completed transaction. */
final case class TransactionResult[+A](
  result: A,
  evaluatedFlags: Map[String, FlagEvaluation[_]],
  overriddenFlags: Set[String]
) {
  def allFlagKeys: Set[String] = evaluatedFlags.keySet

  def providerEvaluatedKeys: Set[String] = evaluatedFlags.keySet -- overriddenFlags

  def flagCount: Int = evaluatedFlags.size

  def overrideCount: Int = overriddenFlags.size

  def getEvaluation(key: String): Option[FlagEvaluation[_]] = evaluatedFlags.get(key)

  def wasEvaluated(key: String): Boolean = evaluatedFlags.contains(key)

  def wasOverridden(key: String): Boolean = overriddenFlags.contains(key)

  def map[B](f: A => B): TransactionResult[B] =
    copy(result = f(result))

  def toValueMap: Map[String, Any] =
    evaluatedFlags.view.mapValues(_.value).toMap
}

object TransactionResult {
  def empty[A](result: A): TransactionResult[A] =
    TransactionResult(result, Map.empty, Set.empty)
}
