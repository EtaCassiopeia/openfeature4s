package openfeature.zio

import java.time.Instant
import zio._
import openfeature.model.{EvaluationContext, FlagResolution, FlagEvaluation, TransactionResult}

object FlagEvaluationOps {
  def evaluated[A](key: String, resolution: FlagResolution[A]): UIO[FlagEvaluation[A]] =
    Clock.instant.map { now =>
      FlagEvaluation(key, resolution.value, resolution, wasOverridden = false, now)
    }

  def overridden[A](key: String, value: A): UIO[FlagEvaluation[A]] =
    Clock.instant.map { now =>
      FlagEvaluation(
        key = key,
        value = value,
        resolution = FlagResolution.cached(key, value),
        wasOverridden = true,
        timestamp = now
      )
    }
}

final private[zio] case class TransactionState(
  overrides: Map[String, Any],
  evaluated: Ref[Map[String, FlagEvaluation[_]]],
  context: EvaluationContext,
  cacheEvaluations: Boolean
) {
  def record[A](evaluation: FlagEvaluation[A]): UIO[Unit] =
    evaluated.update(_ + (evaluation.key -> evaluation))

  def getOverride(key: String): Option[Any] =
    overrides.get(key)

  def getCachedEvaluation(key: String): UIO[Option[FlagEvaluation[_]]] =
    if (cacheEvaluations) evaluated.get.map(_.get(key))
    else ZIO.none

  def getEvaluations: UIO[Map[String, FlagEvaluation[_]]] =
    evaluated.get

  def toResult[A](result: A): UIO[TransactionResult[A]] =
    evaluated.get.map { evals =>
      TransactionResult(
        result = result,
        evaluatedFlags = evals,
        overriddenFlags = evals.filter(_._2.wasOverridden).keySet
      )
    }
}

private[zio] object TransactionState {
  def make(
    overrides: Map[String, Any],
    context: EvaluationContext,
    cacheEvaluations: Boolean = true
  ): UIO[TransactionState] =
    Ref.make(Map.empty[String, FlagEvaluation[_]]).map { evaluated =>
      TransactionState(overrides, evaluated, context, cacheEvaluations)
    }
}
