package openfeature.cats

import openfeature.model.FeatureFlagError

/** Wraps a typed [[FeatureFlagError]] as a JVM exception for surfacing through `MonadThrow`.
  *
  * Cats Effect uses a single-type-param `F[A]` — there is no typed error channel. Errors from flag evaluation surface
  * via `MonadThrow[F].raiseError`. Callers who want typed errors can recover:
  * {{{
  * flags.boolean(key, default).attemptNarrow[FeatureFlagException].map(_.map(_.error))
  * }}}
  */
final class FeatureFlagException(val error: FeatureFlagError)
    extends RuntimeException(error.message, error.cause.orNull)
