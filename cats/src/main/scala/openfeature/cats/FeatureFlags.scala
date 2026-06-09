package openfeature.cats

import openfeature.model.*
import cats.effect.{Async, Deferred, IO, IOLocal, Resource}
import cats.effect.kernel.{MonadCancel, Ref}
import cats.syntax.all.*
import fs2.Stream

/** Cats Effect 3 / FS2 client for OpenFeature flag evaluation.
  *
  * ==Error model==
  * There is no typed error channel in `F[A]`. Evaluation errors are raised via `MonadThrow[F].raiseError` as
  * [[FeatureFlagException]]. Callers wanting typed errors can recover:
  * {{{
  * flags.boolean(key, default).attemptNarrow[FeatureFlagException].map(_.map(_.error))
  * }}}
  *
  * ==Context hierarchy==
  * Three levels of context are merged at evaluation time (later overrides earlier):
  *   1. API-level global context — set via `setGlobalContext` 2. Fiber-local context — set via `withContext` (uses
  *      `IOLocal` when `F = IO`) 3. Per-call context — passed directly to each evaluation method
  *
  * ==Thread safety==
  * All state (`Ref`, `IOLocal`, `Topic`) is safe for concurrent access.
  */
trait FeatureFlags[F[_]]:

  // Detailed evaluation — returns full [[FlagResolution]] including metadata, reason, and variant

  def booleanDetails(
    key: FlagKey,
    default: Boolean,
    ctx: EvaluationContext = EvaluationContext.empty
  ): F[FlagResolution[Boolean]]

  def stringDetails(
    key: FlagKey,
    default: String,
    ctx: EvaluationContext = EvaluationContext.empty
  ): F[FlagResolution[String]]

  def intDetails(
    key: FlagKey,
    default: Int,
    ctx: EvaluationContext = EvaluationContext.empty
  ): F[FlagResolution[Int]]

  def longDetails(
    key: FlagKey,
    default: Long,
    ctx: EvaluationContext = EvaluationContext.empty
  ): F[FlagResolution[Long]]

  def doubleDetails(
    key: FlagKey,
    default: Double,
    ctx: EvaluationContext = EvaluationContext.empty
  ): F[FlagResolution[Double]]

  def valueDetails[A: FlagType](
    key: FlagKey,
    default: A,
    ctx: EvaluationContext = EvaluationContext.empty
  ): F[FlagResolution[A]]

  // Simple evaluation — returns only the resolved value

  def boolean(key: FlagKey, default: Boolean): F[Boolean]
  def string(key: FlagKey, default: String): F[String]
  def int(key: FlagKey, default: Int): F[Int]
  def long(key: FlagKey, default: Long): F[Long]
  def double(key: FlagKey, default: Double): F[Double]
  def value[A: FlagType](key: FlagKey, default: A): F[A]

  // Context management

  /** Replace the API-level global context. Affects all subsequent evaluations. */
  def setGlobalContext(ctx: EvaluationContext): F[Unit]

  /** Read the current API-level global context. */
  def globalContext: F[EvaluationContext]

  /** Run `fa` with `ctx` merged into the evaluation context for the duration of `fa`.
    *
    * When `F = IO` the context is fiber-local (two concurrent fibers see independent contexts). For other `F`, the
    * implementation falls back to a bracket on a shared `Ref` which is safe for sequential but not concurrent callers.
    * Use [[FeatureFlags.makeIO]] for guaranteed fiber isolation.
    */
  def withContext[A](ctx: EvaluationContext)(fa: F[A]): F[A]

  // Hooks

  def addHook(hook: FeatureHook[F]): F[Unit]
  def addHooks(hooks: List[FeatureHook[F]]): F[Unit]

  // Events and status

  /** Stream of provider lifecycle events. Completes when the resource is released. */
  def events: Stream[F, ProviderEvent]

  def providerStatus: F[ProviderStatus]

object FeatureFlags:

  /** Create a `FeatureFlags[F]` backed by the given Java SDK provider.
    *
    * `withContext` uses a `Ref`-based bracket — safe for sequential use. For true fiber isolation (two concurrent
    * fibers maintain independent evaluation contexts) use [[makeIO]].
    */
  def make[F[_]](
    provider: dev.openfeature.sdk.FeatureProvider,
    domain: Option[String] = None
  )(using F: Async[F]): Resource[F, FeatureFlags[F]] =
    FeatureFlagsLive.make[F](
      provider,
      localCtxProvider = Ref.of[F, EvaluationContext](EvaluationContext.empty).map(ContextProvider.fromRef(_)),
      domain = domain
    )

  /** Create a `FeatureFlags[F]` for a provider that starts in `NotReady` state.
    *
    * Uses `setProvider` (non-blocking), so resource acquisition completes before the provider is ready. The optional
    * `onReady` deferred is completed when the Java SDK fires `PROVIDER_READY`, which lets a `TestFeatureProvider`
    * synchronize `setStatus(Ready)` with SDK event propagation.
    */
  def makeAsync[F[_]](
    provider: dev.openfeature.sdk.FeatureProvider,
    onReady: Option[Deferred[F, Unit]] = None,
    domain: Option[String] = None
  )(using F: Async[F]): Resource[F, FeatureFlags[F]] =
    FeatureFlagsLive.makeAsync[F](
      provider,
      onReady,
      localCtxProvider = Ref.of[F, EvaluationContext](EvaluationContext.empty).map(ContextProvider.fromRef(_)),
      domain = domain
    )

  /** Create a `FeatureFlags[IO]` with fiber-isolated `withContext` via `IOLocal`.
    *
    * Prefer this over [[make]] when two concurrent fibers must maintain independent evaluation contexts.
    */
  def makeIO(
    provider: dev.openfeature.sdk.FeatureProvider,
    domain: Option[String] = None
  ): Resource[IO, FeatureFlags[IO]] =
    Resource
      .eval(IOLocal(EvaluationContext.empty))
      .flatMap(local =>
        FeatureFlagsLive.make[IO](
          provider,
          localCtxProvider = IO.pure(ContextProvider.fromIOLocal(local)),
          domain = domain
        )
      )
