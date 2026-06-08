package openfeature.cats

import openfeature.model.*
import cats.Applicative

/** Lifecycle extension point for flag evaluation, analogous to `openfeature.zio.FeatureHook` but returning `F[_]`
  * effects instead of `UIO`.
  *
  * Hook execution order (mirrors the OpenFeature spec §4.4.1):
  *   - `before`: API hooks → invocation hooks, in registration order
  *   - `after` / `error` / `finallyAfter`: reverse order (invocation → API)
  *
  * Default implementations are no-ops so implementors only override the stages they need.
  */
trait FeatureHook[F[_]]:

  /** Flag value types this hook should be invoked for. Defaults to all types. */
  def supportedFlagTypes: Set[FlagValueType] = FlagValueType.allTypes

  /** Called before flag evaluation. May return an updated context and hints.
    *
    * Return `None` to leave the context and hints unchanged.
    */
  def before(ctx: HookContext, hints: HookHints)(using F: Applicative[F]): F[Option[(EvaluationContext, HookHints)]] =
    F.pure(None)

  /** Called after a successful flag evaluation. */
  def after[A](ctx: HookContext, details: FlagResolution[A], hints: HookHints)(using F: Applicative[F]): F[Unit] =
    F.unit

  /** Called when flag evaluation fails. */
  def error(ctx: HookContext, err: FeatureFlagError, hints: HookHints)(using F: Applicative[F]): F[Unit] =
    F.unit

  /** Called unconditionally after evaluation (success or failure). */
  def finallyAfter(ctx: HookContext, details: Option[FlagResolution[?]], hints: HookHints)(using
    F: Applicative[F]
  ): F[Unit] =
    F.unit

object FeatureHook:
  def noop[F[_]]: FeatureHook[F] = new FeatureHook[F] {}
