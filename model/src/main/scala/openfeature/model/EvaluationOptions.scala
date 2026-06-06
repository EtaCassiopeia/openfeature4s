package openfeature.model

import java.time.Duration

/** Options for flag evaluation, including invocation-level hook hints and timeout.
  *
  * Hooks themselves are effect-system specific and are handled by each backend module (openfeature4s-zio,
  * openfeature4s-cats, etc.) rather than at the model level.
  *
  * @param hookHints
  *   Read-only hints passed to hooks during evaluation
  * @param timeout
  *   Maximum duration for this evaluation. Overrides the global evaluation timeout. `None` means use the global default
  *   (which itself defaults to no timeout).
  */
final case class EvaluationOptions(
  hookHints: HookHints = HookHints.empty,
  timeout: Option[Duration] = None
) {
  def withHint(key: String, value: Any): EvaluationOptions =
    copy(hookHints = hookHints + (key -> value))

  def withTimeout(duration: Duration): EvaluationOptions =
    copy(timeout = Some(duration))
}

object EvaluationOptions {
  val empty: EvaluationOptions = EvaluationOptions()
}
