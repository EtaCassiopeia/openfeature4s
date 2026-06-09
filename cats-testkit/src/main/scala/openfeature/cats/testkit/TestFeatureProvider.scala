package openfeature.cats.testkit

import openfeature.cats.FeatureFlags
import openfeature.model.*
import openfeature.bridge.ErrorCodeConverter
import cats.effect.{Async, Deferred, Resource}
import cats.effect.kernel.Ref
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Topic
import dev.openfeature.sdk.{
  EvaluationContext as OFEvaluationContext,
  EventProvider,
  Metadata,
  OpenFeatureAPI,
  ProviderEvaluation,
  ProviderEventDetails,
  ProviderState,
  Value
}
import dev.openfeature.sdk.exceptions.*

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList, CountDownLatch}
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/** Cats Effect analog of the ZIO testkit's `TestFeatureProvider`.
  *
  * Implements the Java SDK's `EventProvider` so it can be plugged directly into the OpenFeature Java SDK (via
  * `FeatureFlags.make`). The Scala control surface returns `F[_]` effects.
  */
final class TestFeatureProvider[F[_]] private (
  private val flags: ConcurrentHashMap[String, Any],
  private val javaState: AtomicReference[ProviderState],
  private val evaluations: CopyOnWriteArrayList[(String, OFEvaluationContext)],
  private val topic: Topic[F, Option[ProviderEvent]],
  private val statusRef: Ref[F, ProviderStatus],
  private val initLatch: Option[CountDownLatch],
  private val readySignal: Option[Deferred[F, Unit]],
  private val behaviorRef: AtomicReference[TestFeatureProvider.BehaviorConfig],
  private val dispatcher: Dispatcher[F]
)(using F: Async[F])
    extends EventProvider:

  import TestFeatureProvider.{BehaviorConfig, ErrorMode}

  private def applyBehavior(): Unit =
    val cfg = behaviorRef.get()
    cfg.delay.foreach(d => Thread.sleep(d.toMillis))
    cfg.errorMode.foreach:
      case ErrorMode.FlagNotFound     => throw new FlagNotFoundError("Simulated: flag not found")
      case ErrorMode.ParseError       => throw new ParseError("Simulated: parse error")
      case ErrorMode.TypeMismatch     => throw new TypeMismatchError("Simulated: type mismatch")
      case ErrorMode.ProviderNotReady => throw new ProviderNotReadyError("Simulated: provider not ready")
      case ErrorMode.General          => throw new GeneralError("Simulated: general error")
    if cfg.failureProbability > 0.0 &&
      java.util.concurrent.ThreadLocalRandom.current().nextDouble() < cfg.failureProbability
    then throw new GeneralError("Simulated: random failure")

  @scala.annotation.nowarn("msg=deprecated")
  override def getMetadata: Metadata = new Metadata:
    override def getName: String = "TestFeatureProvider"

  override def getState: ProviderState = javaState.get()

  override def initialize(ctx: OFEvaluationContext): Unit =
    initLatch.foreach(_.await())
    javaState.set(ProviderState.READY)

  override def shutdown(): Unit =
    initLatch.foreach(_.countDown())
    // Unblock any fiber suspended in setStatus(Ready) → awaitReady so it is not leaked on teardown.
    readySignal.foreach(sig => dispatcher.unsafeRunAndForget(sig.complete(()).void))
    javaState.set(ProviderState.NOT_READY)

  private def evalResult[A](
    key: String,
    ctx: OFEvaluationContext,
    default: A,
    convert: Any => A
  ): ProviderEvaluation[A] =
    evaluations.add((key, ctx)) // record attempt before any injected failure
    applyBehavior()
    val value = Option(flags.get(key)).fold(default)(convert)
    ProviderEvaluation
      .builder[A]()
      .value(value)
      .reason(if flags.containsKey(key) then "TARGETING_MATCH" else "DEFAULT")
      .build()

  override def getBooleanEvaluation(
    key: String,
    default: java.lang.Boolean,
    ctx: OFEvaluationContext
  ): ProviderEvaluation[java.lang.Boolean] =
    evalResult(key, ctx, default, _.asInstanceOf[Boolean])

  override def getStringEvaluation(key: String, default: String, ctx: OFEvaluationContext): ProviderEvaluation[String] =
    evalResult(key, ctx, default, _.toString)

  override def getIntegerEvaluation(
    key: String,
    default: java.lang.Integer,
    ctx: OFEvaluationContext
  ): ProviderEvaluation[java.lang.Integer] =
    evalResult(key, ctx, default, { case i: Int => i; case n: Number => n.intValue() })

  override def getDoubleEvaluation(
    key: String,
    default: java.lang.Double,
    ctx: OFEvaluationContext
  ): ProviderEvaluation[java.lang.Double] =
    evalResult(key, ctx, default, { case d: Double => d; case n: Number => n.doubleValue() })

  override def getObjectEvaluation(key: String, default: Value, ctx: OFEvaluationContext): ProviderEvaluation[Value] =
    evalResult(key, ctx, default, v => new Value(v.toString))

  // Flag management

  def setFlag[A](key: FlagKey, value: A): F[Unit] = F.delay(flags.put(key.value, value)).void
  def setFlags(newFlags: Map[FlagKey, Any]): F[Unit] = F.delay {
    flags.clear(); newFlags.foreach { case (k, v) => flags.put(k.value, v) }
  }
  def removeFlag(key: FlagKey): F[Unit] = F.delay(flags.remove(key.value)).void
  def clearFlags: F[Unit]               = F.delay(flags.clear())

  // Lifecycle

  def setStatus(status: ProviderStatus): F[Unit] =
    val updateJavaState: F[Unit] = F.delay {
      // Always release initLatch — countDown on a zero latch is a no-op.
      // Without this, setStatus(Error) before Ready leaves initialize() blocked.
      initLatch.foreach(_.countDown())
      status match
        case ProviderStatus.Ready        => javaState.set(ProviderState.READY)
        case ProviderStatus.NotReady     => javaState.set(ProviderState.NOT_READY)
        case ProviderStatus.Error        => javaState.set(ProviderState.ERROR)
        case ProviderStatus.Stale        => javaState.set(ProviderState.STALE)
        case ProviderStatus.Fatal        => javaState.set(ProviderState.FATAL)
        case ProviderStatus.ShuttingDown => javaState.set(ProviderState.NOT_READY)
    }
    val awaitReady: F[Unit] =
      readySignal.filter(_ => status == ProviderStatus.Ready).traverse_(_.get)
    statusRef.set(status) *> updateJavaState *> awaitReady

  def getStatus: F[ProviderStatus] = statusRef.get

  // Evaluation tracking

  def getEvaluations: F[List[(String, OFEvaluationContext)]] = F.delay(evaluations.asScala.toList)
  def clearEvaluations: F[Unit]                              = F.delay(evaluations.clear())
  def wasEvaluated(key: FlagKey): F[Boolean]                 = F.delay(evaluations.asScala.exists(_._1 == key.value))
  def evaluationCount(key: FlagKey): F[Int]                  = F.delay(evaluations.asScala.count(_._1 == key.value))

  // Event streaming

  def events: Stream[F, ProviderEvent] = topic.subscribe(128).unNoneTerminate

  /** Like `events` but uses `subscribeAwait` so the subscription is guaranteed to be registered
    * before the returned `Resource` completes acquisition. Use in tests to avoid the race between
    * subscription and event emission that exists with bare `events` + `IO.sleep`.
    */
  def eventsResource: Resource[F, Stream[F, ProviderEvent]] =
    topic.subscribeAwait(128).map(_.unNoneTerminate)

  def emitEvent(event: ProviderEvent): F[Unit] =
    topic.publish1(Some(event)).flatMap {
      case Left(_) =>
        F.raiseError(new IllegalStateException("[openfeature4s] cannot emit event: provider topic is closed"))
      case Right(()) =>
        F.unit // bridge fully disabled for diagnostic
    }

  // Behavior controls

  def setDelay(d: java.time.Duration): F[Unit] =
    F.delay(behaviorRef.updateAndGet(_.copy(delay = Some(d)))).void

  def clearDelay: F[Unit] =
    F.delay(behaviorRef.updateAndGet(_.copy(delay = None))).void

  def setErrorMode(mode: ErrorMode): F[Unit] =
    F.delay(behaviorRef.updateAndGet(_.copy(errorMode = Some(mode)))).void

  def clearErrorMode: F[Unit] =
    F.delay(behaviorRef.updateAndGet(_.copy(errorMode = None))).void

  def setFailureProbability(p: Double): F[Unit] =
    val clamped = if p.isNaN then 0.0 else p.max(0.0).min(1.0)
    F.delay(behaviorRef.updateAndGet(_.copy(failureProbability = clamped))).void

  def clearBehavior: F[Unit] =
    F.delay(behaviorRef.set(BehaviorConfig())).void

object TestFeatureProvider:

  private[testkit] case class BehaviorConfig(
    delay: Option[java.time.Duration] = None,
    errorMode: Option[ErrorMode] = None,
    failureProbability: Double = 0.0
  ):
    require(
      !failureProbability.isNaN && failureProbability >= 0.0 && failureProbability <= 1.0,
      s"failureProbability must be in [0.0, 1.0], got $failureProbability"
    )

  enum ErrorMode:
    case FlagNotFound, ParseError, TypeMismatch, ProviderNotReady, General

  /** Create a `(TestFeatureProvider, FeatureFlags)` pair backed by a ready provider. */
  def make[F[_]](
    initialFlags: Map[FlagKey, Any] = Map.empty
  )(using F: Async[F]): Resource[F, (TestFeatureProvider[F], FeatureFlags[F])] =
    Dispatcher.parallel[F].flatMap { dispatcher =>
      Resource.eval(buildProvider[F](initialFlags, notReady = false, dispatcher)).flatMap { provider =>
        Resource
          .make(F.pure(provider))(p => p.topic.publish1(None).attempt.void)
          .flatMap(p => FeatureFlags.make[F](p).map(flags => (p, flags)))
      }
    }

  /** Create a `(TestFeatureProvider, FeatureFlags)` pair where the provider starts in `NotReady` state.
    *
    * Uses `setProvider` (non-blocking) so resource acquisition completes before the provider is ready. Call
    * `provider.setStatus(ProviderStatus.Ready)` to simulate the provider becoming ready. `setStatus(Ready)` suspends
    * until the Java SDK fires PROVIDER_READY and both `statusRef` and the event topic have been updated.
    */
  def makeAsync[F[_]](
    initialFlags: Map[FlagKey, Any] = Map.empty
  )(using F: Async[F]): Resource[F, (TestFeatureProvider[F], FeatureFlags[F])] =
    Dispatcher.parallel[F].flatMap { dispatcher =>
      Resource.eval(buildProvider[F](initialFlags, notReady = true, dispatcher)).flatMap { provider =>
        Resource
          .make(F.pure(provider))(p => p.topic.publish1(None).attempt.void)
          .flatMap(p => FeatureFlags.makeAsync[F](p, p.readySignal).map(flags => (p, flags)))
      }
    }

  private def buildProvider[F[_]](
    initialFlags: Map[FlagKey, Any],
    notReady: Boolean,
    dispatcher: Dispatcher[F]
  )(using F: Async[F]): F[TestFeatureProvider[F]] =
    for
      topic       <- Topic[F, Option[ProviderEvent]]
      statusRef   <- Ref.of[F, ProviderStatus](if notReady then ProviderStatus.NotReady else ProviderStatus.Ready)
      readySignal <- if notReady then Deferred[F, Unit].map(Some(_)) else F.pure(None)
    yield
      val flags = new ConcurrentHashMap[String, Any]()
      initialFlags.foreach { case (k, v) => flags.put(k.value, v) }
      val javaState =
        new AtomicReference[ProviderState](if notReady then ProviderState.NOT_READY else ProviderState.READY)
      val evaluations = new CopyOnWriteArrayList[(String, OFEvaluationContext)]()
      val initLatch   = if notReady then Some(new CountDownLatch(1)) else None
      val behaviorRef = new AtomicReference[BehaviorConfig](BehaviorConfig())
      new TestFeatureProvider[F](
        flags,
        javaState,
        evaluations,
        topic,
        statusRef,
        initLatch,
        readySignal,
        behaviorRef,
        dispatcher
      )
