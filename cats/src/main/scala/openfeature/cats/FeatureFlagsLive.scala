package openfeature.cats

import openfeature.model.*
import openfeature.bridge.{ClientEvaluator, ContextConverter, ErrorCodeConverter}
import cats.effect.{Async, Deferred, IO, IOLocal, Resource}
import cats.effect.kernel.{MonadCancel, Ref}
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fs2.Stream
import fs2.concurrent.Topic
import dev.openfeature.sdk.{
  Client as OFClient,
  EventDetails,
  FeatureProvider,
  OpenFeatureAPI,
  ProviderEvent as JavaProviderEvent
}

import java.util.UUID
import scala.jdk.CollectionConverters.*

// Internal abstraction for fiber-local vs Ref-based context storage.
private[cats] trait ContextProvider[F[_]]:
  def get: F[EvaluationContext]
  def set(ctx: EvaluationContext): F[Unit]
  def locally[A](ctx: EvaluationContext)(fa: F[A]): F[A]

private[cats] object ContextProvider:
  def fromRef[F[_]](ref: Ref[F, EvaluationContext])(using F: MonadCancel[F, ?]): ContextProvider[F] =
    new ContextProvider[F]:
      def get: F[EvaluationContext]            = ref.get
      def set(ctx: EvaluationContext): F[Unit] = ref.set(ctx)
      def locally[A](ctx: EvaluationContext)(fa: F[A]): F[A] =
        ref.getAndSet(ctx).bracket(_ => fa)(ref.set)

  def fromIOLocal(local: IOLocal[EvaluationContext]): ContextProvider[IO] =
    new ContextProvider[IO]:
      def get: IO[EvaluationContext]            = local.get
      def set(ctx: EvaluationContext): IO[Unit] = local.set(ctx)
      def locally[A](ctx: EvaluationContext)(fa: IO[A]): IO[A] =
        local.getAndSet(ctx).bracket(_ => fa)(local.set)

/** Internal implementation — not part of the public API. */
private[cats] class FeatureFlagsLive[F[_]](
  client: OFClient,
  globalCtxRef: Ref[F, EvaluationContext],
  localCtx: ContextProvider[F],
  hooksRef: Ref[F, List[FeatureHook[F]]],
  topic: Topic[F, Option[ProviderEvent]],
  statusRef: Ref[F, ProviderStatus]
)(using F: Async[F])
    extends FeatureFlags[F]:

  private def resolvedContext(callCtx: EvaluationContext): F[EvaluationContext] =
    (globalCtxRef.get, localCtx.get).mapN(_.merge(_).merge(callCtx))

  private def evaluate[A](
    key: FlagKey,
    default: A,
    callCtx: EvaluationContext,
    typeName: String
  )(using ft: FlagType[A]): F[FlagResolution[A]] =
    for
      ctx <- resolvedContext(callCtx)
      ofCtx = ContextConverter.toOpenFeature(ctx)
      result <- ClientEvaluator.evaluateStandard(typeName, client, key.value, default, ofCtx) match
        case Some(erased) =>
          F.blocking(erased.call())
            .map { details =>
              val value   = erased.extract(details).asInstanceOf[A]
              val reason  = Option(details.getReason).map(resolveReason).getOrElse(ResolutionReason.Unknown)
              val variant = Option(details.getVariant)
              val errCode = Option(details.getErrorCode).map(ErrorCodeConverter.fromJava)
              val errMsg  = Option(details.getErrorMessage)
              FlagResolution(value, variant, reason, FlagMetadata.empty, key.value, errCode, errMsg)
            }
            .recover { case ex =>
              val err = FeatureFlagError.classify(ex)
              FlagResolution.error(key.value, default, FeatureFlagError.toErrorCode(err), err.message)
            }
        case None =>
          F.blocking {
            ft.decode(client.getObjectDetails(key.value, null, ofCtx).getValue) match
              case Right(v)  => FlagResolution(v, None, ResolutionReason.Default, FlagMetadata.empty, key.value)
              case Left(msg) => FlagResolution.error(key.value, default, ErrorCode.TypeMismatch, msg)
          }.recover { case ex =>
            val err = FeatureFlagError.classify(ex)
            FlagResolution.error(key.value, default, FeatureFlagError.toErrorCode(err), err.message)
          }
    yield result

  private def resolveReason(r: String): ResolutionReason = r match
    case "STATIC"          => ResolutionReason.Static
    case "DEFAULT"         => ResolutionReason.Default
    case "TARGETING_MATCH" => ResolutionReason.TargetingMatch
    case "SPLIT"           => ResolutionReason.Split
    case "CACHED"          => ResolutionReason.Cached
    case "DISABLED"        => ResolutionReason.Disabled
    case "STALE"           => ResolutionReason.Stale
    case "ERROR"           => ResolutionReason.Error
    case _                 => ResolutionReason.Unknown

  def booleanDetails(key: FlagKey, default: Boolean, ctx: EvaluationContext): F[FlagResolution[Boolean]] =
    evaluate(key, default, ctx, "Boolean")
  def stringDetails(key: FlagKey, default: String, ctx: EvaluationContext): F[FlagResolution[String]] =
    evaluate(key, default, ctx, "String")
  def intDetails(key: FlagKey, default: Int, ctx: EvaluationContext): F[FlagResolution[Int]] =
    evaluate(key, default, ctx, "Int")
  def longDetails(key: FlagKey, default: Long, ctx: EvaluationContext): F[FlagResolution[Long]] =
    evaluate(key, default, ctx, "Long")
  def doubleDetails(key: FlagKey, default: Double, ctx: EvaluationContext): F[FlagResolution[Double]] =
    evaluate(key, default, ctx, "Double")
  def valueDetails[A: FlagType](key: FlagKey, default: A, ctx: EvaluationContext): F[FlagResolution[A]] =
    evaluate(key, default, ctx, FlagType[A].typeName)

  def boolean(key: FlagKey, default: Boolean): F[Boolean] = booleanDetails(key, default).map(_.value)
  def string(key: FlagKey, default: String): F[String]    = stringDetails(key, default).map(_.value)
  def int(key: FlagKey, default: Int): F[Int]             = intDetails(key, default).map(_.value)
  def long(key: FlagKey, default: Long): F[Long]          = longDetails(key, default).map(_.value)
  def double(key: FlagKey, default: Double): F[Double]    = doubleDetails(key, default).map(_.value)
  def value[A: FlagType](key: FlagKey, default: A): F[A]  = valueDetails(key, default).map(_.value)

  def setGlobalContext(ctx: EvaluationContext): F[Unit]      = globalCtxRef.set(ctx)
  def globalContext: F[EvaluationContext]                    = globalCtxRef.get
  def withContext[A](ctx: EvaluationContext)(fa: F[A]): F[A] = localCtx.locally(ctx)(fa)

  def addHook(hook: FeatureHook[F]): F[Unit]         = hooksRef.update(_ :+ hook)
  def addHooks(hooks: List[FeatureHook[F]]): F[Unit] = hooksRef.update(_ ++ hooks)

  def events: Stream[F, ProviderEvent]  = topic.subscribe(128).unNoneTerminate
  def providerStatus: F[ProviderStatus] = statusRef.get

private[cats] object FeatureFlagsLive:
  def make[F[_]](
    provider: FeatureProvider,
    localCtxProvider: F[ContextProvider[F]]
  )(using F: Async[F]): Resource[F, FeatureFlags[F]] =
    Dispatcher.parallel[F].flatMap { dispatcher =>
      Resource
        .make(
          acquire = for
            globalCtxRef <- Ref.of[F, EvaluationContext](EvaluationContext.empty)
            localCtx     <- localCtxProvider
            hooksRef     <- Ref.of[F, List[FeatureHook[F]]](Nil)
            statusRef    <- Ref.of[F, ProviderStatus](ProviderStatus.NotReady)
            topic        <- Topic[F, Option[ProviderEvent]]
            api          <- F.delay(OpenFeatureAPI.getInstance())
            domain = s"openfeature4s-cats-${UUID.randomUUID()}"
            _ <- F.blocking(api.setProviderAndWait(domain, provider))
            _ <- statusRef.set(ProviderStatus.Ready)
            client = api.getClient(domain)
            _ <- F.delay(registerEventHandlers(client, provider, dispatcher, statusRef, topic))
          yield (new FeatureFlagsLive[F](client, globalCtxRef, localCtx, hooksRef, topic, statusRef), topic)
        )(release = { case (_, topic) =>
          F.delay(provider.shutdown()).attempt.void *>
            topic.publish1(None).attempt.void
        })
        .map(_._1)
    }

  /** Non-blocking variant: uses `setProvider` so resource acquisition completes before the provider reaches READY.
    *
    * `onReady` is completed when the Java SDK fires PROVIDER_READY, allowing callers to synchronize on readiness.
    */
  def makeAsync[F[_]](
    provider: FeatureProvider,
    onReady: Option[Deferred[F, Unit]],
    localCtxProvider: F[ContextProvider[F]]
  )(using F: Async[F]): Resource[F, FeatureFlags[F]] =
    Dispatcher.parallel[F].flatMap { dispatcher =>
      Resource
        .make(
          acquire = for
            globalCtxRef <- Ref.of[F, EvaluationContext](EvaluationContext.empty)
            localCtx     <- localCtxProvider
            hooksRef     <- Ref.of[F, List[FeatureHook[F]]](Nil)
            statusRef    <- Ref.of[F, ProviderStatus](ProviderStatus.NotReady)
            topic        <- Topic[F, Option[ProviderEvent]]
            api          <- F.delay(OpenFeatureAPI.getInstance())
            domain = s"openfeature4s-cats-${UUID.randomUUID()}"
            _ <- F.delay(api.setProvider(domain, provider))
            client = api.getClient(domain)
            _ <- F.delay(registerEventHandlers(client, provider, dispatcher, statusRef, topic, onReady))
          yield (new FeatureFlagsLive[F](client, globalCtxRef, localCtx, hooksRef, topic, statusRef), topic)
        )(release = { case (_, topic) =>
          F.delay(provider.shutdown()).attempt.void *>
            topic.publish1(None).attempt.void
        })
        .map(_._1)
    }

  private def registerEventHandlers[F[_]](
    client: OFClient,
    provider: FeatureProvider,
    dispatcher: Dispatcher[F],
    statusRef: Ref[F, ProviderStatus],
    topic: Topic[F, Option[ProviderEvent]],
    onReady: Option[Deferred[F, Unit]] = None
  )(using F: Async[F]): Unit =
    val meta = ProviderMetadata(provider.getMetadata.getName)

    // Surfaces F failures to stderr rather than silently discarding them.
    def safeRunAndForget(fa: F[Unit]): Unit =
      dispatcher.unsafeRunAndForget(
        fa.handleErrorWith(e => F.delay(System.err.println(s"[openfeature4s] event handler error: ${e.getMessage}")))
      )

    def publishToTopic(event: ProviderEvent): F[Unit] =
      topic.publish1(Some(event)).flatMap {
        case Right(()) => F.unit
        case Left(_)   => F.raiseError(new IllegalStateException(s"[openfeature4s] event topic closed: $event"))
      }

    def publish(event: ProviderEvent, newStatus: ProviderStatus): Unit =
      safeRunAndForget(statusRef.set(newStatus) *> publishToTopic(event))

    client.on(
      JavaProviderEvent.PROVIDER_READY,
      (_: EventDetails) =>
        // guarantee ensures onReady is completed even if the topic publish fails,
        // so setStatus(Ready) callers are never permanently suspended.
        safeRunAndForget(
          (statusRef.set(ProviderStatus.Ready) *> publishToTopic(ProviderEvent.Ready(meta)))
            .guarantee(onReady.fold(F.unit)(_.complete(()).void))
        )
    )
    client.on(
      JavaProviderEvent.PROVIDER_ERROR,
      (d: EventDetails) =>
        val errCode = Option(d.getErrorCode).map(ErrorCodeConverter.fromJava)
        publish(
          ProviderEvent.Error(new RuntimeException(d.getMessage), meta, errCode, Option(d.getMessage)),
          ProviderStatus.Error
        )
    )
    client.on(
      JavaProviderEvent.PROVIDER_STALE,
      (d: EventDetails) => publish(ProviderEvent.Stale(Option(d.getMessage).getOrElse(""), meta), ProviderStatus.Stale)
    )
    client.on(
      JavaProviderEvent.PROVIDER_CONFIGURATION_CHANGED,
      (d: EventDetails) =>
        val changed = Option(d.getFlagsChanged).fold(Set.empty[String])(_.asScala.toSet)
        publish(ProviderEvent.ConfigurationChanged(changed, meta), ProviderStatus.Ready)
    )
