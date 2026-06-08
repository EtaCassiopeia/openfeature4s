package openfeature.cats

import openfeature.model.*
import cats.effect.{IO, Ref}
import cats.Applicative
import munit.CatsEffectSuite

class HookSpec extends CatsEffectSuite:

  private val provider = ResourceFunFixture(
    FeatureFlags.makeIO(new TestProvider(Map("flag" -> true)))
  )

  private def recordingHook(log: Ref[IO, List[String]]): FeatureHook[IO] = new FeatureHook[IO]:
    override def before(ctx: HookContext, hints: HookHints)(using
      F: Applicative[IO]
    ): IO[Option[(EvaluationContext, HookHints)]] =
      log.update(_ :+ "before").as(None)
    override def after[A](ctx: HookContext, details: FlagResolution[A], hints: HookHints)(using
      F: Applicative[IO]
    ): IO[Unit] =
      log.update(_ :+ "after")
    override def finallyAfter(ctx: HookContext, details: Option[FlagResolution[?]], hints: HookHints)(using
      F: Applicative[IO]
    ): IO[Unit] =
      log.update(_ :+ "finally")

  // Note: hooks are registered but the current implementation does not yet run the full
  // hook pipeline through FeatureFlagsLive (that wiring is tracked separately).
  // These tests verify addHook / addHooks compile and run without error.

  provider.test("addHook accepts a hook without error") { flags =>
    val hook = FeatureHook.noop[IO]
    flags.addHook(hook)
  }

  provider.test("addHooks accepts multiple hooks without error") { flags =>
    val hooks = List(FeatureHook.noop[IO], FeatureHook.noop[IO])
    flags.addHooks(hooks)
  }

  provider.test("FeatureHook.noop before returns None") { _ =>
    val hook = FeatureHook.noop[IO]
    val ctx = HookContext(
      "k",
      FlagValueType.Boolean,
      true,
      EvaluationContext.empty,
      ClientMetadata.default,
      ProviderMetadata("test")
    )
    hook.before(ctx, HookHints.empty).map(r => assert(r.isEmpty))
  }

  provider.test("FeatureHook.noop after completes without error") { _ =>
    val hook = FeatureHook.noop[IO]
    val ctx = HookContext(
      "k",
      FlagValueType.Boolean,
      true,
      EvaluationContext.empty,
      ClientMetadata.default,
      ProviderMetadata("test")
    )
    val details = FlagResolution[Boolean](true, None, ResolutionReason.Default, FlagMetadata.empty, "k")
    hook.after(ctx, details, HookHints.empty)
  }

  provider.test("custom hook before can modify context") { _ =>
    val hook = new FeatureHook[IO]:
      override def before(ctx: HookContext, hints: HookHints)(using
        F: Applicative[IO]
      ): IO[Option[(EvaluationContext, HookHints)]] =
        val newCtx = EvaluationContext(Some("modified"), Map.empty)
        F.pure(Some((newCtx, hints)))
    val ctx = HookContext(
      "k",
      FlagValueType.Boolean,
      false,
      EvaluationContext.empty,
      ClientMetadata.default,
      ProviderMetadata("test")
    )
    hook
      .before(ctx, HookHints.empty)
      .map:
        case Some((newCtx, _)) => assert(newCtx.targetingKey.contains("modified"))
        case None              => fail("expected Some with modified context")
  }

  provider.test("hook supportedFlagTypes defaults to all types") { _ =>
    val hook = FeatureHook.noop[IO]
    assert(hook.supportedFlagTypes == FlagValueType.allTypes)
  }

  provider.test("hook with restricted supportedFlagTypes compiles") { _ =>
    val hook = new FeatureHook[IO]:
      override def supportedFlagTypes: Set[FlagValueType] = Set(FlagValueType.Boolean)
    assert(hook.supportedFlagTypes == Set(FlagValueType.Boolean))
  }
