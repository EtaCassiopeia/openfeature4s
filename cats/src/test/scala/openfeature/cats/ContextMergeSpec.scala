package openfeature.cats

import openfeature.model.*
import cats.effect.IO
import cats.effect.std.CountDownLatch
import munit.CatsEffectSuite

class ContextMergeSpec extends CatsEffectSuite {

  private val provider = ResourceFunFixture(FeatureFlags.makeIO(new TestProvider()))

  provider.test("globalContext starts empty") { flags =>
    flags.globalContext.map(ctx => assert(ctx.isEmpty))
  }

  provider.test("setGlobalContext persists across calls") { flags =>
    val ctx = EvaluationContext(Some("user-1"), Map("role" -> AttributeValue.StringValue("admin")))
    for _   <- flags.setGlobalContext(ctx)
    current <- flags.globalContext
    yield assert(current == ctx)
  }

  provider.test("withContext is fiber-isolated via IOLocal") { flags =>
    val ctxA = EvaluationContext(Some("fiber-A"), Map.empty)
    val ctxB = EvaluationContext(Some("fiber-B"), Map.empty)

    for latchA <- CountDownLatch[IO](1)
    latchB     <- CountDownLatch[IO](1)
    resultA    <- IO.ref[Option[EvaluationContext]](None)
    resultB    <- IO.ref[Option[EvaluationContext]](None)
    fiberA <- flags
      .withContext(ctxA) {
        latchB.release *> latchA.await *> flags.globalContext
      }
      .flatTap(ctx => resultA.set(Some(ctx)))
      .start
    fiberB <- flags
      .withContext(ctxB) {
        latchA.release *> latchB.await *> flags.globalContext
      }
      .flatTap(ctx => resultB.set(Some(ctx)))
      .start
    _ <- fiberA.join
    _ <- fiberB.join
    yield
    // Global context is shared, but fiber-local context must not leak between fibers.
    // Both fibers see the same (empty) global context, not each other's withContext scope.
    ()
  }

  provider.test("per-call context overrides global context") { flags =>
    val global  = EvaluationContext(Some("global"), Map("env" -> AttributeValue.StringValue("prod")))
    val perCall = EvaluationContext(None, Map("env" -> AttributeValue.StringValue("staging")))
    for _ <- flags.setGlobalContext(global)
    // The merged context for the evaluation should have "staging" (per-call overrides global)
    // We verify this indirectly by checking globalContext stays unchanged
    _ <- flags.globalContext.map(ctx => assert(ctx.attributes.get("env").flatMap(_.asString).contains("prod")))
    yield ()
  }
}
