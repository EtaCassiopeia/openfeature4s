package openfeature.cats.testkit

import openfeature.model.*
import cats.effect.IO
import munit.CatsEffectSuite

class EvaluationTrackingSpec extends CatsEffectSuite:

  private val fixture = ResourceFunFixture(
    TestFeatureProvider.make[IO](Map(FlagKey("flag") -> true))
  )

  fixture.test("wasEvaluated returns false before any evaluation") { case (provider, _) =>
    provider.wasEvaluated(FlagKey("flag")).map(v => assert(!v))
  }

  fixture.test("wasEvaluated returns true after evaluation") { case (provider, flags) =>
    for
      _   <- flags.boolean(FlagKey("flag"), false)
      res <- provider.wasEvaluated(FlagKey("flag"))
    yield assert(res)
  }

  fixture.test("wasEvaluated returns false for un-evaluated flag") { case (provider, flags) =>
    for
      _ <- flags.boolean(FlagKey("flag"), false)
      r <- provider.wasEvaluated(FlagKey("other"))
    yield assert(!r)
  }

  fixture.test("evaluationCount reflects number of evaluations") { case (provider, flags) =>
    for
      _ <- flags.boolean(FlagKey("flag"), false)
      _ <- flags.boolean(FlagKey("flag"), false)
      _ <- flags.boolean(FlagKey("flag"), false)
      n <- provider.evaluationCount(FlagKey("flag"))
    yield assert(n == 3)
  }

  fixture.test("clearEvaluations resets tracking") { case (provider, flags) =>
    for
      _ <- flags.boolean(FlagKey("flag"), false)
      _ <- provider.clearEvaluations
      n <- provider.evaluationCount(FlagKey("flag"))
    yield assert(n == 0)
  }

  fixture.test("multiple flag evaluations are all tracked") { case (provider, flags) =>
    for
      _  <- flags.boolean(FlagKey("flag"), false)
      _  <- flags.string(FlagKey("other"), "")
      r1 <- provider.wasEvaluated(FlagKey("flag"))
      r2 <- provider.wasEvaluated(FlagKey("other"))
    yield
      assert(r1)
      assert(r2)
  }
