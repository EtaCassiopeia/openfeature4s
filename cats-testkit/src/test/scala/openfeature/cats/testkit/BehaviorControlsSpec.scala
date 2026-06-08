package openfeature.cats.testkit

import openfeature.model.*
import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite

import java.time.Duration

class BehaviorControlsSpec extends CatsEffectSuite:

  private val fixture = ResourceFunFixture(TestFeatureProvider.make[IO]())

  fixture.test("setFlag makes a flag available for evaluation") { case (provider, flags) =>
    for
      _   <- provider.setFlag(FlagKey("k"), 42)
      res <- flags.int(FlagKey("k"), 0)
    yield assert(res == 42)
  }

  fixture.test("clearFlags removes all flags") { case (provider, flags) =>
    for
      _   <- provider.setFlag(FlagKey("k"), true)
      _   <- provider.clearFlags
      res <- flags.boolean(FlagKey("k"), false)
    yield assert(res == false) // returns default
  }

  fixture.test("removeFlag removes a specific flag") { case (provider, flags) =>
    for
      _   <- provider.setFlag(FlagKey("a"), "hello")
      _   <- provider.setFlag(FlagKey("b"), "world")
      _   <- provider.removeFlag(FlagKey("a"))
      ra  <- flags.string(FlagKey("a"), "default")
      rb  <- flags.string(FlagKey("b"), "default")
    yield
      assert(ra == "default")
      assert(rb == "world")
  }

  fixture.test("setErrorMode causes evaluation to return default with error reason") { case (provider, flags) =>
    for
      _   <- provider.setErrorMode(TestFeatureProvider.ErrorMode.General)
      res <- flags.booleanDetails(FlagKey("k"), false)
      _   <- provider.clearErrorMode
    yield assert(res.isError)
  }

  fixture.test("clearErrorMode restores normal evaluation") { case (provider, flags) =>
    for
      _   <- provider.setFlag(FlagKey("k"), true)
      _   <- provider.setErrorMode(TestFeatureProvider.ErrorMode.General)
      _   <- provider.clearErrorMode
      res <- flags.boolean(FlagKey("k"), false)
    yield assert(res == true)
  }

  fixture.test("setFailureProbability(0.0) never fails") { case (provider, flags) =>
    for
      _ <- provider.setFlag(FlagKey("k"), true)
      _ <- provider.setFailureProbability(0.0)
      // Run 10 evaluations — none should fail
      results <- (1 to 10).toList.traverse(_ => flags.boolean(FlagKey("k"), false))
      _       <- provider.setFailureProbability(0.0)
    yield assert(results.forall(_ == true))
  }

  fixture.test("setFailureProbability(1.0) always fails") { case (provider, flags) =>
    for
      _ <- provider.setFlag(FlagKey("k"), true)
      _ <- provider.setFailureProbability(1.0)
      // Error should surface as default value with error resolution
      res <- flags.booleanDetails(FlagKey("k"), false)
      _   <- provider.setFailureProbability(0.0)
    yield assert(res.isError)
  }

  fixture.test("clearBehavior resets all controls") { case (provider, flags) =>
    for
      _ <- provider.setFlag(FlagKey("k"), true)
      _ <- provider.setErrorMode(TestFeatureProvider.ErrorMode.General)
      _ <- provider.clearBehavior
      r <- flags.boolean(FlagKey("k"), false)
    yield assert(r == true)
  }
