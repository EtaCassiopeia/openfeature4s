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
      _  <- provider.setFlag(FlagKey("a"), "hello")
      _  <- provider.setFlag(FlagKey("b"), "world")
      _  <- provider.removeFlag(FlagKey("a"))
      ra <- flags.string(FlagKey("a"), "default")
      rb <- flags.string(FlagKey("b"), "default")
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

  fixture.test("setFlags replaces all flags atomically") { case (provider, flags) =>
    for
      _    <- provider.setFlag(FlagKey("a"), "original")
      _    <- provider.setFlag(FlagKey("b"), "keep")
      _    <- provider.setFlags(Map(FlagKey("b") -> "updated", FlagKey("c") -> "new"))
      a    <- flags.string(FlagKey("a"), "default")
      b    <- flags.string(FlagKey("b"), "default")
      c    <- flags.string(FlagKey("c"), "default")
    yield
      assertEquals(a, "default") // removed by setFlags
      assertEquals(b, "updated")
      assertEquals(c, "new")
  }

  fixture.test("setDelay adds latency without breaking evaluation") { case (provider, flags) =>
    for
      _ <- provider.setFlag(FlagKey("k"), true)
      _ <- provider.setDelay(Duration.ofMillis(50))
      r <- flags.boolean(FlagKey("k"), false)
      _ <- provider.clearDelay
    yield assertEquals(r, true)
  }

  fixture.test("clearDelay removes the delay") { case (provider, flags) =>
    for
      _ <- provider.setFlag(FlagKey("k"), true)
      _ <- provider.setDelay(Duration.ofMillis(50))
      _ <- provider.clearDelay
      r <- flags.boolean(FlagKey("k"), false)
    yield assertEquals(r, true)
  }

  fixture.test("double flag evaluates correctly") { case (provider, flags) =>
    for
      _ <- provider.setFlag(FlagKey("d"), 3.14)
      r <- flags.double(FlagKey("d"), 0.0)
    yield assertEqualsDouble(r, 3.14, 0.001)
  }

  fixture.test("long flag evaluates correctly") { case (provider, flags) =>
    val big = Int.MaxValue.toLong + 1L
    for
      _ <- provider.setFlag(FlagKey("l"), big)
      r <- flags.long(FlagKey("l"), 0L)
    yield assertEquals(r, big)
  }

  fixture.test("ErrorMode.FlagNotFound produces FlagNotFound error code") { case (provider, flags) =>
    for
      _ <- provider.setErrorMode(TestFeatureProvider.ErrorMode.FlagNotFound)
      r <- flags.booleanDetails(FlagKey("k"), false)
      _ <- provider.clearErrorMode
    yield
      assert(r.isError)
      assertEquals(r.errorCode, Some(ErrorCode.FlagNotFound))
  }

  fixture.test("ErrorMode.ParseError produces ParseError error code") { case (provider, flags) =>
    for
      _ <- provider.setErrorMode(TestFeatureProvider.ErrorMode.ParseError)
      r <- flags.booleanDetails(FlagKey("k"), false)
      _ <- provider.clearErrorMode
    yield
      assert(r.isError)
      assertEquals(r.errorCode, Some(ErrorCode.ParseError))
  }

  fixture.test("ErrorMode.TypeMismatch produces TypeMismatch error code") { case (provider, flags) =>
    for
      _ <- provider.setErrorMode(TestFeatureProvider.ErrorMode.TypeMismatch)
      r <- flags.booleanDetails(FlagKey("k"), false)
      _ <- provider.clearErrorMode
    yield
      assert(r.isError)
      assertEquals(r.errorCode, Some(ErrorCode.TypeMismatch))
  }

  fixture.test("ErrorMode.ProviderNotReady produces ProviderNotReady error code") { case (provider, flags) =>
    for
      _ <- provider.setErrorMode(TestFeatureProvider.ErrorMode.ProviderNotReady)
      r <- flags.booleanDetails(FlagKey("k"), false)
      _ <- provider.clearErrorMode
    yield
      assert(r.isError)
      assertEquals(r.errorCode, Some(ErrorCode.ProviderNotReady))
  }

  fixture.test("evaluations are tracked even when ErrorMode is active") { case (provider, flags) =>
    for
      _ <- provider.setErrorMode(TestFeatureProvider.ErrorMode.General)
      _ <- flags.boolean(FlagKey("k"), false)
      n <- provider.evaluationCount(FlagKey("k"))
      _ <- provider.clearErrorMode
    yield assertEquals(n, 1)
  }
