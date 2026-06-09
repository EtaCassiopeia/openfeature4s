package openfeature.cats.testkit

import openfeature.model.*
import cats.effect.IO
import munit.CatsEffectSuite

class FactoryMethodsSpec extends CatsEffectSuite:

  private val ready = ResourceFunFixture(TestFeatureProvider.make[IO]())

  ready.test("make produces a Ready provider") { case (provider, _) =>
    provider.getStatus.map(s => assert(s == ProviderStatus.Ready))
  }

  ready.test("make produces a working FeatureFlags[F]") { case (provider, flags) =>
    for
      _   <- provider.setFlag(FlagKey("x"), true)
      res <- flags.boolean(FlagKey("x"), false)
    yield assert(res == true)
  }

  ready.test("make with initial flags makes them available immediately") { _ =>
    val fixture = ResourceFunFixture(
      TestFeatureProvider.make[IO](Map(FlagKey("k") -> "hello"))
    )
    // Inner test: resolve the fixture inline
    TestFeatureProvider.make[IO](Map(FlagKey("k") -> "hello")).use { case (_, flags) =>
      flags.string(FlagKey("k"), "").map(v => assert(v == "hello"))
    }
  }

  ready.test("make returns default for unset flag") { case (_, flags) =>
    flags.boolean(FlagKey("missing"), false).map(v => assert(v == false))
  }

  private val async = ResourceFunFixture(TestFeatureProvider.makeAsync[IO]())

  async.test("makeAsync produces a NotReady provider initially") { case (provider, _) =>
    provider.getStatus.map(s => assert(s == ProviderStatus.NotReady))
  }

  async.test("makeAsync provider transitions to Ready via setStatus") { case (provider, _) =>
    for
      before <- provider.getStatus
      _      <- provider.setStatus(ProviderStatus.Ready)
      after  <- provider.getStatus
    yield
      assert(before == ProviderStatus.NotReady)
      assert(after == ProviderStatus.Ready)
  }
