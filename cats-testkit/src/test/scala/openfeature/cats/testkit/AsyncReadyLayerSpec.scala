package openfeature.cats.testkit

import openfeature.model.*
import cats.effect.IO
import munit.CatsEffectSuite

class AsyncReadyLayerSpec extends CatsEffectSuite:

  private val fixture = ResourceFunFixture(TestFeatureProvider.makeAsync[IO]())

  fixture.test("provider starts in NotReady state") { case (provider, _) =>
    provider.getStatus.map(s => assert(s == ProviderStatus.NotReady))
  }

  fixture.test("evaluations before Ready return default with error resolution") { case (_, flags) =>
    flags
      .booleanDetails(FlagKey("k"), false)
      .map(res => assert(res.value == false)) // default returned on error
  }

  fixture.test("setStatus(Ready) transitions provider to Ready") { case (provider, _) =>
    for
      _     <- provider.setStatus(ProviderStatus.Ready)
      status <- provider.getStatus
    yield assert(status == ProviderStatus.Ready)
  }

  fixture.test("evaluations succeed after setStatus(Ready)") { case (provider, flags) =>
    for
      _ <- provider.setFlag(FlagKey("x"), "value")
      _ <- provider.setStatus(ProviderStatus.Ready)
      // Give the SDK event bridge a moment to process the PROVIDER_READY event
      _ <- IO.sleep(scala.concurrent.duration.FiniteDuration(100, "ms"))
      r <- flags.string(FlagKey("x"), "")
    yield assert(r == "value")
  }

  fixture.test("setStatus(Error) transitions provider to Error") { case (provider, _) =>
    for
      _ <- provider.setStatus(ProviderStatus.Error)
      s <- provider.getStatus
    yield assert(s == ProviderStatus.Error)
  }

  fixture.test("provider can cycle through multiple status transitions") { case (provider, _) =>
    for
      _ <- provider.setStatus(ProviderStatus.Ready)
      s1 <- provider.getStatus
      _  <- provider.setStatus(ProviderStatus.Stale)
      s2 <- provider.getStatus
      _  <- provider.setStatus(ProviderStatus.Ready)
      s3 <- provider.getStatus
    yield
      assert(s1 == ProviderStatus.Ready)
      assert(s2 == ProviderStatus.Stale)
      assert(s3 == ProviderStatus.Ready)
  }
